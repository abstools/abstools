/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved.
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.abs_models.backend.java.lib.runtime;

import java.util.ArrayList;
import java.util.List;

import org.abs_models.backend.java.lib.runtime.TaskStack.Frame;
import org.abs_models.backend.java.lib.types.ABSRef;
import org.abs_models.backend.java.lib.types.ABSValue;
import org.abs_models.backend.java.observing.COGView;
import org.abs_models.backend.java.observing.ClassView;
import org.abs_models.backend.java.observing.FutView;
import org.abs_models.backend.java.observing.ObjectView;
import org.abs_models.backend.java.observing.TaskObserver;
import org.abs_models.backend.java.observing.TaskStackView;
import org.abs_models.backend.java.observing.TaskView;

public class Task<T extends ABSRef> implements Runnable {
    private final ABSFut<? super ABSValue> future;
    private final int id;
    private Thread executingThread;
    private ABSException exception;

    private final TaskStack stack;
    private final AsyncCall<T> call;

    /**
     * Real time attributes
     */
    private final long arrival;
    private final long cost;
    private final long deadline;
    private final boolean critical;
    private final int value;
    private long start;
    private long finish;


    public Task(AsyncCall<T> call) {
        this.call = call;
        future = new ABSTaskFut(this);
        ABSRuntime runtime = ((ABSObject)call.getTarget()).__ABS_getRuntime();
        id = runtime.freshTaskID();
        if (runtime.debuggingEnabled()) {
            stack = new TaskStack(this);
        } else {
            stack = null;
        }

        this.arrival = System.currentTimeMillis();
        if (call instanceof AbstractAsyncCallRT) {
            AbstractAsyncCallRT<?> callRT = (AbstractAsyncCallRT<?>)call;
            this.cost = callRT.getCost();
            this.deadline = callRT.getDeadline();
            this.critical = callRT.isCritical();
        } else {
            this.cost = -1;
            this.deadline = -1;
            this.critical = false;
        }
        this.start = -1;         // TODO set to time when task is first scheduled
        this.finish = 0;        // TODO
        this.value = 0;         // TODO
    }

    public int getID() {
        return id;
    }

    public long getArrival() {
        return arrival;
    }

    public long getCost() {
        return cost;
    }

    public long getDeadline() {
        return deadline;
    }

    public long getStart() {
        return start;
    }

    public void setStart(long ms) {
        start = ms;
    }

    public long getFinish() {
        return finish;
    }

    public boolean isCritical() {
        return critical;
    }

    public int getValue() {
        return value;
    }

    public synchronized void setLocalVariable(String name, ABSValue v) {
        if (stack != null) {
            Frame f = stack.getCurrentFrame();
            f.setValue(name,v);
            if (view != null) {
                view.localVariableChanged(f,name,v);
            }
        }
    }

    public synchronized boolean isDeadlocked() {
        return exception != null && exception.isDeadlock();
    }

    public synchronized boolean hasException() {
        return exception != null;
    }

    public synchronized ABSException getException() {
        return exception;
    }

    // not synchronized as only called when task is locked
    public void setException(ABSException exception) {
        this.exception = exception;
    }

    public COG getCOG() {
        return ((ABSObject)call.getTarget()).getCOG();
    }

    public void schedule() {
        getCOG().addTask(this);
    }

    public ABSFut<?> getFut() {
        return future;
    }

    // only for observing
    void calledGetOnFut(ABSFut<?> someFut) {
        View v = view;
        if (v != null)
            v.calledGetOnFut(someFut);
    }

    // only for observing
    void futureReady(ABSFut<?> someFut) {
        View v = view;
        if (v != null)
            v.futureReady(someFut);
    }

    public void popStackFrame() {
        if (stack != null) {
            Frame oldFrame = stack.popFrame();
            if (view != null) {
                view.stackFrameRemoved(oldFrame);
            }
        }
    }

    public void newStackFrame(ABSObject target, String methodName) {
        if (stack != null) {

            ClassView cv = null;
            if (target != null)
                cv = target.getView().getClassView();

            Frame f = stack.pushNewFrame(new ABSMethod(cv,methodName));
            if (view != null) {
                view.newStackFrameCreated(f);
            }
        }
    }

    public void run() {
        synchronized (this) {
            if (view != null)
                view.taskStarted();
            executingThread = Thread.currentThread();
        }


        try {
            ABSValue res = (ABSValue) call.call();
            future.resolve(res);
        } catch (ABSException e) {
            this.exception = e;
            future.smash(e);
            ABSRuntime.getRuntime().handleABSException(this,e);
        } catch (SystemTerminatedException e) {

        } catch (Exception e) {
            e.printStackTrace();
            ABSException absException = new ABSAssertException("Sorry, this is a bug in the Java backend of ABS: " +
                        "Unexpected exception: " + e);
            // TODO new subclass for internal error
            absException.setStackTrace(e.getStackTrace());
            ABSRuntime.getRuntime().handleABSException(this, absException );
        }

        synchronized (this) {
            if (view != null)
                view.taskFinished();
        }

    }

    public String toString() {
        return "Task (" + id + ") [" + getCOG() + ", Method: " + call.getTarget().getClass().getSimpleName() + "." + call.methodName()
                + "]";
    }

    private volatile View view;

    private Object viewCreationLock = new Object();
    private boolean finished = false;

    public TaskView getView() {
        synchronized(viewCreationLock) {
        if (view == null) {
            view = new View();
        }
        return view;
        }
    }

    public synchronized Thread getExecutingThread() {
        return executingThread;
    }


    public AsyncCall<?> getCall() {
        return call;
    }


    public synchronized void nextStep(String fileName, int line) {
        if (view != null)
            view.nextStep(fileName, line);
    }

    private class View implements TaskView {
        private List<TaskObserver> taskListener;

        @Override
        public TaskView getSender() {
            if (call.getSender() == null)
                return null;
            return call.getSender().getView();
        }

        public void stackFrameRemoved(Frame oldFrame) {
            for (TaskObserver l : getObservers()) {
                l.stackFrameRemoved(this, oldFrame);
            }
        }

        public synchronized void localVariableChanged(Frame f,String name, ABSValue v) {
            for (TaskObserver l : getObservers()) {
                l.localVariableChanged(f,name, v);
            }
        }

        public synchronized void newStackFrameCreated(Frame f) {
            for (TaskObserver l : getObservers()) {
                l.stackFrameCreated(this,f);
            }
        }

        public synchronized void nextStep(String fileName, int line) {
            for (TaskObserver l : getObservers()) {
                l.taskStep(this, fileName, line);
            }
        }

        public synchronized void futureReady(ABSFut<?> someFut) {
            for (TaskObserver l : getObservers()) {
                l.taskRunningAfterWaiting(this, someFut.getView());
            }
        }

        public synchronized void calledGetOnFut(ABSFut<?> someFut) {
            for (TaskObserver l : getObservers()) {
                l.taskBlockedOnFuture(this, someFut.getView());
            }
        }

        @Override
        public ObjectView getSource() {
            if (call.getSource() == null)
                return null;
            return call.getSource().getView();
        }

        @Override
        public ObjectView getTarget() {
            return ((ABSObject)call.getTarget()).getView();
        }

        private synchronized List<TaskObserver> getObservers() {
            if (taskListener == null)
                taskListener = new ArrayList<>(1);
            return taskListener;
        }

        public synchronized void taskStarted() {
            for (TaskObserver l : getObservers()) {
                l.taskStarted(this);
            }
        }

        public synchronized void taskFinished() {
            for (TaskObserver l : getObservers()) {
                l.taskFinished(this);
            }
        }

        @Override
        public COGView getCOG() {
            return Task.this.getCOG().getView();
        }

        @Override
        public String getMethodName() {
            return Task.this.call.methodName();
        }

        @Override
        public List<ABSValue> getArgs() {
            return Task.this.call.getArgs();
        }

        @Override
        public FutView getFuture() {
            return future.getView();
        }

        @Override
        public int getID() {
            return id;
        }

        @Override
        public boolean isDeadlocked() {
            return Task.this.isDeadlocked();
        }

        @Override
        public synchronized void registerTaskListener(TaskObserver listener) {
            getObservers().add(listener);
        }

        @Override
        public boolean hasException() {
            return Task.this.hasException();
        }

        @Override
        public ABSException getException() {
            return Task.this.getException();
        }

        @Override
        public TaskStackView getStack() {
            return stack;
        }

    }

    public synchronized boolean isFinished() {
        return finished;
    }

    public synchronized void setFinished(boolean b) {
       finished = b;
    }

}
