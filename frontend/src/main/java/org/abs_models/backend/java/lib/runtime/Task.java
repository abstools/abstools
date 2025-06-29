/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved.
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.abs_models.backend.java.lib.runtime;

import java.util.ArrayList;
import java.util.List;

import org.abs_models.backend.java.lib.runtime.TaskStack.Frame;
import org.abs_models.backend.java.lib.types.ABSRef;
import org.abs_models.backend.java.observing.COGView;
import org.abs_models.backend.java.observing.ClassView;
import org.abs_models.backend.java.observing.FutView;
import org.abs_models.backend.java.observing.ObjectView;
import org.abs_models.backend.java.observing.TaskObserver;
import org.abs_models.backend.java.observing.TaskStackView;
import org.abs_models.backend.java.observing.TaskView;

import org.apfloat.Aprational;

public class Task<T extends ABSRef> implements Runnable {
    private final ABSFut<Object> future;
    private final int id;
    private Thread executingThread;
    private ABSException exception;

    private final TaskStack stack;
    private final AsyncCall<T> call;

    // Real-time task attributes

    // Arrival time (set at runtime)
    private final long arrival;
    // Cost (taken from method definition annotation) (currently unused)
    private final Aprational cost;
    /**
     * The deadline, as an absolute clock value.  Note that we convert from
     * relative values (in ABS code) to absolute (during runtime).
     */
    private final Aprational deadline_t;
    private final boolean critical;
    private final int value;
    private long start;
    private long finish;


    public Task(AsyncCall<T> call) {
        this.call = call;
        future = new ABSTaskFut(this);
        ABSRuntime runtime = ABSRuntime.getRuntime();
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
            this.deadline_t = callRT.getDeadlineAbsolute();
            this.critical = callRT.isCritical();
        } else {
            this.cost = new Aprational(-1);
            this.deadline_t = new Aprational(-1);
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

    public Aprational getCost() {
        return cost;
    }

    public Aprational getDeadlineAbsolute() {
        return deadline_t;
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

    public synchronized void setLocalVariable(String name, Object v) {
        if (stack != null) {
            Frame f = stack.getCurrentFrameView();
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
            Object res = call.call();
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

    private View view;

    private boolean finished = false;

    public TaskView getView() {
        if (view == null) {
            synchronized(this) {
                if (view == null) {
                    view = new View();
                }
            }
        }
        return view;
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
        public TaskView getSenderView() {
            if (call.getSender() == null)
                return null;
            return call.getSender().getView();
        }

        public void stackFrameRemoved(Frame oldFrame) {
            for (TaskObserver l : getObservers()) {
                l.stackFrameRemoved(this, oldFrame);
            }
        }

        public synchronized void localVariableChanged(Frame f,String name, Object v) {
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
        public ObjectView getSourceObjectView() {
            if (call.getSource() == null)
                return null;
            return call.getSource().getView();
        }

        @Override
        public ObjectView getTargetObjectView() {
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
        public COGView getCOGView() {
            return Task.this.getCOG().getView();
        }

        @Override
        public String getMethodName() {
            return Task.this.call.methodName();
        }

        @Override
        public List<Object> getArgs() {
            return Task.this.call.getArgs();
        }

        @Override
        public FutView getFutView() {
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
        public TaskStackView getStackView() {
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
