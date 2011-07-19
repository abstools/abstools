/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.backend.java.lib.runtime;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.concurrent.atomic.AtomicInteger;

import abs.backend.java.lib.runtime.TaskStack.Frame;
import abs.backend.java.lib.types.ABSRef;
import abs.backend.java.lib.types.ABSValue;
import abs.backend.java.observing.COGView;
import abs.backend.java.observing.ClassView;
import abs.backend.java.observing.FutView;
import abs.backend.java.observing.MethodView;
import abs.backend.java.observing.ObjectView;
import abs.backend.java.observing.TaskObserver;
import abs.backend.java.observing.TaskStackView;
import abs.backend.java.observing.TaskView;

public class Task<T extends ABSRef> {
    private final ABSFut<? super ABSValue> future;
    private final int id;
    private Thread executingThread;
    private ABSException exception;
    private final TaskStack stack;
    private final AsyncCall<T> call;

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
    }

    public int getID() {
        return id;
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
            ABSValue res = (ABSValue) call.execute();
            future.resolve(res);
        } catch (ABSException e) {
            this.exception = e;
            future.smash(e);
            getCOG().getRuntime().handleABSException(this,e);
        } catch (SystemTerminatedException e) {
            
        } catch (Exception e) {
            e.printStackTrace();
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

    public synchronized TaskView getView() {
        if (view == null) {
            view = new View();
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
        public TaskView getSender() {
            if (call.getSender() == null)
                return null;
            return call.getSender().getView();
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
                taskListener = new ArrayList<TaskObserver>(1);
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

}
