/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.backend.java;
import java.io.File;
import java.util.List;
import java.util.concurrent.atomic.AtomicInteger;

import abs.backend.java.lib.runtime.ABSException;
import abs.backend.java.lib.runtime.ABSRuntime;
import abs.backend.java.lib.types.ABSValue;
import abs.backend.java.observing.COGView;
import abs.backend.java.observing.FutView;
import abs.backend.java.observing.GuardView;
import abs.backend.java.observing.ObjectCreationObserver;
import abs.backend.java.observing.ObjectView;
import abs.backend.java.observing.SystemObserver;
import abs.backend.java.observing.TaskObserver;
import abs.backend.java.observing.TaskSchedulerObserver;
import abs.backend.java.observing.TaskStackFrameView;
import abs.backend.java.observing.TaskView;
import abs.backend.java.scheduling.ScheduleAction;
import abs.backend.java.scheduling.ScheduleOptions;
import abs.backend.java.scheduling.SimpleTaskScheduler.TaskInfo;
import abs.backend.java.scheduling.TaskScheduler;
import abs.backend.java.scheduling.TotalSchedulingStrategy;


public class RuntimeUsageTest implements SystemObserver, ObjectCreationObserver, TaskSchedulerObserver, TaskObserver {

    String name;
    
    RuntimeUsageTest(String n) {
        name = n;
    }
    
    class TestScheduler implements TotalSchedulingStrategy {

        private ABSRuntime runtime;
        private final AtomicInteger counter = new AtomicInteger();

        public TestScheduler(ABSRuntime r) {
            this.runtime = r;
        }

        @Override
        public ScheduleAction choose(ScheduleOptions options) {
            int i = counter.incrementAndGet();
            if (i == 5) {
                System.out.println("Terminating system "+name);
                runtime.shutdown();
            }
            return options.allOptions().get(0);
        }

        @Override
        public TaskInfo schedule(TaskScheduler scheduler, List<TaskInfo> scheduableTasks) {
            return scheduableTasks.get(0);
        }
        
    }
    
    /**
     * @param args
     */
    public static void main(String[] args) {
        try {
            for (int i = 0; i < 2; i++) {
                RuntimeUsageTest t = new RuntimeUsageTest("System "+i);
                ABSRuntime r = new ABSRuntime();
                r.addSystemObserver(t);
                r.setTotalSchedulingStrategy(t.getScheduler(r));
                r.enableDebugging(true);
                r.start(new File("javatest"), "LeaderElection.Main");
            }
        } catch (ClassNotFoundException e) {
            e.printStackTrace();
        } catch (InstantiationException e) {
            e.printStackTrace();
        } catch (IllegalAccessException e) {
            e.printStackTrace();
        }
    }

    private TotalSchedulingStrategy getScheduler(ABSRuntime r) {
        return new TestScheduler(r);
    }

    @Override
    public void systemStarted() {
        System.out.println(name+" started");
    }

    @Override
    public void newCOGCreated(COGView cog, ObjectView initialObject) {
        System.out.println(name+": created cog "+cog.getID());
        objectCreated(initialObject);
        cog.registerObjectCreationListener(this);
        cog.getScheduler().registerTaskSchedulerObserver(this);
    }

    @Override
    public void objectCreated(ObjectView o) {
        System.out.println(name+": created object "+o.getID());
    }

    @Override
    public void objectInitialized(ObjectView o) {
        // TODO Auto-generated method stub
        
    }

    @Override
    public void taskCreated(TaskView task) {
        System.out.println(name+": created task "+task.getID());
        task.registerTaskListener(this);
    }

    @Override
    public void taskReady(TaskView view) {
        // TODO Auto-generated method stub
        
    }

    @Override
    public void taskResumed(TaskView runningTask, GuardView view) {
        // TODO Auto-generated method stub
        
    }

    @Override
    public void taskSuspended(TaskView task, GuardView guard) {
        // TODO Auto-generated method stub
        
    }

    @Override
    public void taskStarted(TaskView task) {
        // TODO Auto-generated method stub
        
    }

    @Override
    public void taskFinished(TaskView task) {
        // TODO Auto-generated method stub
        
    }

    @Override
    public void taskBlockedOnFuture(TaskView task, FutView fut) {
        // TODO Auto-generated method stub
        
    }

    @Override
    public void taskRunningAfterWaiting(TaskView view, FutView fut) {
        // TODO Auto-generated method stub
        
    }

    @Override
    public void taskStep(TaskView task, String fileName, int line) {
        // TODO Auto-generated method stub
        
    }

    @Override
    public void taskDeadlocked(TaskView task) {
        // TODO Auto-generated method stub
        
    }

    @Override
    public void stackFrameCreated(TaskView task, TaskStackFrameView stackFrame) {
        System.out.println(name+": new stack frame created");
    }

    @Override
    public void localVariableChanged(TaskStackFrameView stackFrame, String n, ABSValue v) {
        System.out.println(name+":task "+stackFrame.getStack().getTask().getID()+": local variable "+n+" = "+v);
        
    }

    @Override
    public void systemFinished() {
        System.out.println(name+" system finished");
    }

    @Override
    public void systemError(ABSException e) {
        // TODO Auto-generated method stub
        
    }

    @Override
    public void stackFrameRemoved(TaskView task, TaskStackFrameView oldFrame) {
        // TODO Auto-generated method stub
        
    }

}
