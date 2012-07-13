/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.backend.java.observing;

import java.io.PrintStream;
import java.util.List;

import abs.backend.java.lib.types.ABSValue;
import abs.backend.java.utils.StringUtil;

/**
 * A simple observer that prints observed events to the command line
 * 
 * @author Jan Schäfer
 *
 */
public class ConsoleObserver extends RegistratingObserver implements FutObserver, TaskObserver {

    PrintStream s = System.out;
    
    private void show(String string) {
        s.println("[Event] "+string);
    }

    @Override
    public void systemStarted() {
        show("System started");
    }

    @Override
    public void newCOGCreated(COGView cog, ObjectView o) {
        super.newCOGCreated(cog, o);
        show("COG["+cog.getID()+"] created");
        show(o.getClassName()+"["+o.getID()+"] created in COG["+cog.getID()+"]");
    }
    
    @Override
    public void objectCreated(ObjectView o) {
        super.objectCreated(o);
        show(objectString(o)+" created in COG["+o.getCOG().getID()+"]");
    }
    
    @Override
    public void methodCalled(ObjectView object, String method, List<ABSValue> args) {
        show("Method called: "+method+"("+StringUtil.iterableToString(args, ",")+")");
    }
    
    String objectString(ObjectView o) {
        return o.getClassName()+"["+o.getID()+"]";
    }
    
    @Override
    public void taskCreated(TaskView task) {
        show("Task["+task.getID()+"] created ("
                +objectString(task.getTarget())+
                "!"+task.getMethodName()+
                "("+StringUtil.iterableToString(task.getArgs(), ",")+
                ")"+
                ")");

        task.registerTaskListener(this);
        task.getFuture().registerFutObserver(this);
    }
    
    @Override
    public void systemFinished() {
        show("System finished");
    }

    @Override
    public void onResolved(FutView fut, ABSValue value) {
        show("Future["+fut.getID()+"] resolved with value '"+value+"'");
    }

    @Override
    public void taskStarted(TaskView task) {
        show("Task["+task.getID()+"] started");
    }

    @Override
    public void taskFinished(TaskView task) {
        show("Task["+task.getID()+"] finished");
    }

    @Override
    public void taskBlockedOnFuture(TaskView task, FutView fut) {
        show("Task["+task.getID()+"] blocked on Future["+fut.getID()+"]");
    }

    @Override
    public void taskRunningAfterWaiting(TaskView task, FutView fut) {
        show("Task["+task.getID()+"] continued running");
    }

    @Override
    public void taskStep(TaskView task, String fileName, int line) {
        // TODO Auto-generated method stub
        
    }

    @Override
    public void taskDeadlocked(TaskView task) {
        show("Task["+task.getID()+"] deadlocked");
    }

    @Override
    public void stackFrameCreated(TaskView task, TaskStackFrameView stackFrame) {
        show("Task["+task.getID()+"] called "+stackFrame.getMethod().getClassView().getName()+"."+stackFrame.getMethod().getName());
    }

    @Override
    public void localVariableChanged(TaskStackFrameView stackFrame, String name, ABSValue v) {
        // TODO Auto-generated method stub
        
    }

    @Override
    public void stackFrameRemoved(TaskView task, TaskStackFrameView oldFrame) {
    }
    
    

}
