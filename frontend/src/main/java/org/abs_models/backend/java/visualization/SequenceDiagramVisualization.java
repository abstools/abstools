/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved.
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.abs_models.backend.java.visualization;

import java.io.IOException;
import java.io.PrintWriter;
import java.net.Socket;
import java.net.UnknownHostException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ArrayBlockingQueue;
import java.util.concurrent.atomic.AtomicInteger;

import org.abs_models.backend.java.lib.runtime.ABSException;
import org.abs_models.backend.java.lib.types.ABSValue;
import org.abs_models.backend.java.observing.COGView;
import org.abs_models.backend.java.observing.DefaultTaskObserver;
import org.abs_models.backend.java.observing.FutView;
import org.abs_models.backend.java.observing.GuardView;
import org.abs_models.backend.java.observing.ObjectCreationObserver;
import org.abs_models.backend.java.observing.ObjectView;
import org.abs_models.backend.java.observing.SystemObserver;
import org.abs_models.backend.java.observing.TaskObserver;
import org.abs_models.backend.java.observing.TaskSchedulerObserver;
import org.abs_models.backend.java.observing.TaskStackFrameView;
import org.abs_models.backend.java.observing.TaskView;

public class SequenceDiagramVisualization implements SystemObserver, TaskObserver, ObjectCreationObserver, TaskSchedulerObserver {
    private static final int MAX_ARG_LENGTH = 30;

    Set<FutView> waitingFutures = new HashSet<>();
    Set<FutView> resolvedFutures = new HashSet<>();

    final Map<COGView, String> createdCOGClasses = new HashMap<>();

    final Map<COGView, Integer> objectIds = new HashMap<>();
    final Map<String, AtomicInteger> idCounters = new HashMap<>();

    protected String getName() {
        return "Simulation";
    }

    protected boolean abstractEnvironment = false;
    boolean showStartMsg = true;
    boolean staticActors = false;

    boolean firstMessage = true;

    private Worker worker;

    @Override
    public synchronized void newCOGCreated(COGView cog, ObjectView initialObject) {
        // writeOut(initialObject.getClassName() + ":" +
        // initialObject.getClassName() + "[ap]");
        // System.writeOut(initialObject.getClassName());
        String className = initialObject.getClassName();
        createdCOGClasses.put(cog,className);
        if (isObservedClass(className)) {
            cog.getScheduler().registerTaskSchedulerObserver(this);
            if (!staticActors) {
                if (!firstMessage) {
                    // special support for adding nodes later to a diagram
                    writeOut("#newobj ");
                }
                // use sdedit's <label> syntax to create a label that has the class name and the COG ID
                // the "a" flag suppresses the object's name (before the colon) - which is useless in this case, as the label is used
                writeOutLn(getActorName(initialObject) + ":" + className + "[a]" + "\"" + className + " [COG " + cog.getID() + "]\"");
            }
        }

        cog.registerObjectCreationListener(this);

    }

    @Override
    public synchronized void objectCreated(ObjectView o) {
/*        if (!staticActors) {
            writeOut(getActorName(o) + ":" + o.getClassName() + "[a]");
        }
*/

    }

    @Override
    public synchronized void objectInitialized(ObjectView o) {
/*        if (!staticActors) {
            writeOut(getActorName(o) + ":" + o.getClassName() + "[a]");
        }
*/

    }


    protected synchronized Integer getID(ObjectView v) {
        Integer id = objectIds.get(v.getCOG());
        if (id == null) {
            AtomicInteger counter = idCounters.get(v.getClassName());
            if (counter == null) {
                counter = new AtomicInteger();
                idCounters.put(v.getClassName(), counter);
            }
            id = counter.incrementAndGet();
            objectIds.put(v.getCOG(), id);
        }
        return id;
    }

    public boolean isObservedClass(String className) {
        return getObservedClasses().contains(className);
    }


    @Override
    public synchronized void systemStarted() {
        worker = new Worker();
        worker.start();
        writeOutLn(getName());
        initializeActors();
    }

    protected void writeOutLn(String s) {
        writeOut(s + "\n");
    }

    protected void writeOut(String s) {
        worker.write(s);
    }

    protected void writeOutLn() {
        writeOutLn("");
    }

    protected void initializeActors() {
        if (showStartMsg)
            writeOutLn("HiddenEnv:HiddenEnv[pe]");

        if (abstractEnvironment)
            writeOutLn("ENVIRONMENT:ENVIRONMENT[ap]");
    }

    public String getActorName(ObjectView v) {
        if (abstractEnvironment && getEnvironmentClasses().contains(getClassName(v))) {
            return "ENVIRONMENT";
        }

        Integer id = getID(v);
        return getClassName(v) + "_" + id.intValue();
    }

    public String getActorName(String className) {
        if (abstractEnvironment && getEnvironmentClasses().contains(className)) {
            return "ENVIRONMENT";
        }

        return className;
    }

    private final TaskBlockingObserver TASK_BLOCKING_OBSERVER = new TaskBlockingObserver();

    class TaskBlockingObserver extends DefaultTaskObserver {
        @Override
        public void taskBlockedOnFuture(TaskView task, FutView fut) {
            synchronized (SequenceDiagramVisualization.this) {
                if (!isObserved(task))
                    return;
                waitingFutures.add(fut);
                String actorName = getActorName(task.getTarget());
                writeOutLn("*" + fut.getID() + " " + actorName);
                writeOutLn("future get");
                writeOutLn("*" + fut.getID());
                writeOutLn("(" + fut.getID() + ") " + actorName);
            }
        }

        @Override
        public void taskRunningAfterWaiting(TaskView task, FutView fut) {
            synchronized (SequenceDiagramVisualization.this) {
                if (!isObserved(task))
                    return;

                if (!waitingFutures.contains(fut) || resolvedFutures.contains(fut))
                    return;
                resolvedFutures.add(fut);

                TaskView futTask = fut.getResolvingTask();
                String sourceClass = futTask.getTarget().getClassName();
                writeOut(getActorName(futTask.getTarget()));
                if (isSystemClass(sourceClass)) {
                    //if (futTask.getID() != 1) // no main task
                        writeOut("[" + "Task" + futTask.getID() + "]");
                    writeOut(":>");
                } else {
                    writeOut(":");
                }
                String futTaskName = getActorName(task.getTarget()) + "[" + "FutTask" + futTask.getID() + "]";
                writeOut(futTaskName);
                writeOutLn(".future resolved\\:" + shorten(String.valueOf(fut.getValue())));
                writeOutLn(futTaskName + ":stop");
                writeOutLn("(" + fut.getID() + ") " + getActorName(task.getTarget()));
                writeOutLn(getActorName(futTask.getTarget())+ "[Task" + futTask.getID() + "]:stop");
            }
        }

    }

    @Override
    public synchronized void taskCreated(TaskView task) {
        task.registerTaskListener(this);

        if (task.getSource() == null) {
            return;
        }

        String sourceClass = getClassName(task.getSource());
        String targetClass = getClassName(task.getTarget());

        if (isObservedClass(sourceClass) && isObservedClass(targetClass)) {

            String msgAction = ":>";

            if (isEnvironmentClass(sourceClass) || isEnvironmentClass(targetClass))
                msgAction = ":";

            String source = getActorName(task.getSource());
            if (isSystemClass(sourceClass))
                source = source + "[" + "Task" + task.getSender().getID() + "]";

            if (firstMessage) {
                writeOutLn();
                if (showStartMsg)
                    writeOutLn("HiddenEnv:Main_1[Task1].start()");
                firstMessage = false;
            }

            if (isObservedClass(targetClass))
                task.registerTaskListener(TASK_BLOCKING_OBSERVER);

            writeOut(source);
            writeOut(msgAction);

            /*
             * if (systemClasses.contains(task.getSource().getClassName())) {
             * writeOut(":>"); }
             */
            writeOut(getActorName(task.getTarget()));
            if (isSystemClass(targetClass)) {
                //if (task.getID() != 1) // no main task
                    writeOut("[" + "Task" + task.getID() + "]");
            }
            writeOut(".");
            writeOut(task.getMethodName());
            writeOut("(");
            StringBuffer argString = new StringBuffer();
            boolean first = true;
            for (ABSValue v : task.getArgs()) {
                if (first)
                    first = false;
                else
                    argString.append(", ");
                argString.append(""+v);
            }

            writeOut(shorten(String.valueOf(argString.toString())));
            writeOutLn(")");

        }
    }

    private String escapeColons(String s) {
        return s.replaceAll(":", "\\\\:");
    }

    private String getClassName(ObjectView obj) {
        return createdCOGClasses.get(obj.getCOG());
    }

    private String shorten(String arg) {
        String escapedArg = escapeColons(arg);
        if (escapedArg.length() > MAX_ARG_LENGTH) {
            StringBuffer sb = new StringBuffer(escapedArg);
            int halfLength = MAX_ARG_LENGTH / 2;
            String rest = escapedArg.substring(escapedArg.length() - halfLength);
            sb.setLength(halfLength);
            sb.append(" .. ");
            sb.append(rest);
            return sb.toString();
        }
        return escapedArg;
    }

    protected boolean isSystemClass(String source) {
        return getSystemClasses().contains(source);
    }

    @Override
    public synchronized void taskFinished(TaskView task) {
        if (!isObserved(task))
            return;
        // writeOut("Future " + task.getFuture().getID() + " resolved\\: " +
        // task.getFuture().getValue());
        if (!waitingFutures.contains(task.getFuture())) {
            String taskName = getActorName(task.getTarget());
            //if (task.getID() != 1) // no main task
                taskName += "[" + "Task" + task.getID() + "]";
            writeOutLn(taskName + ":"); // do something to avoid empty tasks
            writeOutLn(taskName + ":stop");
            resolvedFutures.add(task.getFuture());
        }
    }

    @Override
    public synchronized void taskStarted(TaskView task) {
        if (!isObserved(task))
            return;
        String target = task.getTarget().getClassName();
        // writeOut(target+"[" + "Task" + task.getID() + "]:<started>"); //
        // do something to avoid empty tasks

    }

    @Override
    public synchronized void taskSuspended(TaskView task, GuardView guard) {
        if (!isObserved(task))
            return;

        if (guard.isFutureGuard()) {
            FutView fut = guard.getFuture();
            if (isObserved(fut.getResolvingTask())) {
                waitingFutures.add(fut);
                String actorName = getActorName(task.getTarget());
                writeOutLn("*" + fut.getID() + " " + actorName);
                // writeOut("[" + "Task" + task.getID() + "]");
                // writeOut(":");
                if (guard.isTrue()) {
                    writeOut("yielding");
                } else {
                    writeOut("waiting");
                }
                // writeOut(" on future "+fut.getID());
                writeOutLn();
                writeOutLn("*" + fut.getID());
                writeOutLn("(" + fut.getID() + ") " + actorName);
            }
        }
    }

    public boolean isObserved(TaskView task) {
        String source = task.getSource().getClassName();
        String target = task.getTarget().getClassName();
        return isObservedClass(source) && isObservedClass(target)
                && isSystemClass(target);
    }

    @Override
    public synchronized void taskRunningAfterWaiting(TaskView task, FutView fut) {
    }

    @Override
    public synchronized void taskResumed(TaskView task, GuardView guard) {
        if (!isObserved(task))
            return;
        if (guard.isFutureGuard()) {
            FutView fut = guard.getFuture();

            TaskView resolvingTask = fut.getResolvingTask();
            if (isObserved(resolvingTask)) {

                if (!waitingFutures.contains(fut) || resolvedFutures.contains(fut))
                    return;
                resolvedFutures.add(fut);

                writeOut(getActorName(resolvingTask.getTarget()));
                writeOut("[" + "Task" + resolvingTask.getID() + "]");
                writeOut(":>");
                writeOut(getActorName(task.getTarget()));
                writeOut("[" + "Taskx" + resolvingTask.getID() + "]");
                writeOut(".resolved\\: ");
                writeOut(shorten(String.valueOf(fut.getValue())));
                writeOutLn();
                writeOut(getActorName(resolvingTask.getTarget()));
                writeOut("[" + "Task" + resolvingTask.getID() + "]");
                writeOut(":stop");
                writeOutLn();
                writeOut(getActorName(task.getTarget()));
                writeOut("[" + "Taskx" + resolvingTask.getID() + "]");
                writeOut(":stop");
                writeOutLn();
                writeOutLn("(" + fut.getID() + ") " + getActorName(task.getTarget()));
            }
        }
    }

    @Override
    public void taskBlockedOnFuture(TaskView task, FutView fut) {
        // never invoked
    }

    public static final List<String> EMPTY_STRING_LIST =
        Collections.unmodifiableList(new ArrayList<>(0));

    Collection<String> getObservedClasses() {
        return createdCOGClasses.values();
    }

    List<String> getSystemClasses() {
        return EMPTY_STRING_LIST;
    }

    List<String> getEnvironmentClasses() {
        return EMPTY_STRING_LIST;
    }

    boolean isEnvironmentClass(String s) {
        if (s.equals("ENVIRONMENT"))
            return true;
        return getEnvironmentClasses().contains(s);
    }

    @Override
    public void taskStep(TaskView task, String fileName, int line) {
        // TODO Auto-generated method stub

    }

    @Override
    public void taskReady(TaskView view) {
        // TODO Auto-generated method stub

    }

    @Override
    public void taskDeadlocked(TaskView task) {
        // TODO Auto-generated method stub

    }

    @Override
    public void stackFrameCreated(TaskView task, TaskStackFrameView stackFrame) {
        // TODO Auto-generated method stub

    }

    @Override
    public void localVariableChanged(TaskStackFrameView stackFrame, String name, ABSValue v) {
        // TODO Auto-generated method stub

    }

    @Override
    public void systemFinished() {
        worker.stopMe();
    }

    private static class Worker extends Thread {
        private PrintWriter out;
        volatile private boolean stopped = false;

        private final ArrayBlockingQueue<String> buffer = new ArrayBlockingQueue<>(10000000, true);
        private Worker() {
            super("SDEdit Communication Thread");
            super.setDaemon(true);
        }

        @Override public void run() {
            connect();
            while(!stopped || !buffer.isEmpty()) {
                 try {
                     String msg = buffer.take();
                     out.print(msg);
                     out.flush();
                 } catch (InterruptedException e) {
                     // terminate after buffer is empty
                 }
             }
        }

        void stopMe() {
            stopped = true;
            interrupt();
        }

        private void connect() {
            try {
                long startms= System.currentTimeMillis();
                while (true) {
                    try {
                        Socket sk = new Socket("localhost", 60001);
                        out = new PrintWriter(sk.getOutputStream());


                        return;
                    } catch (java.net.SocketException e) {
                        try {
                            System.out.println("Waiting for sdedit server on localhost:60001 ... "+((System.currentTimeMillis()-startms) / 1000)+"s");
                            Thread.sleep(500);
                        } catch (InterruptedException e2) {
                            e2.printStackTrace();
                        }
                    }
                }
            } catch (UnknownHostException e) {
                e.printStackTrace();
            } catch (IOException e) {
                e.printStackTrace();
            }
        }

        public void write(String s) {
            boolean f = false;
            while(!f) {
                f = buffer.offer(s);
            }
        }
    }

    @Override
    public void systemError(ABSException e) {
        // do nothing
    }

    @Override
    public void stackFrameRemoved(TaskView task, TaskStackFrameView oldFrame) {
        // TODO Auto-generated method stub

    }
}
