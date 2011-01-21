package abs.backend.java.visualization;

import java.io.*;
import java.net.*;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.atomic.AtomicInteger;

import abs.backend.java.lib.types.ABSValue;
import abs.backend.java.observing.COGView;
import abs.backend.java.observing.EmptyTaskObserver;
import abs.backend.java.observing.FutView;
import abs.backend.java.observing.GuardView;
import abs.backend.java.observing.ObjectCreationObserver;
import abs.backend.java.observing.ObjectView;
import abs.backend.java.observing.SystemObserver;
import abs.backend.java.observing.TaskObserver;
import abs.backend.java.observing.TaskSchedulerObserver;
import abs.backend.java.observing.TaskStackFrameView;
import abs.backend.java.observing.TaskView;

public class SequenceDiagramVisualization implements SystemObserver, TaskObserver, ObjectCreationObserver, TaskSchedulerObserver {
    private static final int MAX_ARG_LENGTH = 30;

    Set<FutView> waitingFutures = new HashSet<FutView>();
    Set<FutView> resolvedFutures = new HashSet<FutView>();

    final Map<COGView, String> createdCOGClasses = new HashMap<COGView, String>();

    final Map<COGView, Integer> objectIds = new HashMap<COGView, Integer>();
    final Map<String, AtomicInteger> idCounters = new HashMap<String, AtomicInteger>();

    PrintWriter out;
    GUI gui;

    protected String getName() {
        return "Simulation";
    }

    protected boolean abstractEnvironment = false;
    boolean showStartMsg = true;
    boolean staticActors = false;
    boolean withGUI = false;

    boolean firstMessage = true;

    @Override
    public synchronized void newCOGCreated(COGView cog, ObjectView initialObject) {
        // out.println(initialObject.getClassName() + ":" +
        // initialObject.getClassName() + "[ap]");
        // System.out.println(initialObject.getClassName());
        String className = initialObject.getClassName();
        createdCOGClasses.put(cog,className);
        if (isObservedClass(className)) {
            cog.getScheduler().registerTaskSchedulerObserver(this);
            if (!staticActors) {
                if (!firstMessage) {
                    // special support for adding nodes later to a diagram
                    out.print("#newobj ");
                }
                out.println(getActorName(initialObject) + ":" + className + "[a]");
            }
        }
        
        cog.registerObjectCreationListener(this);

    }
    
    @Override
    public synchronized void objectCreated(ObjectView o) {
/*        if (!staticActors) {
            out.println(getActorName(o) + ":" + o.getClassName() + "[a]");
        }
*/        
        
    }

    @Override
    public synchronized void objectInitialized(ObjectView o) {
/*        if (!staticActors) {
            out.println(getActorName(o) + ":" + o.getClassName() + "[a]");
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
        try {
            long startms= System.currentTimeMillis();
            while (true) {
                try {
                    
                    Socket skt = new Socket("localhost", 60001);

                    out = new PrintWriter(skt.getOutputStream(), true);
                    out.println(getName());

                    initializeActors();

                    if (withGUI) {
                        gui = new GUI();
                    }
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

    protected void initializeActors() {
        if (showStartMsg)
            out.println("HiddenEnv:HiddenEnv[pe]");

        if (abstractEnvironment)
            out.println("ENVIRONMENT:ENVIRONMENT[ap]");
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

    TaskBlockingObserver TASK_BLOCKING_OBSERVER = new TaskBlockingObserver();

    class TaskBlockingObserver extends EmptyTaskObserver {
        @Override
        public void taskBlockedOnFuture(TaskView task, FutView fut) {
            synchronized (SequenceDiagramVisualization.this) {
                if (!isObserved(task))
                    return;
                waitingFutures.add(fut);
                String actorName = getActorName(task.getTarget());
                out.println("*" + fut.getID() + " " + actorName);
                out.println("future get");
                out.println("*" + fut.getID());
                out.println("(" + fut.getID() + ") " + actorName);
            }
        }

        @Override
        public void taskRunningAfterWaiting(TaskView task, FutView fut) {
            synchronized (SequenceDiagramVisualization.this) {
                if (!isObserved(task))
                    return;

                if (resolvedFutures.contains(fut))
                    return;
                resolvedFutures.add(fut);

                TaskView futTask = fut.getResolvingTask();
                String sourceClass = futTask.getTarget().getClassName();
                out.print(getActorName(futTask.getTarget()));
                if (isSystemClass(sourceClass)) {
                    if (futTask.getID() != 1) // no main task
                        out.print("[" + "Task" + futTask.getID() + "]");
                    out.print(":>");
                } else {
                    out.print(":");
                }
                String futTaskName = getActorName(task.getTarget()) + "[" + "FutTask" + futTask.getID() + "]";
                out.print(futTaskName);
                out.println(".future resolved\\:" + shorten(fut.getValue().toString()));
                out.println(futTaskName + ":stop");
                out.println("(" + fut.getID() + ") " + getActorName(task.getTarget()));
                out.println(getActorName(futTask.getTarget())+ "[Task" + futTask.getID() + "]:stop");
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
                out.println();
                if (showStartMsg)
                    out.println("HiddenEnv:Main_1[Task1].start()");
                firstMessage = false;
            }

            if (withGUI)
                gui.waitForClick();

            if (isObservedClass(targetClass))
                task.registerTaskListener(TASK_BLOCKING_OBSERVER);

            out.print(source);
            out.print(msgAction);

            /*
             * if (systemClasses.contains(task.getSource().getClassName())) {
             * out.print(":>"); }
             */
            out.print(getActorName(task.getTarget()));
            if (isSystemClass(targetClass)) {
                if (task.getID() != 1) // no main task
                    out.print("[" + "Task" + task.getID() + "]");
            }
            out.print(".");
            out.print(task.getMethodName());
            out.print("(");
            StringBuffer argString = new StringBuffer();
            boolean first = true;
            for (ABSValue v : task.getArgs()) {
                if (first)
                    first = false;
                else
                    argString.append(", ");
                argString.append(""+v);
            }

            out.print(shorten(argString.toString()));
            out.println(")");
            
        }
    }

    private String escapeColons(String s) {
        return s.replaceAll("\\:", "\\\\:");
    }

    private String getClassName(ObjectView obj) {
        return createdCOGClasses.get(obj.getCOG());        
    }

    private String shorten(String arg) {
        String escapedArg = escapeColons(arg);
        if (escapedArg.length() > MAX_ARG_LENGTH) {
            StringBuffer sb = new StringBuffer(escapedArg);
            int halfLength = MAX_ARG_LENGTH / 2;
            String rest = arg.substring(escapedArg.length() - halfLength);
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
        System.out.println("Task Finished: "+task.getID());
        
        if (!isObserved(task))
            return;
        // out.println("Future " + task.getFuture().getID() + " resolved\\: " +
        // task.getFuture().getValue());
        if (!waitingFutures.contains(task.getFuture())) {
            String taskName = getActorName(task.getTarget());
            if (task.getID() != 1) // no main task
                taskName += "[" + "Task" + task.getID() + "]";
            out.println(taskName + ":"); // do something to avoid empty tasks
            out.println(taskName + ":stop");
            resolvedFutures.add(task.getFuture());
        }
    }

    @Override
    public synchronized void taskStarted(TaskView task) {
        if (!isObserved(task))
            return;
        String target = task.getTarget().getClassName();
        // out.println(target+"[" + "Task" + task.getID() + "]:<started>"); //
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
                out.println("*" + fut.getID() + " " + actorName);
                // out.print("[" + "Task" + task.getID() + "]");
                // out.print(":");
                if (guard.isTrue()) {
                    out.print("yielding");
                } else {
                    out.print("waiting");
                }
                // out.print(" on future "+fut.getID());
                out.println();
                out.println("*" + fut.getID());
                out.println("(" + fut.getID() + ") " + actorName);
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

                if (resolvedFutures.contains(fut))
                    return;
                resolvedFutures.add(fut);

                out.print(getActorName(resolvingTask.getTarget()));
                out.print("[" + "Task" + resolvingTask.getID() + "]");
                out.print(":>");
                out.print(getActorName(task.getTarget()));
                out.print("[" + "Taskx" + resolvingTask.getID() + "]");
                out.print(".resolved\\: ");
                out.print(shorten(fut.getValue().toString()));
                out.println();
                out.print(getActorName(resolvingTask.getTarget()));
                out.print("[" + "Task" + resolvingTask.getID() + "]");
                out.print(":stop");
                out.println();
                out.print(getActorName(task.getTarget()));
                out.print("[" + "Taskx" + resolvingTask.getID() + "]");
                out.print(":stop");
                out.println();
                out.println("(" + fut.getID() + ") " + getActorName(task.getTarget()));
            }
        }
    }

    @Override
    public void taskBlockedOnFuture(TaskView task, FutView fut) {
        // never invoked
    }

    public static final List<String> EMPTY_STRING_LIST = new ArrayList<String>(0);

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

}
