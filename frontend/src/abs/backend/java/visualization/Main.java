package abs.backend.java.visualization;

import java.io.*;
import java.net.*;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import abs.backend.java.lib.types.ABSValue;
import abs.backend.java.observing.COGView;
import abs.backend.java.observing.EmptyTaskObserver;
import abs.backend.java.observing.FutView;
import abs.backend.java.observing.GuardView;
import abs.backend.java.observing.ObjectView;
import abs.backend.java.observing.SystemObserver;
import abs.backend.java.observing.TaskObserver;
import abs.backend.java.observing.TaskView;

public class Main implements SystemObserver, TaskObserver {
    private static final int MAX_ARG_LENGTH = 30;
    
    List<FutView> waitingFutures = new ArrayList<FutView>();
    
	final List<String> createdCOGClasses = new ArrayList<String>();
	
    
    PrintWriter out;
    GUI gui;
    
    protected String getName() {
    	return "Simulation";
    }
    
    protected boolean abstractEnvironment = false;
    boolean showStartMsg = true;
    boolean staticActors = false;
    
    boolean firstMessage = true;
    
    @Override
    public synchronized void newCOGCreated(COGView cog, ObjectView initialObject) {
        //out.println(initialObject.getClassName() + ":" + initialObject.getClassName() + "[ap]");
        //System.out.println(initialObject.getClassName());
    	String className = initialObject.getClassName();
		createdCOGClasses.add(className);
        if (isObservedClass(className)) {
            cog.getScheduler().registerTaskObserver(this);
            if (!staticActors) {
           		out.println(className+":"+className+"[a]");
            }
        }
        
    }

    public boolean isObservedClass(String className) {
		return getObservedClasses().contains(className);
	}

	@Override
    public synchronized void systemStarted() {
        try {
            Socket skt = new Socket("localhost", 60001);
            out = new PrintWriter(skt.getOutputStream(), true);
            out.println(getName());
            
            initializeActors();
            
            gui = new GUI();
            
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

	public String getActorName(String className) {
    	if (abstractEnvironment &&
    		getEnvironmentClasses().contains(className)) {
    			return "ENVIRONMENT";
    	} 
    	
    	return className;
    }
    
    TaskBlockingObserver TASK_BLOCKING_OBSERVER = new TaskBlockingObserver();
    class TaskBlockingObserver extends EmptyTaskObserver {
    	@Override
    	public void taskBlockedOnFuture(TaskView task, FutView fut) {
    		synchronized (Main.this) {
    			if (!isObserved(task))
    				return;
    			out.println("*"+fut.getID()+" "+task.getTarget().getClassName());
    			out.println("future get");
    			out.println("*"+fut.getID());
    			out.println("("+fut.getID()+") "+task.getTarget().getClassName());
    		}
    	}

    	@Override
    	public void taskRunningAfterWaiting(TaskView task, FutView fut) {
    		synchronized (Main.this) {
    			if (!isObserved(task))
    				return;
    			
    			TaskView futTask = fut.getResolvingTask();
    			String sourceName = futTask.getTarget().getClassName();
    			out.print(getActorName(sourceName));
    			if (getSystemClasses().contains(sourceName)) {
    				if (futTask.getID() != 1) // no main task
    					out.print("[" + "Task" + futTask.getID() + "]");
        			out.print(":>");
    			} else {
        			out.print(":");
    			}
    			String futTaskName = task.getTarget().getClassName()+"[" + "FutTask"+futTask.getID()+"]"; 
    			out.print(futTaskName);
    			out.println(".future resolved\\:"+fut.getValue());
    			out.println(futTaskName+":stop");
    			out.println("("+fut.getID()+") "+task.getTarget().getClassName());
    		}
    	}

    	
    	
    }
    
    @Override
    public synchronized void taskCreated(TaskView task) {
    	
    	if (task.getSource() == null) {
    			return;	
    	} 
    	String sourceClass = task.getSource().getClassName();
      String targetClass = task.getTarget().getClassName();
    	
        if (isObservedClass(sourceClass) && isObservedClass(targetClass)) {

            String msgAction = ":>";
            
         	if (isEnvironmentClass(sourceClass) || isEnvironmentClass(targetClass))
       			msgAction = ":";

            String source = getActorName(sourceClass);
         	if (isSystemClass(sourceClass)) 
            	source = source+"[" + "Task" + task.getSender().getID() + "]";

         if (firstMessage) {
           	out.println();
           	if (showStartMsg)
           		out.println("HiddenEnv:Main[Task1].start()");
           	firstMessage = false;
           }

           gui.waitForClick();
            
            if (isObservedClass(targetClass))
            	task.registerTaskListener(TASK_BLOCKING_OBSERVER);
            
            
            out.print(source);
            out.print(msgAction);

            /*if (systemClasses.contains(task.getSource().getClassName())) {
                out.print(":>");
            }*/
            out.print(getActorName(targetClass));
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
                if (first) first = false;
                else argString.append(", ");
                argString.append(v.toString().replaceAll("\\:", "\\\\:"));
            }
            if (argString.length() > MAX_ARG_LENGTH) {
            	int halfLength = MAX_ARG_LENGTH / 2;
            	String rest = argString.substring(argString.length()-halfLength);
            	argString.setLength(halfLength);
            	argString.append(" .. ");
            	argString.append(rest);
            }
            out.print(argString.toString());
            out.println(")");
        }
    }

	protected boolean isSystemClass(String source) {
		return getSystemClasses().contains(source);
	}

	@Override
    public synchronized void taskFinished(TaskView task) {
    	if (!isObserved(task))
    		return;
        String target = task.getTarget().getClassName();
        //out.println("Future " + task.getFuture().getID() + " resolved\\: " + task.getFuture().getValue());
        if (!waitingFutures.contains(task.getFuture())) {
        	String taskName = target;
			if (task.getID() != 1) // no main task
				taskName += "[" + "Task" + task.getID() + "]";
            out.println(taskName+":"); // do something to avoid empty tasks
            out.println(taskName+":stop");
        }
    }

    @Override
    public synchronized void taskStarted(TaskView task) {
    	if (!isObserved(task))
    		return;
        String target = task.getTarget().getClassName();
        //out.println(target+"[" + "Task" + task.getID() + "]:<started>"); // do something to avoid empty tasks
        
    }

    @Override
    public synchronized void taskSuspended(TaskView task, GuardView guard) {
    	if (!isObserved(task))
    		return;
    	
    	if (guard.isFutureGuard()) {
    		FutView fut = guard.getFuture();
    		if (isObserved(fut.getResolvingTask())) {
    		    waitingFutures.add(fut);
    		    out.println("*" + fut.getID() + " " + task.getTarget().getClassName());
    	        //out.print("[" + "Task" + task.getID() + "]");
    	        //out.print(":");
    	        if (guard.isTrue()) {
    	            out.print("yielding");
    	        } else {
    	            out.print("waiting");
    	        }
    		    //out.print(" on future "+fut.getID());
    		    out.println();
    		    out.println("*" + fut.getID());
    		    out.println("(" + fut.getID() + ") " + task.getTarget().getClassName());
    		}
    	}
    }

	public boolean isObserved(TaskView task) {
        String source = task.getSource().getClassName();
        String target = task.getTarget().getClassName();
        return getObservedClasses().contains(source) 
        	&& getObservedClasses().contains(target) 
        	&& getSystemClasses().contains(target);
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
		        out.print(resolvingTask.getTarget().getClassName());
		        out.print("[" + "Task" + resolvingTask.getID() + "]");
		        out.print(":>");
		        out.print(task.getTarget().getClassName());
                out.print("[" + "Taskx" + resolvingTask.getID() + "]");
		        out.print(".resolved\\: ");		        
		        out.print(fut.getValue());
		        out.println();
		        out.print(resolvingTask.getTarget().getClassName());
		        out.print("[" + "Task" + resolvingTask.getID() + "]");
                out.print(":stop");
                out.println();
		        out.print(task.getTarget().getClassName());
                out.print("[" + "Taskx" + resolvingTask.getID() + "]");
                out.print(":stop");
                out.println();
                out.println("(" + fut.getID() + ") " + task.getTarget().getClassName());
		    }
		}
	}

	@Override
	public void taskBlockedOnFuture(TaskView task, FutView fut) {
		// never invoked
	}

	public static final List<String> EMPTY_STRING_LIST = new ArrayList<String>(0);
	List<String> getObservedClasses() {
		return createdCOGClasses;
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
}
