/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.absmodels.abs.plugin.debug.scheduling;

import static org.absmodels.abs.plugin.debug.DebugUtils.*;
import static org.absmodels.abs.plugin.util.UtilityFunctions.showErrorMessage;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.List;
import java.util.Random;

import org.absmodels.abs.plugin.debug.DebugUtils;
import org.absmodels.abs.plugin.debug.perspective.SchedulerChoiceDelegate;
import org.absmodels.abs.plugin.debug.views.debugview.DebugView;
import org.eclipse.jface.action.IMenuCreator;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.swt.widgets.Display;

import abs.backend.java.debugging.TaskState;
import abs.backend.java.observing.TaskStackFrameView;
import abs.backend.java.observing.TaskStackView;
import abs.backend.java.observing.TaskView;
import abs.backend.java.scheduling.ActivateTask;
import abs.backend.java.scheduling.ScheduleAction;
import abs.backend.java.scheduling.ScheduleOptions;
import abs.backend.java.scheduling.UsesRandomSeed;
import abs.backend.java.scheduling.SimpleTaskScheduler.TaskInfo;
import abs.backend.java.scheduling.TaskScheduler;
import abs.backend.java.scheduling.TotalSchedulingStrategy;

public class SchedulingStrategy implements TotalSchedulingStrategy, UsesRandomSeed {
	private boolean doDebug = true;
	
    private List<ScheduleAction> scheduleActions;
	private History history;

	boolean waitForUserInput = true;
    List<TaskView> schedulableTasks;
	TotalScheduler curScheduler;
	TotalScheduler baseScheduler;
	
	abs.backend.java.debugging.TaskInfo steppedTask = null;

    private Random random = null;
	
	public SchedulingStrategy() {
		schedulerRef = this;
		history = new History();
		
		curScheduler = new GUIScheduler(this);
		baseScheduler = new RunTaskScheduler(this);
	}
	
	/**
	 * Sets the current scheduler. If the old scheduler is the GUIScheduler, it is interrupted
	 * and notified about the changed scheduler.
	 * @param scheduler
	 */
	public synchronized void setCurrentScheduler(TotalScheduler scheduler){
	    if (random != null && scheduler instanceof UsesRandomSeed) {
	        ((UsesRandomSeed) scheduler).setRandom(random);
	    }
		TotalSchedulingStrategy oldscheduler = curScheduler;
		curScheduler = scheduler;
		schedulerUpdated(oldscheduler);
	}
	
	/**
	 * Sets the base scheduler. If the new base scheduler is the TaskScheduler the selected task
	 * is set on the scheduler
	 * @param scheduler
	 */
	public synchronized void setBaseScheduler(TotalScheduler scheduler){
	    if (random != null && scheduler instanceof UsesRandomSeed) {
            ((UsesRandomSeed) scheduler).setRandom(random);
        }
		this.baseScheduler = scheduler;
		if(baseScheduler instanceof RunTaskScheduler){
			RunTaskScheduler ts = (RunTaskScheduler) baseScheduler;
			Object o = getDebugViewerSelection();
			if(o instanceof TaskView)
				ts.setTask((TaskView) o);
		}
	}
	
	/**
	 * resumes the scheduling with the current base scheduler (Interrupts the
	 * GUIScheduler if it is the current scheduler)
	 */
	public synchronized void resumeAutomaticScheduling(){
		TotalSchedulingStrategy oldScheduler = curScheduler;
		baseScheduler.reset();
		curScheduler = baseScheduler;
		disableHighlighting();
		schedulerUpdated(oldScheduler);
	}

	private void schedulerUpdated(TotalSchedulingStrategy oldScheduler) {
		if(oldScheduler instanceof GUIScheduler){
			GUIScheduler gs = (GUIScheduler) oldScheduler;
			gs.interrupt();
			notifyAll();
		}
	}
	
	/**
	 * Gets called if the selection of the {@link DebugView} {@link TreeViewer} changes
	 * @param o the selection
	 */
	public void taskSelectionChanged(Object o){
		if(baseScheduler instanceof RunTaskScheduler){
			RunTaskScheduler ts = (RunTaskScheduler) baseScheduler;
			if(o instanceof TaskView){
				ts.setTask((TaskView) o);
			} else if(o instanceof TaskStackFrameView){
				ts.setTask(((TaskStackFrameView)o).getStack().getTask());
			}
		}
	}
	
	/**
	 * Has to be called when the selection of the current scheduler in the scheduler menu
	 * has changed. Updates the current base scheduler accordingly.
	 */
	public void updateScheduler(){
	    if (DebugUtils.schedulerMenu != null && schedulerMenu.getMenuCreator() instanceof SchedulerChoiceDelegate) {
	        // update selection in UI
	        SchedulerChoiceDelegate menu = (SchedulerChoiceDelegate) schedulerMenu.getMenuCreator();
	        menu.setSelection(DebugUtils.getScheduler());
	    }
	    
		switch(DebugUtils.getScheduler()){
		case interactive:
			setBaseScheduler(new RunTaskScheduler(this));
			refreshButtonEnablement();
			break;
		case random:
			setBaseScheduler(new RandomScheduler(new Random()));
			refreshButtonEnablement();
			break;
		case replay:
		    setBaseScheduler(new HistoryReplayScheduler(this));
		    refreshButtonEnablement();
			break;
		default:
			throw new RuntimeException("Non exhaustive match in scheduler enum" +
					" while updating scheduler in SchedulingStrategy!");
		}
	}
	
	/**
	 * Has to be called when the RunTime shuts down
	 */
	public synchronized void systemFinished(){
		scheduleActions = null;
		schedulableTasks = null;
	}

	public synchronized List<TaskView> getSchedulableTasks(){
		return schedulableTasks;
	}
	
	@Override
	public ScheduleAction choose(ScheduleOptions options) {
		synchronized (this) {
			scheduleActions = options.allOptions();

			schedulableTasks = new ArrayList<TaskView>();
			for (ScheduleAction a : scheduleActions) {
				if(a.getTask() != null){
					schedulableTasks.add(a.getTask());
				} else {
					schedulableTasks.addAll(a.getCOG().getView().getScheduler().getSchedulableTasks());
				}
			}
		}
		
		if(highlightStep){
			highlightNextStep();
		}
		ScheduleAction a = curScheduler.choose(options);
		// is null when the system was terminated
		if (a != null) {
			logAction(a);
			history.chosenAction(a);
		}
		return a;
		
	}

	/**
	 * Updates the graphical display after a step. Refreshes the buttons enablement and
	 * selects the next stepable task in the {@link DebugView}
	 */
	public void highlightNextStep() {
		Display.getDefault().asyncExec(new Runnable() {
			@Override
			public void run() {
				refreshButtonEnablement();
				if(baseScheduler instanceof RunTaskScheduler && steppedTaskIsSchedulable()){
					selectNextTask();
				} else if(baseScheduler instanceof RandomScheduler){
				    if (steppedTask != null) {
				        setDebugTreeSelection(steppedTask.getTaskView());
				    }
				}
				refreshVariableView();
				refreshDebugViewer();
			}
		});
	}
	
	private boolean steppedTaskIsSchedulable() {
		return steppedTask != null && ((steppedTask.getState() == TaskState.FINISHED)
				|| (steppedTask.getState() == TaskState.SUSPENDED && !schedulableTasks.contains(steppedTask.getTaskView())));
	}
	
	public void setLastTask(abs.backend.java.debugging.TaskInfo lastTask){
		steppedTask = lastTask;
	}
	
	/**
	 * Gets called when the user selects to run to a specific line (in a file)
	 * @param file
	 * @param line
	 */
	public synchronized void doRunToLine(String file, int line){
		disableHighlighting();
		setCurrentScheduler(new RunToLineScheduler(this, file, line));
	}

	/**
	 * Gets called when the user selects to do multiple steps
	 * @param n the number of steps to perform
	 */
	public synchronized void doMultipleSteps(int n){
		disableHighlighting();
		baseScheduler.reset();
		setCurrentScheduler(new NStepScheduler(this, n));
	}
	
	/**
	 * Gets called when the user selects to do a single step
	 */
    public synchronized void doSingleStep() {
        waitForUserInput = false;
        notify();
    }
    
    /**
	 * Gets called when the user selects to do a step over
	 */
    public synchronized void doStepOver(){
    	if(steppedTask!=null){
    		TaskStackView stack = steppedTask.getTaskView().getStack();
    		if(stack.hasFrames()){
        		disableHighlighting();
    			setCurrentScheduler(new StepOverScheduler(this, stack.getCurrentFrame()));
    		} else {
    			doSingleStep();
    		}
    	} else {
    		doSingleStep();
    	}
    }
    
    public synchronized List<ScheduleAction> getScheduleActions(){
    	return scheduleActions;
    }

	@Override
	public TaskInfo schedule(final TaskScheduler scheduler, List<TaskInfo> scheduableTasks) {
		 TaskInfo task = curScheduler.schedule(scheduler, scheduableTasks);
		 logTaskInfo(task);
		 history.activatedTask(task);
		 return task;
	}   
	
	private void logAction(ScheduleAction a) {
		if(doDebug)
			System.out.println("chosen action: "+a);
	}
	
	private void logTaskInfo(TaskInfo t) {
		if(doDebug)
			System.out.println("chosen task: "+t.toString());
	}
	
	public void saveHistory(File file){
		history.saveHistory(file);
	}
	
	class History{
		List<ScheduleAction> history;
		
		public History(){
			history = new ArrayList<ScheduleAction>();
		}
		
		private void saveHistory(File f){
			try {
	            PrintWriter p = new PrintWriter(new FileOutputStream(f));
	            for (ScheduleAction a : history) {
	                p.append(a.shortString());
	                p.append("\n");
	            }
	            p.close();
	        } catch (FileNotFoundException e) {
	        	showErrorMessage("Could not save history: " + e.getMessage());
	        }
		}
		
		public void activatedTask(TaskInfo task){
			history.add(new ActivateTask(task.task.getCOG(), task.task));
		}
		
		public void chosenAction(ScheduleAction a){
			history.add(a);
		}
	}
	
	/**
	 * Waits for user user decision on which action the scheduler should take next.
	 * @param options the current scheduling options coming from the debugger in the abs frontend
	 * @return a valid scheduling option
	 */
	ScheduleAction awaitGUIAction(ScheduleOptions options) {
		setCurrentScheduler(new GUIScheduler(this));
		enableHightlighting();
		return curScheduler.choose(options);
	}

    @Override
    public void setRandom(Random random) {
        this.random = random;
    }

}
