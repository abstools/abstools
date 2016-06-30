/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.absmodels.abs.plugin.debug.scheduling;

import java.util.List;

import abs.backend.java.observing.TaskView;
import abs.backend.java.scheduling.ScheduleAction;
import abs.backend.java.scheduling.ScheduleOptions;
import abs.backend.java.scheduling.ScheduleTask;
import abs.backend.java.scheduling.TaskScheduler;
import abs.backend.java.scheduling.SimpleTaskScheduler.TaskInfo;

/**
 * This is a base scheduler. When asked, it gives the next step of the given task as long as there are actions
 * for the task left. When no steps are left, the scheduler switches the current scheduler to the
 * {@link GUIScheduler}.
 * 
 * @author mweber
 *
 */
public class RunTaskScheduler implements TotalScheduler{
	private final SchedulingStrategy schedulingStrategy;
	private TaskView task;
	private boolean doSchedule = true;

	public RunTaskScheduler(SchedulingStrategy schedulingStrategy) {
		this.schedulingStrategy = schedulingStrategy;
	}
	
	public void setTask(TaskView task){
		this.task = task;
	}
	
	@Override
	public ScheduleAction choose(ScheduleOptions options) {
		List<ScheduleAction> schedulableActions = options.allOptions();
		boolean isEqual = false;
		if(doSchedule && this.schedulingStrategy.schedulableTasks.contains(task)){
			for (ScheduleAction action : schedulableActions) {
				if(action instanceof ScheduleTask){
					isEqual = (action.getCOG().getID() == task.getCOG().getID());
				} else{
					isEqual = (action.getTask().getID() == task.getID());
				}
				doSchedule = false;
				if (isEqual) {
					return action;
				}
			}
		} else {
			for (ScheduleAction action : schedulableActions) {
				if(!(action instanceof ScheduleTask) && (action.getTask().getID() == task.getID())){
					return action;
				}
			}
		}
		return this.schedulingStrategy.awaitGUIAction(options);
	}

	@Override
	public TaskInfo schedule(TaskScheduler scheduler,
			List<TaskInfo> scheduableTasks) {
		for(TaskInfo taskInfo : scheduableTasks){
			if(taskInfo.task.getID() == task.getID()){
				return taskInfo;
			}
		}
		throw new RuntimeException("Trying to schedule non-schedulable task!");
	}

	@Override
	public void reset() {
		doSchedule = true;
	}
	
}