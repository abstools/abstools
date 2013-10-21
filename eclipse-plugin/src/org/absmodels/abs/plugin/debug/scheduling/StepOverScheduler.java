/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.absmodels.abs.plugin.debug.scheduling;

import java.util.List;

import abs.backend.java.observing.TaskStackFrameView;
import abs.backend.java.observing.TaskStackView;
import abs.backend.java.scheduling.ScheduleAction;
import abs.backend.java.scheduling.ScheduleOptions;
import abs.backend.java.scheduling.TaskScheduler;
import abs.backend.java.scheduling.SimpleTaskScheduler.TaskInfo;

/**
 * This scheduler does actions on the current base scheduler until the stack frame of the last stepped
 * task equals the given stack frame. That means, the task is again in the same method.
 * 
 * @author mweber
 *
 */
public class StepOverScheduler implements TotalScheduler{
	private final SchedulingStrategy schedulingStrategy;
	private TaskStackFrameView frame;
	private boolean firstStep = true;
	
	public StepOverScheduler(SchedulingStrategy schedulingStrategy, TaskStackFrameView frame) {
		this.schedulingStrategy = schedulingStrategy;
		this.frame = frame;
	}

	@Override
	public ScheduleAction choose(ScheduleOptions options) {
		if(firstStep){
			firstStep = false;
			schedulingStrategy.baseScheduler.reset();
			return schedulingStrategy.baseScheduler.choose(options);
		} else {
			TaskStackView stack = schedulingStrategy.steppedTask.getTaskView().getStack();
			if(stack.hasFrames() && stack.getCurrentFrame().equals(frame)){
				return schedulingStrategy.awaitGUIAction(options);
			} else {
				schedulingStrategy.baseScheduler.reset();
				return schedulingStrategy.baseScheduler.choose(options);
			}
		}
	}

	@Override
	public TaskInfo schedule(TaskScheduler scheduler,
			List<TaskInfo> scheduableTasks) {
		return schedulingStrategy.baseScheduler.schedule(scheduler, scheduableTasks);
	}

	@Override
	public void reset() {
	}
}