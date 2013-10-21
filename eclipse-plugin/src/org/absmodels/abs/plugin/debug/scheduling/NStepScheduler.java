/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.absmodels.abs.plugin.debug.scheduling;

import java.util.List;

import abs.backend.java.scheduling.ScheduleAction;
import abs.backend.java.scheduling.ScheduleOptions;
import abs.backend.java.scheduling.TaskScheduler;
import abs.backend.java.scheduling.SimpleTaskScheduler.TaskInfo;

/**
 * This scheduler does the given number of steps on the current base scheduler. After the given steps, 
 * the current scheduler is switched to the {@link GUIScheduler}.
 * 
 * @author mweber
 *
 */
public class NStepScheduler implements TotalScheduler{
	private final SchedulingStrategy schedulingStrategy;
	private int steps = 0;

	public NStepScheduler(SchedulingStrategy schedulingStrategy, int n){
		this.schedulingStrategy = schedulingStrategy;
		steps = n;
	}
	
	@Override
	public ScheduleAction choose(ScheduleOptions options) {
		if(steps>0){
			steps--;
			return this.schedulingStrategy.baseScheduler.choose(options);
		} else {
			return this.schedulingStrategy.awaitGUIAction(options);
		}
	}

	@Override
	public TaskInfo schedule(TaskScheduler scheduler,
			List<TaskInfo> scheduableTasks) {
		return this.schedulingStrategy.baseScheduler.schedule(scheduler, scheduableTasks);
	}

	@Override
	public void reset() {
	}
	
}