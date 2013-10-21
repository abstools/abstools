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
 * Takes a file (path as string like in the module declarations) and a line and does actions on the current
 * base scheduler until the line is reached. When the line is reached the current scheduler is switched
 * to the {@link GUIScheduler}.
 * 
 * @author mweber
 *
 */
public class RunToLineScheduler implements TotalScheduler {
	private final SchedulingStrategy schedulingStrategy;
	private int lineNumber;
	private String fileName;
	
	public RunToLineScheduler(SchedulingStrategy schedulingStrategy, String fileName, int lineNumber) {
		this.schedulingStrategy = schedulingStrategy;
		this.fileName = fileName;
		this.lineNumber = lineNumber;
	}
	
	@Override
	public ScheduleAction choose(ScheduleOptions options) {
		if(schedulingStrategy.steppedTask==null ||
				(schedulingStrategy.steppedTask.getCurrentLine() != lineNumber) || 
				!schedulingStrategy.steppedTask.getCurrentFile().equals(fileName)){
			schedulingStrategy.baseScheduler.reset();
			return schedulingStrategy.baseScheduler.choose(options);
		} else {
			return schedulingStrategy.awaitGUIAction(options);
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