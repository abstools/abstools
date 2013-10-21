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
 * This scheduler waits for user input. When step button is pressed the scheduler asks the underlying 
 * base scheduler for its next step and returns it. Complex action schedulers use this scheduler
 * to signal the user that no further steps are available and the user has to choose the next action.
 * 
 * @author mweber
 *
 */
public class GUIScheduler implements TotalScheduler{
		private final SchedulingStrategy schedulingStrategy;
		
		public GUIScheduler(SchedulingStrategy schedulingStrategy) {
			this.schedulingStrategy = schedulingStrategy;
		}

		private boolean interrupted = false;
		@Override
		public ScheduleAction choose(ScheduleOptions options) {
			synchronized (schedulingStrategy) {
				while (schedulingStrategy.waitForUserInput) {
		            try {
		            	schedulingStrategy.wait();
		            } catch (InterruptedException e) {
		            	/* 
		            	 * ignoring exception, because this comes from the runtime
		            	 * and if the runtime is still active, we would have to
		            	 * return a valid scheduling option here (which we can
		            	 * not guess).
		            	 */
		            	System.out.println("System terminated");
		            	return null;
		            }
		            if(interrupted && schedulingStrategy.curScheduler!=null)
		            	return schedulingStrategy.curScheduler.choose(options);
		        }
		        schedulingStrategy.waitForUserInput = true;
		        schedulingStrategy.baseScheduler.reset();
		        ScheduleAction res = schedulingStrategy.baseScheduler.choose(options);
		        return res;
			}
		}

		@Override
		public TaskInfo schedule(TaskScheduler scheduler,
				final List<TaskInfo> scheduableTasks) {
			TaskInfo task = schedulingStrategy.baseScheduler.schedule(scheduler, scheduableTasks);
			return task;
		}
		
		/**
		 * Interrupts the scheduler, which is waiting for user input.
		 * Should be used to signal that the curScheduler changed.
		 */
		public void interrupt(){
			interrupted = true;
		}

		@Override
		public void reset() {
		}
		
	}