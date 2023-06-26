/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved.
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.abs_models.backend.java.scheduling;

import java.util.ArrayList;
import java.util.List;

import org.abs_models.backend.java.lib.runtime.COG;

public class SchedulableTasksFilterFifo implements SchedulableTasksFilter {

    @Override
    public List<SimpleTaskScheduler.TaskInfo> filter(List<SimpleTaskScheduler.TaskInfo> schedulableTasks) {
          List<SimpleTaskScheduler.TaskInfo> result = new ArrayList<>(schedulableTasks.size());

          with_next_task :
          for (SimpleTaskScheduler.TaskInfo task : schedulableTasks) {
              if (!task.hasBeenActivated) {
                  for (SimpleTaskScheduler.TaskInfo otherTask : schedulableTasks) {
                      if (otherTask.id < task.id
                              && !otherTask.hasBeenActivated
                              && getSourceCog(otherTask) == getSourceCog(task)
                              && getTargetCog(otherTask) == getTargetCog(otherTask)) {
                          // there is an earlier unactivated task => skip this task
                          continue with_next_task;
                      }
                  }
              }
              result.add(task);
          }
          return result;
    }

    private COG getTargetCog(SimpleTaskScheduler.TaskInfo task) {
        return task.task.getCOG();
    }

    private COG getSourceCog(SimpleTaskScheduler.TaskInfo task) {
        return task.task.getCall().getSource().getCOG();
    }

}
