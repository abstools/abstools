/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved.
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.backend.java.scheduling;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import abs.backend.java.lib.runtime.COG;
import abs.backend.java.scheduling.SimpleTaskScheduler.TaskInfo;

public class ScheduableTasksFilterFifo implements ScheduableTasksFilter {

    @Override
    public List<TaskInfo> filter(List<TaskInfo> scheduableTasks) {
          List<TaskInfo> result = new ArrayList<>(scheduableTasks.size());

          with_next_task :
          for (TaskInfo task : scheduableTasks) {
              if (!task.hasBeenActivated) {
                  for (TaskInfo otherTask : scheduableTasks) {
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

    private COG getTargetCog(TaskInfo task) {
        return task.task.getCOG();
    }

    private COG getSourceCog(TaskInfo task) {
        return task.task.getCall().getSource().getCOG();
    }

}
