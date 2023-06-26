/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.abs_models.backend.java.scheduling;

import java.util.List;

public class SchedulableTasksFilterDefault implements SchedulableTasksFilter {

    @Override
    public List<SimpleTaskScheduler.TaskInfo> filter(List<SimpleTaskScheduler.TaskInfo> schedulableTasks) {
        return schedulableTasks;
    }

}
