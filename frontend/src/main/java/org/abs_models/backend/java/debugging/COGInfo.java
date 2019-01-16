/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved.
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.abs_models.backend.java.debugging;

import java.util.ArrayList;
import java.util.List;

import org.abs_models.backend.java.observing.COGView;
import org.abs_models.backend.java.observing.ObjectView;

public class COGInfo {
    final COGView cog;
    final ObjectView initialObject;
    final List<TaskInfo> tasks = new ArrayList<>();

    COGInfo(COGView cog, ObjectView o) {
        this.cog = cog;
        this.initialObject = o;
    }

    public void addTask(TaskInfo task) {
        tasks.add(task);
    }

    //New Method
    public List<TaskInfo> getTasks(){
        return tasks;
    }

    public ObjectView getInitialObject(){
        return initialObject;
    }

}
