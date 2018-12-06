/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.abs_models.backend.java;

import org.abs_models.backend.java.debugging.Debugger;
import org.abs_models.backend.java.observing.TaskView;

public class TestDebugger implements Debugger {

    @Override
    public void nextStep(TaskView taskView, String fileName, int line) {
        System.out.println(taskView.getMethodName() + ":" + fileName + ":" + line);
    }

}
