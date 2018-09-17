/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.abs_models.backend.java.visualization;

import org.abs_models.backend.java.observing.TaskView;

public class UMLSequenceChart extends SequenceDiagramVisualization {
    
    @Override
    public boolean isObserved(TaskView task) {
        return true;
    }

    @Override
    public boolean isObservedClass(String className) {
        return true;
    }

    @Override
    protected boolean isSystemClass(String source) {
        return !isEnvironmentClass(source);
    }

}
