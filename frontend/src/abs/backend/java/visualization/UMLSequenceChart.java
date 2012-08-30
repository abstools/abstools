/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.backend.java.visualization;

import abs.backend.java.observing.TaskView;

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
