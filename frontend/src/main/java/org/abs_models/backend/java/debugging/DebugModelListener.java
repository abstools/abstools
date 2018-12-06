/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.abs_models.backend.java.debugging;

public interface DebugModelListener {
    void taskInfoChanged(TaskInfo line);

    void taskInfoAdded(TaskInfo line);

    void taskInfoRemoved(TaskInfo line);

    void cogCreated(COGInfo info);

    void cogChanged(COGInfo info);
}
