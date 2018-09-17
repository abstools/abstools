/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.abs_models.backend.java.scheduling;

import org.abs_models.backend.java.lib.runtime.COG;

public class ScheduleTask extends ScheduleAction {
    public ScheduleTask(COG cog) {
        super(cog);
    }

    @Override
    public String toString() {
        return "Schedule task in COG " + getCOG().getID();
    }

    @Override
    public String shortString() {
        return getCOG().getID() + ",S,";
    }

    public boolean equals(Object o) {
        if (!(o instanceof ScheduleTask))
            return false;
        return this.getCOG() == ((ScheduleTask) o).getCOG();
    }

}
