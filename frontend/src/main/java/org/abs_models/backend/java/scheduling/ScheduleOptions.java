/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved.
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.abs_models.backend.java.scheduling;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

public class ScheduleOptions {
    private List<ScheduleAction> actions = new ArrayList<>();

    public int numOptions() {
        return actions.size();
    }

    public void addOption(ScheduleAction a) {
        // do not add two actions for the same COG
        for (ScheduleAction a2 : actions) {
            if (a.getCOG() == a2.getCOG())
                return;
        }

        actions.add(a);
    }


    public List<ScheduleAction> allOptions() {
        return Collections.unmodifiableList(actions);
    }

    public void removeOption(ScheduleAction next) {
        actions.remove(next);
    }

    public boolean isEmpty() {
        return actions.isEmpty();
    }

}
