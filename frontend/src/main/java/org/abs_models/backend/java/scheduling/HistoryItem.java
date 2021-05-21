/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved.
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.abs_models.backend.java.scheduling;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

public class HistoryItem {


    public int cogId;
    public int taskid;
    public HistoryAction action;

    HistoryItem(String s) {
        String[] strings = s.trim().split(",");
        cogId = Integer.parseInt(strings[0]);
        String a = strings[1];
        if (a.equals("S"))
            action = HistoryAction.SCHEDULE;
        else if (a.equals("E"))
            action = HistoryAction.EXECUTE;
        else if (a.equals("A"))
            action = HistoryAction.ACTIVATE;

        if (action != HistoryAction.SCHEDULE) {
            taskid = Integer.parseInt(strings[2]);
        }
    }

    public boolean matches(ScheduleAction a) {
        if (cogId != a.getCOG().getID())
            return false;

        if ((a instanceof StepTask && action != HistoryAction.EXECUTE)
                || (a instanceof ScheduleTask && action != HistoryAction.SCHEDULE)
                || (a instanceof ActivateTask && action != HistoryAction.ACTIVATE))
            return false;

        if (action != HistoryAction.SCHEDULE) {
            if (a.getTask() == null) {
                System.err.println(a);
                System.err.println(action);
                System.err.println(a.getClass());
            }
            return a.getTask().getID() == taskid;
        } else {
            return true;
        }
    }

    @Override
    public String toString() {
        return cogId + "," + action.toString() + "," + taskid;
    }



    public static List<HistoryItem> loadHistory(File f) throws IOException {
        List<HistoryItem> history;
        try (BufferedReader reader = new BufferedReader(new FileReader(f))) {
            history = new ArrayList<>();
            while (reader.ready()) {
                String s = reader.readLine();
                if (s == null)
                    break;

                history.add(new HistoryItem(s));
            }
        }

        return history;
    }

}
