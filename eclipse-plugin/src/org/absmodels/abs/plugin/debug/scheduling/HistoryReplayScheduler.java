package org.absmodels.abs.plugin.debug.scheduling;

import static org.absmodels.abs.plugin.debug.DebugUtils.getSchedulerRef;
import static org.absmodels.abs.plugin.util.UtilityFunctions.showErrorMessageAsync;
import static org.absmodels.abs.plugin.util.UtilityFunctions.standardExceptionHandling;

import java.io.File;
import java.io.IOException;
import java.util.List;

import org.absmodels.abs.plugin.debug.DebugUtils;
import org.absmodels.abs.plugin.util.Constants.Scheduler;
import org.eclipse.swt.widgets.FileDialog;

import abs.backend.java.lib.runtime.ABSRuntime;
import abs.backend.java.scheduling.HistoryAction;
import abs.backend.java.scheduling.HistoryItem;
import abs.backend.java.scheduling.ScheduleAction;
import abs.backend.java.scheduling.ScheduleOptions;
import abs.backend.java.scheduling.SimpleTaskScheduler.TaskInfo;
import abs.backend.java.scheduling.TaskScheduler;

public class HistoryReplayScheduler implements TotalScheduler {

    private SchedulingStrategy schedulingStrategy;
    private List<HistoryItem> history;
    private List<HistoryItem> completeHistory;

    public HistoryReplayScheduler(SchedulingStrategy schedulingStrategy) {
        this.schedulingStrategy = schedulingStrategy;
        
        String historyFile = DebugUtils.getHistoryFile();
        
        if (historyFile == null || historyFile.isEmpty()) {
            showLoadHistoryDialog();
        } else {
            loadHistory(new File(historyFile));
        }
    }

    
    void showLoadHistoryDialog() {
        FileDialog dialog = new FileDialog(DebugUtils.getDebugViewer().getControl().getShell());
//        dialog.setFilterExtensions(new String[]{"*.log"});
        final String fileName = dialog.open();
        if (fileName == null) {
            DebugUtils.setScheduler(Scheduler.interactive);
            if(getSchedulerRef() != null)
                getSchedulerRef().updateScheduler();
            return;
        }
        File file = new File(fileName);
        loadHistory(file);
    }
    
    private void loadHistory(File file) {
        try {
            completeHistory = HistoryItem.loadHistory(file);
            history = completeHistory;
        } catch (IOException e) {
            standardExceptionHandling(e);
            showErrorMessageAsync("Could not load history.");
        }

    }


    @Override
    public ScheduleAction choose(ScheduleOptions options) {
        ScheduleAction a = getOptionByHistory(options);
        if (history.isEmpty()) {
            clearScheduler();
        }
        return a;
    }
    
    private ScheduleAction getOptionByHistory(ScheduleOptions options) {
        HistoryItem item = history.remove(0);
        for (ScheduleAction a : options.allOptions()) {
            if (item.matches(a)) {
                return a;
            }
        }
        showErrorMessageAsync("Illegal History!");
        ABSRuntime.getCurrentRuntime().shutdown();
        return null;
    }

    @Override
    public TaskInfo schedule(TaskScheduler scheduler, List<TaskInfo> scheduableTasks) {
        TaskInfo i = getNextTask(scheduableTasks);
        if (history.isEmpty()) {
            clearScheduler();
        }
        return i;
    }

    private TaskInfo getNextTask(List<TaskInfo> scheduableTasks) {
        if (!history.isEmpty()) {
            try {
                HistoryItem at = history.remove(0);
                if (at.action != HistoryAction.ACTIVATE)
                    throw new IllegalStateException("Task action action expected!");

                for (TaskInfo i : scheduableTasks) {
                    if (i.task.getID() == at.taskid && i.task.getCOG().getID() == at.cogId) {

                        return i;
                    }
                }
                throw new IllegalStateException("Task " + at.taskid + " cannot be scheduled.");
            } catch (Exception e) {
                e.printStackTrace();
                showErrorMessageAsync("Illegal History! " + e.getMessage());
            }
        }
        return null;
    }
    

    private void clearScheduler() {
        schedulingStrategy.setBaseScheduler(null);
    }

    @Override
    public void reset() {
        history = completeHistory;
    }

}
