package abs.backend.java.debugging;

import java.util.concurrent.Semaphore;

import abs.backend.java.observing.FutView;
import abs.backend.java.observing.GuardView;
import abs.backend.java.observing.TaskView;

public class TaskInfo {
    public FutView blockedOnFuture;
    TaskView task;
    int currentLine;
    int previousLine;
    String previousFile;
    String currentFile;
    TaskState state = TaskState.READY;
    GuardView waitingOnGuard;
    Semaphore stepSema = new Semaphore(0);
    boolean isStepping = true;
    
    public TaskInfo(TaskView task) {
       this.task = task;
    }
    
    public String toString() {
        return "Task " + task.getID();
    }
    
    public void updateLine(int newLine) {
        previousLine = currentLine;
        currentLine = newLine;
    }
    
    public void updateFile(String fileName) {
        previousFile = currentFile;
        currentFile = fileName;
    }
    
    //New Method
    public TaskState getState(){
        return state;
    }
}
