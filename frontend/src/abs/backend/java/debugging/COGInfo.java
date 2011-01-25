package abs.backend.java.debugging;

import java.util.ArrayList;
import java.util.List;

import abs.backend.java.observing.COGView;
import abs.backend.java.observing.ObjectView;

public class COGInfo {
    final COGView cog;
    final ObjectView initialObject;
    final List<TaskInfo> tasks = new ArrayList<TaskInfo>();

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