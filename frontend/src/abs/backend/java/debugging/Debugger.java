package abs.backend.java.debugging;

import abs.backend.java.observing.TaskView;

public interface Debugger {
    public void nextStep(TaskView taskView, String fileName, int line);
}
