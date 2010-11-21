package abs.backend.java;

import abs.backend.java.debugging.Debugger;
import abs.backend.java.observing.TaskView;

public class TestDebugger implements Debugger {

    @Override
    public void nextStep(TaskView taskView, String fileName, int line) {
        System.out.println(taskView.getMethodName() + ":" + fileName + ":" + line);
    }

}
