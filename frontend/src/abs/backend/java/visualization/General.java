package abs.backend.java.visualization;

import abs.backend.java.observing.TaskView;

public class General extends Main {

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
