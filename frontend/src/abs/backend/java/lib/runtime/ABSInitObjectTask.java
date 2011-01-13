package abs.backend.java.lib.runtime;

public class ABSInitObjectTask<T extends ABSObject> extends Task<T> {

    public ABSInitObjectTask(Task<?> sender, ABSObject source, T target) {
        super(sender, source, target);
    }

    @Override
    public Object execute() {
        target.__ABS_init();
        return null;
    }

    @Override
    public String methodName() {
        return "initialization block";
    }

}
