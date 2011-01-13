package abs.backend.java.lib.runtime;

public class ABSRunMethodTask<T extends ABSObject> extends Task<T> {

    public ABSRunMethodTask(Task<?> sender, ABSObject source, T target) {
        super(sender, source, target);
    }

    @Override
    public Object execute() {
        target.run();
        return null;
    }

    @Override
    public String methodName() {
        return "run";
    }

}
