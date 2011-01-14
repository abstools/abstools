package abs.backend.java.lib.runtime;

public class ABSMainTask extends Task<ABSObject> {

    public ABSMainTask(ABSObject target) {
        super(null, target);
    }

    @Override
    public Object execute() {
        target.run(); 
        return null;
    }

    @Override
    public String methodName() {
        return "main block";
    }

}
