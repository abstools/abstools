package abs.backend.java.lib.runtime;

import abs.backend.java.lib.types.ABSRef;

public class ABSMainTask<T extends ABSRef> extends Task<T> {

    public ABSMainTask(T target) {
        super(null, target);
    }

    @Override
    public Object execute() {
        ((ABSObject)target).run(); 
        return null;
    }

    @Override
    public String methodName() {
        return "main block";
    }

}
