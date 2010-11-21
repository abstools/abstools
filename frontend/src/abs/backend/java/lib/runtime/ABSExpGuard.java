package abs.backend.java.lib.runtime;

import abs.backend.java.lib.types.ABSBool;

public abstract class ABSExpGuard extends ABSGuard {

    public abstract ABSBool evaluateExp();

    @Override
    public boolean staysTrue() {
        return false;
    }

    @Override
    public boolean isTrue() {
        return evaluateExp().toBoolean();
    }

    @Override
    public String toString() {
        return "ExpressionGuard (" + isTrue() + ")";
    }

    @Override
    public String toABSString() {
        return "<exp>";
    }
}
