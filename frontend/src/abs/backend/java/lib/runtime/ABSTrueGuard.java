package abs.backend.java.lib.runtime;

public class ABSTrueGuard extends ABSGuard {

    @Override
    public boolean isTrue() {
        return true;
    }

    @Override
    public String toString() {
        return "TrueGuard";
    }

    @Override
    public String toABSString() {
        return "True";
    }
}
