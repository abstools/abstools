package abs.backend.java.lib.runtime;

public abstract class ABSException extends RuntimeException {

    public ABSException(String string) {
        super(string);
    }
    
    @Override
    public String getMessage() {
        String res = super.getMessage();
        res += "\nRandom seed: "+Config.RANDOM_SEED;
        return res;
    }
    
    public boolean isDeadlock() { return false; }
    public boolean isAssertion() { return false; }
    public boolean isIllegalSynchronousCall() { return false;  }
    public boolean isNullPointer() { return false;  }

    public abstract String getName();

}
