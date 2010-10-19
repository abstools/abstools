package abs.backend.java.lib.runtime;

public class ABSException extends RuntimeException {

    public ABSException(String string) {
        super(string);
    }
    
    @Override
    public String getMessage() {
        String res = super.getMessage();
        res += "\nRandom seed: "+Config.RANDOM_SEED;
        return res;
    }

}
