package abs.backend.java.lib.runtime;

public class ABSThread extends Thread {
    private COG cog;

    public ABSThread(Runnable r) {
        super(r);
        setName("ABS Main Thread");
    }

    public ABSThread() {
        super();
        setName("ABS Main Thread");
    }

    public COG getCOG() {
        return cog;
    }

    public void setCOG(COG c) {
        cog = c;

    }

    public void checkGuard() {
    }
}
