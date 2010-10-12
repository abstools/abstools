package abs.backend.java.scheduling;

import abs.backend.java.lib.runtime.COG;

public abstract class ScheduleAction {
    private final COG cog;
    private boolean executed;
    
    public ScheduleAction(COG cog) {
        this.cog = cog;
    }
    
    public COG getCOG() {
        return cog;
    }
    
    public synchronized void execute() {
        executed = true;
        notify();
    }
    
    public synchronized void await() {
        try {
            while (!executed) {
                wait();
            }
        } catch (InterruptedException e) {
            e.printStackTrace();
        }
    }

    public abstract String shortString();
    
}
