package abs.backend.java.lib.runtime;

import java.util.ArrayList;
import java.util.List;

public class ABSThreadManager {
    private final List<ABSThread> threads = new ArrayList<ABSThread>();
    
    synchronized void addThread(ABSThread t) {
        threads.add(t);
    }
    
    synchronized void removeThread(ABSThread t) {
        threads.remove(t);
    }
    
    synchronized void shutdownAllThreads() {
        for (ABSThread t : threads) {
            t.shutdown();
        }
    }
}
