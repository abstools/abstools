package abs.backend.java.lib.runtime;

import java.util.concurrent.atomic.AtomicInteger;

public abstract class Task<T> {
    private static AtomicInteger counter = new AtomicInteger();
    private final ABSFut future;
    protected final T target;
    private final int id = counter.incrementAndGet();
    
    public Task(T target) {
        this.target = target;
        future = new ABSFut(this);
    }
    
    public COG getCOG() {
        return ((ABSObject)target).getCOG();        
    }
    
    public void schedule() {
        getCOG().addTask(this);
    }
    
    public ABSFut getFut() {
        return future;
    }
    
    public void run() {
        Object res = execute();
        future.resolve(res);
    }
    
    public abstract Object execute();
    
    public abstract String methodName();
    
    public String toString() {
        return "Task ("+id+") ["+getCOG()+", Method: "+target.getClass().getSimpleName()+"."+methodName()+"]";
    }
}
