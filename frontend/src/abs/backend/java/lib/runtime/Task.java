package abs.backend.java.lib.runtime;

public abstract class Task<T> {
    private final ABSFut future;
    protected final T target;
    
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
}
