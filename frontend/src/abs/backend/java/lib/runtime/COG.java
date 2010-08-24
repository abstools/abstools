package abs.backend.java.lib.runtime;

import java.util.concurrent.atomic.AtomicInteger;

public class COG {
	private TaskScheduler scheduler = new TaskScheduler(this);
	private Class<?> initialClass;
	private static AtomicInteger counter = new AtomicInteger();
	private final int id = counter.incrementAndGet();

	public COG(Class<?> clazz) {
	    initialClass = clazz;
	}
	
	public TaskScheduler getScheduler() {
	   return scheduler;
    }

	public void release() {
		
	}
	
	public void aquire() {
		
	}
	
	public void addTask(Task task) {
	    scheduler.addTask(task);
	}

    public int getID() {
        return id;
    }
}
