package abs.backend.java.scheduling;

import java.util.List;
import java.util.Random;
import java.util.logging.Logger;

import abs.backend.java.lib.runtime.Logging;
import abs.backend.java.scheduling.SimpleTaskScheduler.TaskInfo;

/**
 * A scheduling strategy that randomly chooses the next task
 * 
 * It is possible to set the initial seed used for the random generator by 
 * setting the property abs.schedulerseed to some long value.
 * 
 * @author Jan Schäfer
 *
 */
public class RandomTaskSchedulingStrategy implements TaskSchedulingStrategy {
    private final static Logger logger = Logging.getLogger(RandomTaskSchedulingStrategy.class.getName());

    private final Random random;
    private final long seed;
    
    public static final RandomTaskSchedulingStrategy INSTANCE = new RandomTaskSchedulingStrategy(); 

    public RandomTaskSchedulingStrategy() {
        String seedString = System.getProperty("abs.schedulerseed");
        if (seedString == null)
            seed = System.nanoTime();
        else
            seed = Long.parseLong(seedString);
        
        logger.info("Random Task Scheduler Seed="+seed+(seedString != null ? " (as specified)" : ""));
        random = new Random(seed);
    }
    
    @Override
    public synchronized TaskInfo schedule(TaskScheduler scheduler,
            List<TaskInfo> schedulableTasks) {
        return schedulableTasks.get(random.nextInt(schedulableTasks.size()));
    }

}
