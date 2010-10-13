package abs.backend.java.scheduling;

import java.util.List;
import java.util.Random;
import java.util.logging.Logger;

import abs.backend.java.lib.runtime.Logging;

public class RandomGlobalSchedulingStrategy implements GlobalSchedulingStrategy {
    private final static Logger logger = Logging.getLogger(RandomGlobalSchedulingStrategy.class.getName());
    
    Random random = new Random();
    private long seed;
    
    public RandomGlobalSchedulingStrategy() {
        String seedString = System.getProperty("abs.globalschedulerseed");
        if (seedString == null)
            seed = System.nanoTime();
        else
            seed = Long.parseLong(seedString);
        
        logger.info("Global Scheduler Seed="+seed+(seedString != null ? " (as specified)" : ""));
        random = new Random(seed);
    }
    
    
    @Override
    public ScheduleAction choose(ScheduleOptions options) {
        ScheduleAction a = options.allOptions().get(random.nextInt(options.numOptions()));
        String suff = options.numOptions() == 1 ? " (NO CHOICE)" : "";
        System.out.println("Choosing "+a.shortString()+" from "+shortStringList(options.allOptions())+suff);
        return a;
    }

    private String shortStringList(List<ScheduleAction> allOptions) {
        StringBuilder res = new StringBuilder();
        res.append("{");
        boolean first = true;
        for (ScheduleAction a : allOptions) {
            if (first) first = false;
            else res.append(", ");
            res.append(a.shortString());
        }
        res.append("}");
        return res.toString();
    }

}
