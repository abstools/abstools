package abs.backend.java.scheduling;

import java.util.List;
import java.util.Random;
import java.util.logging.Logger;

import abs.backend.java.lib.runtime.ABSRuntime;
import abs.backend.java.lib.runtime.Config;
import abs.backend.java.lib.runtime.Logging;

public class RandomGlobalSchedulingStrategy implements GlobalSchedulingStrategy {
    private final static Logger logger = Logging.getLogger(RandomGlobalSchedulingStrategy.class.getName());

    private final Random random;
    
    public RandomGlobalSchedulingStrategy(Random r) {
        random = r;
    }

    @Override
    public synchronized ScheduleAction choose(ScheduleOptions options) {
        ScheduleAction a = options.allOptions().get(random.nextInt(options.numOptions()));
        String suff = options.numOptions() == 1 ? " (NO CHOICE)" : "";
        logger.finest("Choosing "+a.shortString()+" from "+shortStringList(options.allOptions())+suff);
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
