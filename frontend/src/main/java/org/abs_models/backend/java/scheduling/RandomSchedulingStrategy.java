/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.abs_models.backend.java.scheduling;

import java.util.List;
import java.util.Random;
import java.util.logging.Logger;

import org.abs_models.backend.java.lib.runtime.Logging;

/**
 * A scheduling strategy that randomly chooses the next task
 * 
 * It is possible to set the initial seed used for the random generator by
 * setting the property abs.schedulerseed to some long value.
 * 
 * @author Jan Schäfer
 * 
 */
public class RandomSchedulingStrategy implements TotalSchedulingStrategy, UsesRandomSeed {
    private final static Logger logger = Logging.getLogger(RandomSchedulingStrategy.class.getName());

    private Random random;

    public RandomSchedulingStrategy() {
    }
    
    public RandomSchedulingStrategy(Random r) {
        random = r;
    }
    
    public void setRandom(Random r) {
        random = r;
    }

    @Override
    public synchronized SimpleTaskScheduler.TaskInfo schedule(TaskScheduler scheduler, List<SimpleTaskScheduler.TaskInfo> schedulableTasks) {
        return schedulableTasks.get(random.nextInt(schedulableTasks.size()));
    }

    @Override
    public synchronized ScheduleAction choose(ScheduleOptions options) {
        ScheduleAction a = options.allOptions().get(random.nextInt(options.numOptions()));
        String suff = options.numOptions() == 1 ? " (NO CHOICE)" : "";
        logger.finest(() -> "Choosing " + a.shortString() + " from " + shortStringList(options.allOptions()) + suff);
        return a;
    }

    private String shortStringList(List<ScheduleAction> allOptions) {
        StringBuilder res = new StringBuilder();
        res.append("{");
        boolean first = true;
        for (ScheduleAction a : allOptions) {
            if (first)
                first = false;
            else
                res.append(", ");
            res.append(a.shortString());
        }
        res.append("}");
        return res.toString();
    }
    
}
