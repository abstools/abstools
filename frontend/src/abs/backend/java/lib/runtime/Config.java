/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.backend.java.lib.runtime;

import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.util.ArrayList;
import java.util.List;
import java.util.logging.Logger;

import abs.backend.java.lib.runtime.RuntimeOptions.Option;
import abs.backend.java.lib.runtime.RuntimeOptions.OptionType;
import abs.backend.java.observing.SystemObserver;
import abs.backend.java.scheduling.DefaultTaskScheduler;
import abs.backend.java.scheduling.GlobalSchedulingStrategy;
import abs.backend.java.scheduling.RandomSchedulingStrategy;
import abs.backend.java.scheduling.RecordingSchedulerStrategy;
import abs.backend.java.scheduling.SimpleTaskScheduler;
import abs.backend.java.scheduling.TaskSchedulerFactory;
import abs.backend.java.scheduling.TaskSchedulingStrategy;
import abs.backend.java.scheduling.TotalSchedulingStrategy;

/**
 * Evaluates system properties
 * 
 * @author Jan Schäfer
 * 
 */
public class Config {

    private static final Logger logger = Logging.getLogger(Config.class.getName());

    private final ABSRuntime runtime;

    private final RuntimeOptions options;

    public Config(ABSRuntime runtime, RuntimeOptions options) {
        this.runtime = runtime;
        this.options = options;
        if (options.help.isTrue()) {
            printHelp();
            System.exit(1);
        }
        
        configureRuntime();
    }
    
    public void configureRuntime() {
        setSimpleOptions();
        loadSystemObserver();
        loadTotalSchedulingStrategy();
        loadTaskSchedulingStrategy();
        loadGlobalSchedulingStrategy();
        loadSchedulerFactory();
    }
    
    public void loadSystemObserver() {
        if (options.systemObserver.wasSet()) {
            for (String s : options.systemObserver.stringArrayValue()) {
                runtime.addSystemObserver((SystemObserver) loadClassByName(s));
            }
        }
    }

    public void setSimpleOptions() {
        runtime.enableDebugging(options.debug.isTrue());
        runtime.terminateOnException(options.terminateOnException.isTrue());
        
        
        loadRandomSeed();
    }

    private void loadRandomSeed() {
        runtime.setRandomSeed(options.randomSeed.longValue());
        
        if (options.printRandomSeed.isTrue()) {
            System.out.println("random seed="+options.randomSeed.longValue());
        }

        if (options.useRandomScheduler.isTrue()) {
            options.totalScheduler.setValue(RandomSchedulingStrategy.class.getName());
        }
    }

    public void printHelp() {
        System.err.println(" ABS Java Backend - Runtime Configuration Options");
        System.err.println(" ================================================");
        for (Option o : options.options) {
            if (o.type != OptionType.BOOLEAN) {
                System.err.println(String.format("%36s\t%s", "-"+o.name + "=<"
                    + o.type.toString().toLowerCase() + ">", o.description));
            } else {
                System.err.println(String.format("%36s\t%s", "-"+o.name, o.description));
            }
        }
    }

    private static Object loadClassByName(String s, Object...args) {
        try {
            Class<?> clazz = Config.class.getClassLoader().loadClass(s);
            for (Constructor<?> c : clazz.getConstructors()) {
                if (c.getParameterTypes().length == args.length) {
                    return c.newInstance(args);
                }
            }
            return clazz.newInstance();
        } catch (Exception e) {
            e.printStackTrace();
        } 
        return null;
    }

    
    public void loadTotalSchedulingStrategy() {
        if (options.totalScheduler.wasSet()) {
            TotalSchedulingStrategy strat = (TotalSchedulingStrategy) loadClassByName(options.totalScheduler.stringValue(), runtime.getRandom());
            
            if (strat != null) {
                logger.config("Using total scheduling strategy defined by class " + strat.getClass().getName());
                runtime.setTotalSchedulingStrategy(strat);
            } else {
                logger.warning("Could not load total scheduler class "+options.totalScheduler.stringValue());
            }
        }
    }

    public void loadTaskSchedulingStrategy() {
        if (options.taskSchedulerStrategy.wasSet()) {
            TaskSchedulingStrategy strat = (TaskSchedulingStrategy) loadClassByName(options.taskSchedulerStrategy.stringValue());
            if (strat == null) {
                logger.warning("Could not load task scheduling strategy class "+options.taskSchedulerStrategy.stringValue());
                return;
            }

            if (strat instanceof RandomSchedulingStrategy) {
                ((RandomSchedulingStrategy) strat).setRandom(runtime.getRandom());
            }

            logger.config("Using task scheduling strategy defined by class " + strat.getClass().getName());

            if (options.recordTaskScheduler.isTrue()) {
                strat = new RecordingSchedulerStrategy(strat);
                logger.config("Recording schedule");
            }

            runtime.setTaskSchedulingStrategy(strat);
        }
    }

    public void loadGlobalSchedulingStrategy() {
        if (options.globalScheduler.wasSet()) {
            GlobalSchedulingStrategy strat = (GlobalSchedulingStrategy) loadClassByName(options.globalScheduler.stringValue());
            if (strat != null)
                runtime.setGlobalSchedulingStrategy(strat);
            else    
                logger.warning("Could not load task global strategy class "+options.globalScheduler.stringValue());
        }
            
    }

    private void loadSchedulerFactory() {
        
        TaskSchedulerFactory taskSchedulerFactory;
        // only the SimpleTaskScheduler supports configuring
        if (options.taskSchedulerStrategy.wasSet() || options.totalScheduler.wasSet()) {
            logger.config("Using simple task scheduler, because task scheduling is specified");
            taskSchedulerFactory = SimpleTaskScheduler.getFactory();
        } else {
            String schedulerName = options.taskScheduler.stringValue();
            logger.config("Scheduler: " + schedulerName);
            if (schedulerName.equals("default")) {
                taskSchedulerFactory = DefaultTaskScheduler.getFactory();
            } else if (schedulerName.equals("simple")) {
                taskSchedulerFactory = SimpleTaskScheduler.getFactory();
            } else {
                logger.warning("The task scheduler " + schedulerName
                        + " does not exist, falling back to the default task scheduler.");
                taskSchedulerFactory = DefaultTaskScheduler.getFactory();
            }
        }
        runtime.setTaskSchedulerFactory(taskSchedulerFactory);
    }

}
