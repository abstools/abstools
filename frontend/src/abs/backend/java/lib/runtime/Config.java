/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.backend.java.lib.runtime;

import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.util.ArrayList;
import java.util.List;
import java.util.Random;
import java.util.logging.Logger;

import abs.backend.java.observing.SystemObserver;
import abs.backend.java.scheduling.DefaultTaskScheduler;
import abs.backend.java.scheduling.GlobalScheduler;
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
    static enum OptionType {
        BOOLEAN, STRING, CLASS, LONG
    }

    static class ConfigOption {
        String propertyName;
        String description;
        OptionType type;

        ConfigOption(String name, String desc, OptionType type) {
            this.propertyName = name;
            this.description = desc;
            this.type = type;
        }
    }

    private static final List<ConfigOption> options = new ArrayList<ConfigOption>();

    public static final ConfigOption LOGLEVEL_OPTION = newOption("abs.loglevel", "sets a logging level",
            OptionType.STRING);
    public static final ConfigOption DEBUG_OPTION = newOption("abs.debug", "enables debugging", OptionType.BOOLEAN);
    public static final ConfigOption TERMINATE_ON_EXCEPTION = newOption("abs.terminateOnException", "terminates the system when an exception occurs", OptionType.BOOLEAN);
    public static final ConfigOption SYSTEM_OBSERVER_OPTION = newOption("abs.systemobserver",
            "sets a system observer class", OptionType.CLASS);
    public static final ConfigOption TOTAL_SCHEDULER_OPTION = newOption("abs.totalscheduler",
            "sets a total scheduler class", OptionType.CLASS);
    public static final ConfigOption GLOBAL_SCHEDULER_OPTION = newOption("abs.globalscheduler",
            "sets a global scheduler class", OptionType.CLASS);
    public static final ConfigOption TASK_SCHEDULER_STRATEGY_OPTION = newOption("abs.taskschedulerstrategy",
            "sets a task scheduler strategy class", OptionType.CLASS);
    public static final ConfigOption TASK_SCHEDULER_OPTION = newOption("abs.taskscheduler",
            "sets the task scheduler to be used", OptionType.STRING);
    public static final ConfigOption RECORD_TASK_SCHEDULER_OPTION = newOption("abs.recordtaskscheduler",
            "enables recording of task scheduling", OptionType.BOOLEAN);
    public static final ConfigOption RANDOM_SEED_OPTION = newOption("abs.randomseed",
            "set the random seed used by schedulers", OptionType.LONG);

    private static ConfigOption newOption(String name, String desc, OptionType type) {
        ConfigOption o = new ConfigOption(name, desc, type);
        options.add(o);
        return o;
    }

    private static final Logger logger = Logging.getLogger(Config.class.getName());

    private boolean configuredTaskScheduling = false;

    private ABSRuntime runtime;

    {
        if (printHelp()) {
            System.exit(1);
        }
    }
    
    public Config(ABSRuntime runtime) {
        this.runtime = runtime;
        loadProperties();
    }
    
    public void loadProperties() {
        loadSystemObserver();
        loadFlagOptions();
        loadRandomSeed();
        loadTotalSchedulingStrategy();
        loadTaskSchedulingStrategy();
        loadGlobalSchedulingStrategy();
        loadSchedulerFactory();
    }
    
    public void loadSystemObserver() {
        String observerString = System.getProperty(SYSTEM_OBSERVER_OPTION.propertyName);
        if (observerString != null) {
            String[] systemObservers = observerString.split(",");

            for (String s : systemObservers) {
                System.out.println(s);
                runtime.addSystemObserver((SystemObserver) loadClassByName(s));
            }
        }
    }

    public void loadFlagOptions() {
        runtime.enableDebugging(System.getProperty(DEBUG_OPTION.propertyName, "false").equals("true"));
        runtime.terminateOnException(System.getProperty(TERMINATE_ON_EXCEPTION.propertyName, "false").equals("true"));
    }

    public static Object loadClassByProperty(String property, Object... args) {
        String s = System.getProperty(property);
        if (s != null) {
            return loadClassByName(s, args);
        }
        return null;
    }

    public static boolean printHelp() {
        if (Boolean.parseBoolean(System.getProperty("abs.help", "false"))) {
            System.err.println(" ABS Java Backend - Runtime Configuration Options");
            System.err.println(" ================================================");
            for (ConfigOption o : options) {
                System.err.println(String.format("%36s\t%s", "-D" + o.propertyName + "=<"
                        + o.type.toString().toLowerCase() + ">", o.description));
            }
            return true;
        }
        
        return false;
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
        } catch (ClassNotFoundException e) {
            e.printStackTrace();
        } catch (InstantiationException e) {
            e.printStackTrace();
        } catch (IllegalAccessException e) {
            e.printStackTrace();
        } catch (IllegalArgumentException e) {
            e.printStackTrace();
        } catch (InvocationTargetException e) {
            e.printStackTrace();
        }
        return null;
    }

    
    public void loadRandomSeed() {
        long randomSeed = System.nanoTime();
        String seedString = System.getProperty("abs.randomseed");
        if (seedString != null) {
            try {
                randomSeed = Long.parseLong(seedString);
            } catch (Exception e) {
                System.err.println("Illegal random seed " + seedString);
            }
            runtime.setRandomSeed(randomSeed);
        }

    }

    public void loadTotalSchedulingStrategy() {
        TotalSchedulingStrategy strat = (TotalSchedulingStrategy) loadClassByProperty(TOTAL_SCHEDULER_OPTION.propertyName, runtime.getRandom());
        if (strat != null) {
            logger.config("Using total scheduling strategy defined by class " + strat.getClass().getName());
            configuredTaskScheduling = true;
            runtime.setTotalSchedulingStrategy(strat);
        }
    }

    public void loadTaskSchedulingStrategy() {
        TaskSchedulingStrategy strat = (TaskSchedulingStrategy) loadClassByProperty(TASK_SCHEDULER_STRATEGY_OPTION.propertyName);
        if (strat == null)
            return;
        if (strat instanceof RandomSchedulingStrategy) {
            ((RandomSchedulingStrategy) strat).setRandom(runtime.getRandom());
        }
        configuredTaskScheduling = true;

        logger.config("Using task scheduling strategy defined by class " + strat.getClass().getName());

        boolean recording = Boolean
                .parseBoolean(System.getProperty(RECORD_TASK_SCHEDULER_OPTION.propertyName, "false"));
        if (recording) {
            strat = new RecordingSchedulerStrategy(strat);
            logger.config("Recording schedule");
        }

        runtime.setTaskSchedulingStrategy(strat);
    }

    public void loadGlobalSchedulingStrategy() {
        GlobalSchedulingStrategy strat = (GlobalSchedulingStrategy) loadClassByProperty(GLOBAL_SCHEDULER_OPTION.propertyName);
        if (strat != null)
            runtime.setGlobalSchedulingStrategy(strat);
    }

    private void loadSchedulerFactory() {
        
        TaskSchedulerFactory taskSchedulerFactory;
        // only the SimpleTaskScheduler supports configuring
        if (configuredTaskScheduling) {
            logger.config("Using simple task scheduler, because task scheduling is specified");
            taskSchedulerFactory = SimpleTaskScheduler.getFactory();
        } else {
            String schedulerName = System.getProperty(TASK_SCHEDULER_OPTION.propertyName, "default");
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
