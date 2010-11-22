package abs.backend.java.lib.runtime;

import java.util.ArrayList;
import java.util.List;
import java.util.Random;
import java.util.logging.Logger;

import abs.backend.java.observing.SystemObserver;
import abs.backend.java.scheduling.DefaultTaskScheduler;
import abs.backend.java.scheduling.GlobalScheduler;
import abs.backend.java.scheduling.GlobalSchedulingStrategy;
import abs.backend.java.scheduling.RandomTaskSchedulingStrategy;
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

    public static final TotalSchedulingStrategy totalSchedulingStrategy;
    public static final GlobalScheduler globalScheduler;
    public static final TaskSchedulingStrategy taskSchedulingStrategy;
    public static final TaskSchedulerFactory taskSchedulerFactory;

    private static final List<ConfigOption> options = new ArrayList<ConfigOption>();

    public static final ConfigOption LOGLEVEL_OPTION = newOption("abs.loglevel", "sets a logging level",
            OptionType.STRING);
    public static final ConfigOption DEBUG_OPTION = newOption("abs.debug", "enables debugging", OptionType.BOOLEAN);
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

    public static final long RANDOM_SEED;
    public static final Random RANDOM = new Random(Config.RANDOM_SEED);

    private static boolean configuredTaskScheduling = false;

    public static final List<SystemObserver> systemObserver = new ArrayList<SystemObserver>();
    static {
        String observerString = System.getProperty(SYSTEM_OBSERVER_OPTION.propertyName);
        if (observerString != null) {
            String[] systemObservers = observerString.split(",");

            for (String s : systemObservers) {
                System.out.println(s);
                systemObserver.add((SystemObserver) loadClassByName(s));
            }
        }

    }

    public static final boolean DEBUGGING = System.getProperty(DEBUG_OPTION.propertyName, "false").equals("true");

    public static Object loadClassByProperty(String property) {
        String s = System.getProperty(property);
        if (s != null) {
            return loadClassByName(s);
        }
        return null;
    }

    static {
        if (Boolean.parseBoolean(System.getProperty("abs.help", "false"))) {
            System.err.println(" ABS Java Backend - Runtime Configuration Options");
            System.err.println(" ================================================");
            for (ConfigOption o : options) {
                System.err.println(String.format("%36s\t%s", "-D" + o.propertyName + "=<"
                        + o.type.toString().toLowerCase() + ">", o.description));
            }
            System.exit(1);
        }
    }

    private static Object loadClassByName(String s) {
        try {
            Class<?> clazz = Config.class.getClassLoader().loadClass(s);
            return clazz.newInstance();
        } catch (ClassNotFoundException e) {
            e.printStackTrace();
        } catch (InstantiationException e) {
            e.printStackTrace();
        } catch (IllegalAccessException e) {
            e.printStackTrace();
        }
        return null;
    }

    static {
        String seedString = System.getProperty("abs.randomseed");
        if (seedString == null) {
            RANDOM_SEED = System.nanoTime();
        } else {
            long seed = 0;
            try {
                seed = Long.parseLong(seedString);
            } catch (Exception e) {
                System.err.println("Illegal random seed " + seedString);
                System.exit(1);
            }
            RANDOM_SEED = seed;
        }

        logger.config("Random Seed: " + RANDOM_SEED);
    }

    public static long getSeed() {
        return RANDOM_SEED;
    }

    static {
        TotalSchedulingStrategy strat = (TotalSchedulingStrategy) loadClassByProperty(TOTAL_SCHEDULER_OPTION.propertyName);
        if (strat != null) {
            logger.config("Using total scheduling strategy defined by class " + strat.getClass().getName());
            configuredTaskScheduling = true;
        }
        totalSchedulingStrategy = strat;
    }

    static {
        TaskSchedulingStrategy strat = (TaskSchedulingStrategy) loadClassByProperty(TASK_SCHEDULER_STRATEGY_OPTION.propertyName);
        if (strat == null)
            strat = totalSchedulingStrategy;
        else
            configuredTaskScheduling = true;

        if (strat == null) {
            strat = new RandomTaskSchedulingStrategy(RANDOM);
        }

        logger.config("Using task scheduling strategy defined by class " + strat.getClass().getName());

        boolean recording = Boolean
                .parseBoolean(System.getProperty(RECORD_TASK_SCHEDULER_OPTION.propertyName, "false"));
        if (recording) {
            strat = new RecordingSchedulerStrategy(strat);
            logger.config("Recording schedule");
        }

        taskSchedulingStrategy = strat;
    }

    static {
        GlobalSchedulingStrategy globalSchedulerStrat = (GlobalSchedulingStrategy) loadClassByProperty(GLOBAL_SCHEDULER_OPTION.propertyName);
        if (globalSchedulerStrat == null)
            globalSchedulerStrat = totalSchedulingStrategy;

        if (globalSchedulerStrat != null) {
            logger.config("Using global scheduling strategy defined by class "
                    + globalSchedulerStrat.getClass().getName());
            globalScheduler = new GlobalScheduler(globalSchedulerStrat);
        } else {
            globalScheduler = null;
        }
    }

    static {
        taskSchedulerFactory = getSchedulerFactory();
    }

    private static TaskSchedulerFactory getSchedulerFactory() {
        // only the SimpleTaskScheduler supports configuring
        if (configuredTaskScheduling) {
            logger.config("Using simple task scheduler, because task scheduling is specified");
            return SimpleTaskScheduler.getFactory();
        }

        String schedulerName = System.getProperty(TASK_SCHEDULER_OPTION.propertyName, "default");
        logger.config("Scheduler: " + schedulerName);
        if (schedulerName.equals("default"))
            return DefaultTaskScheduler.getFactory();
        else if (schedulerName.equals("simple"))
            return SimpleTaskScheduler.getFactory();
        logger.warning("The task scheduler " + schedulerName
                + " does not exist, falling back to the default task scheduler.");
        return DefaultTaskScheduler.getFactory();
    }

    public static final boolean GLOBAL_SCHEDULING = globalScheduler != null;

}
