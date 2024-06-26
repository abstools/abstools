/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved.
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.abs_models.backend.java.lib.runtime;

import java.lang.reflect.Constructor;
import java.util.logging.Logger;

import org.abs_models.backend.java.debugging.GraphicalDebugger;
import org.abs_models.backend.java.observing.SystemObserver;
import org.abs_models.backend.java.scheduling.DefaultTaskScheduler;
import org.abs_models.backend.java.scheduling.GlobalSchedulingStrategy;
import org.abs_models.backend.java.scheduling.InteractiveScheduler;
import org.abs_models.backend.java.scheduling.RandomSchedulingStrategy;
import org.abs_models.backend.java.scheduling.RecordingSchedulerStrategy;
import org.abs_models.backend.java.scheduling.SchedulableTasksFilter;
import org.abs_models.backend.java.scheduling.SimpleTaskScheduler;
import org.abs_models.backend.java.scheduling.TaskSchedulerFactory;
import org.abs_models.backend.java.scheduling.TaskSchedulingStrategy;
import org.abs_models.backend.java.scheduling.TotalSchedulingStrategy;

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

    public static Config initRuntimeFromOptions(ABSRuntime runtime, RuntimeOptions options) {
        Config config = new Config(runtime, options);
        if (config.options.help.isTrue()) {
            config.printHelp();
            System.exit(1);
        }
        config.configureRuntime();
        return config;
    }

    private Config(ABSRuntime runtime, RuntimeOptions options) {
        this.runtime = runtime;
        this.options = options;
    }

    private void configureRuntime() {
        setSimpleOptions();
        loadSystemObserver();
        loadTotalSchedulingStrategy();
        loadTaskSchedulingStrategy();
        loadGlobalSchedulingStrategy();
        loadSchedulerFactory();
        loadSchedulableTasksFilter();
    }

    private void loadSchedulableTasksFilter() {
        if (options.schedulableTasksFilter.wasSet) {
            SchedulableTasksFilter filter = loadClassByName(SchedulableTasksFilter.class, options.schedulableTasksFilter.stringValue());;
            runtime.setSchedulableTasksFilter(filter);
        }
    }

    private void loadSystemObserver() {
        if (options.systemObserver.wasSet()) {
            for (String s : options.systemObserver.stringArrayValue()) {
                logger.finest(() -> "adding systemobserver " + s);
                runtime.addSystemObserver(loadClassByName(SystemObserver.class,s));
            }
        }
    }

    private void setSimpleOptions() {
        runtime.enableDebugging(options.debug.isTrue());
        runtime.terminateOnException(options.terminateOnException.isTrue());

        if (options.graphicalDebug.isTrue()) {
            runtime.enableDebugging(true);
            options.totalScheduler.setValue(InteractiveScheduler.class.getName());
            options.systemObserver.appendStringValue(GraphicalDebugger.class.getName());
        }

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

    public static String getVersion() {
        String version = Config.class.getPackage().getImplementationVersion();
        if (version == null)
            return "HEAD";
        else
            return version;
    }

    private void printHelp() {
        System.err.println(" ABS Java Backend - Runtime Configuration Options");
        System.err.println(" ================================================");
        System.err.println(" Toolchain version " + getVersion());
        System.err.println();
        for (RuntimeOptions.Option o : options.options) {
            if (o.type != RuntimeOptions.OptionType.BOOLEAN) {
                System.err.println(String.format("%36s\t%s",
                    String.join(", ", o.options)
                    + "=<" + o.type.toString().toLowerCase() + ">",
                    o.description));
            } else {
                System.err.println(String.format("%36s\t%s",
                    String.join(", ", o.options),
                    o.description));
            }
        }
    }

    private static <T> T loadClassByName(Class<T> expected, String s, Object...args) {
        try {
            Class<?> clazz = Config.class.getClassLoader().loadClass(s);
            if (!expected.isAssignableFrom(clazz)) {
                throw new IllegalArgumentException("Can't use "+s+" as an instance for "+expected.getCanonicalName());
            }
            for (Constructor<?> c : clazz.getConstructors()) {
                if (c.getParameterTypes().length == args.length) {
                    // Safe cast.
                    return (T) c.newInstance(args);
                }
            }
            throw new IllegalArgumentException("Couldn't find a constructor in class "+s+" with "+args.length+" arguments");
        } catch (Exception e) {
            e.printStackTrace();
        }
        return null;
    }

    private void loadTotalSchedulingStrategy() {
        if (options.totalScheduler.wasSet()) {
            TotalSchedulingStrategy strat = loadClassByName(TotalSchedulingStrategy.class, options.totalScheduler.stringValue(), runtime.getRandom());

            if (strat != null) {
                logger.config("Using total scheduling strategy defined by class " + strat.getClass().getName());
                runtime.setTotalSchedulingStrategy(strat);
            } else {
                logger.warning("Could not load total scheduler class "+options.totalScheduler.stringValue());
            }
        }
    }

    private void loadTaskSchedulingStrategy() {
        if (options.taskSchedulerStrategy.wasSet()) {
            TaskSchedulingStrategy strat = loadClassByName(TaskSchedulingStrategy.class, options.taskSchedulerStrategy.stringValue());
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

    private void loadGlobalSchedulingStrategy() {
        if (options.globalScheduler.wasSet()) {
            GlobalSchedulingStrategy strat = loadClassByName(GlobalSchedulingStrategy.class, options.globalScheduler.stringValue());
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
            if (schedulerName.equals("default")) {
                taskSchedulerFactory = DefaultTaskScheduler.getFactory();
            } else if (schedulerName.equals("simple")) {
                taskSchedulerFactory = SimpleTaskScheduler.getFactory();
            } else {
                logger.warning("The task scheduler " + schedulerName
                        + " does not exist, falling back to the default task scheduler.");
                taskSchedulerFactory = DefaultTaskScheduler.getFactory();
            }
            logger.config("Using " + schedulerName + " task scheduler");
        }
        runtime.setTaskSchedulerFactory(taskSchedulerFactory);
    }

}
