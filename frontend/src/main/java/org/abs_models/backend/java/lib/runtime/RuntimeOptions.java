/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved.
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.abs_models.backend.java.lib.runtime;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Set;

import static org.abs_models.backend.java.lib.runtime.RuntimeOptions.OptionType.*;

public class RuntimeOptions {
    static enum OptionType {
        BOOLEAN, STRING, CLASSLIST, CLASS, LONG;

        Object parseValue(String s) {
            switch (this) {
            case BOOLEAN: return Boolean.parseBoolean(s);
            case STRING: return s;
            case CLASSLIST: return s.split(",");
            case CLASS: return s;
            case LONG: return Long.parseLong(s);
            }
            return null;
        }

    }

    static class Option {
        final String propertyName;
        final Set<String> options;
        final String description;
        Object value;
        final OptionType type;
        boolean wasSet;
        Option(OptionType type, String propertyName, Set<String> options, String description, Object defaultValue) {
            this.propertyName = propertyName;
            this.type = type;
            this.options = options;
            this.description = description;
            this.value = defaultValue;
        }
        Option(OptionType type, String propertyName, String option, String description, Object defaultValue) {
            this(type, propertyName, Set.of(option), description, defaultValue);
        }
        boolean parseCommandLineArg(String arg) {
            if (options.contains(arg)) {
                // boolean argument
                setValue(true);
            } else if (options.stream().anyMatch((name) -> arg.startsWith(name + "="))) {
                // argument with value
                setValue(type.parseValue(arg.substring(arg.indexOf("=") + 1)));
            } else {
                return false;
            }
            return true;
        }

        void setValue(Object o) {
            value = o;
            wasSet = true;
        }
        void readSystemProperty() {
            String s = System.getProperty("abs." + propertyName);
            if (s != null) {
                setValue(type.parseValue(s));
            }
        }

        public boolean wasSet() {
            return wasSet;
        }

        public boolean isTrue() {
            return (Boolean) value;
        }

        public String stringValue() {
            return (String) value;
        }

        public String[] stringArrayValue() {
            return (String[]) value;
        }

        public long longValue() {
            return (Long) value;
        }
        public void appendStringValue(String name) {
            String[] v = stringArrayValue();
            v = Arrays.copyOf(v, v.length+1);
            v[v.length-1] = name;
            setValue(v);
        }

    }

    final List<Option> options = new ArrayList<>();

    public final Option help =
        addOption(BOOLEAN, "help", Set.of("-h", "--help"), "shows help information", false);
    public final Option useNet =
        addOption(BOOLEAN, "net", "--net", "enables the use of ABS-NET", false);
    public final Option debug =
        addOption(BOOLEAN, "debug", "--debug", "enables debugging", false);
    public final Option graphicalDebug =
        addOption(BOOLEAN, "gdebug", "--gdebug", "starts the graphical debugger", false);
    public final Option terminateOnException =
        addOption(BOOLEAN, "terminateOnException", "--terminateOnException", "terminates the system when an exception occurs", false);
    public final Option systemObserver =
        addOption(CLASSLIST, "systemobserver", "--systemobserver", "comma-separated list of system observer classes", new String[0]);
    public final Option totalScheduler =
        addOption(CLASS, "totalscheduler", "--totalscheduler", "sets a total scheduler class", null);
    public final Option globalScheduler =
        addOption(CLASS, "globalscheduler", "--globalscheduler", "sets a global scheduler class", null);
    public final Option taskSchedulerStrategy =
        addOption(CLASS, "taskschedulerstrategy", "--taskschedulerstrategy", "sets a task scheduler strategy class", null);
    public final Option taskScheduler =
        addOption(STRING, "taskscheduler", "--taskscheduler", "sets the task scheduler to be used", "default");
    public final Option recordTaskScheduler =
        addOption(BOOLEAN, "recordtaskscheduler", "--recordtaskscheduler", "enables recording of task scheduling", false);
    public final Option randomSeed =
        addOption(LONG, "randomseed", "--randomseed", "set the random seed used by schedulers", System.nanoTime());
    public final Option useRandomScheduler =
        addOption(BOOLEAN, "useRandomScheduler", "--useRandomScheduler", "sets the random scheduler as the total scheduler", false);
    public final Option printRandomSeed =
        addOption(BOOLEAN, "printrandomseed", "--printrandomseed", "prints that used random seed to stdout", false);
    public final Option logLevel =
        addOption(STRING, "loglevel", "--loglevel", "sets the logging level", "warning");
    public final Option loggedClasses =
        addOption(CLASSLIST, "loggedClasses", "--loggedClasses", "comma-separated list of classes to be logged", new String[0]);
    public final Option schedulableTasksFilter =
        addOption(CLASS, "schedulableTasksFilter", "--schedulableTasksFilter", "sets a filter class for schedulable tasks", null);
    public final Option dynamicUpdates =
        addOption(BOOLEAN, "dynamic", "--dynamic", "enables dynamic program updates (not supported)", false);

    public RuntimeOptions(String[] args) {
        evaluateSystemProperties();
        parseCommandLineArgs(args);
    }

    private void evaluateSystemProperties() {
        for (Option o : options) {
            o.readSystemProperty();
        }
    }

    private Option addOption(OptionType b, String name, String option, String description, Object defaultValue) {
        Option o = new Option(b, name, option, description, defaultValue);
        this.options.add(o);
        return o;
    }

    private Option addOption(OptionType b, String name, Set<String> options, String description, Object defaultValue) {
        Option o = new Option(b, name, options, description, defaultValue);
        this.options.add(o);
        return o;
    }

    void parseCommandLineArgs(String[] args) {
        List<Option> configuredOptions = new ArrayList<>(options);

        for (String arg : args) {
            Option usedOption = null;
            for (Option o : configuredOptions) {
                if (o.parseCommandLineArg(arg)) {
                    usedOption = o;
                    break;
                }
            }
            if (usedOption != null)
                configuredOptions.remove(usedOption);

        }
    }
}
