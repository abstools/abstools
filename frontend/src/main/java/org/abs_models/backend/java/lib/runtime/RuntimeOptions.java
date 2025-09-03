/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved.
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.abs_models.backend.java.lib.runtime;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.ListIterator;
import java.util.Set;

import static org.abs_models.backend.java.lib.runtime.RuntimeOptions.OptionType.*;

public class RuntimeOptions {
    static enum OptionType {
        BOOLEAN, STRING, CLASSLIST, CLASS, LONG;

        Object parseValue(String s) {
                switch (this) {
                    case BOOLEAN: {
                            if (s.equals("true")) return Boolean.TRUE;
                            else if (s.equals("false")) return Boolean.FALSE;
                            else return null;
                        }
                    case STRING: return s;
                    case CLASSLIST: return s.split(",");
                    case CLASS: return s;
                    case LONG: {
                            try {
                                return Long.parseLong(s);
                            } catch (NumberFormatException e) {
                                return null;
                            }
                        }
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
        /**
         * Try to parse the option obtained by calling {@code
         * arglist.next()}.  If unsuccessful, call {@code
         * arglist.previous()}, i.e., reset the cursor.  Otherwise,
         * remove option from {@code arglist}.  If a parameter is
         * required, (try to) read the next string from {@code
         * arglist} as well, and remove it if successful.
         *
         * @param arglist the list of all arguments, with iterator at
         *  the position that should be read.
         * @return true if option and its parameter (if applicable)
         *  could be read; false otherwise.
         */
        // This is a long method, but on the other hand we avoid
        // integrating some third-party parameter parsing library into
        // the compiled model.
        boolean parseCommandLineArg(ListIterator<String> arglist) {
            String option = arglist.next();
            if (!options.contains(option)) {
                // not us; put it back
                arglist.previous();
                return false;
            } else {
                if (type == BOOLEAN) {
                    if (!arglist.hasNext()) {
                        setValue(Boolean.TRUE);
                        // remove option
                        arglist.remove();
                        return true;
                    } else {
                        String argument = arglist.next();
                        Object value = type.parseValue(argument);
                        if (value == null) {
                            // wasn't a boolean, put it back
                            arglist.previous();
                            // move back to option and remove it
                            arglist.previous(); arglist.remove();
                            setValue(Boolean.TRUE);
                            return true;
                        } else {
                            // was a boolean, remove it
                            arglist.remove();
                            // move back to option and remove it too
                            arglist.previous(); arglist.remove();
                            setValue(value);
                            return true;
                        }
                    }
                } else {
                    // Not a Boolean, so the argument is mandatory
                    if (!arglist.hasNext()) {
                        // Note that we do not call `arglist.remove()`
                        // here since we want the "leftover" option in
                        // the error message
                        return false;
                    } else {
                        String argument = arglist.next();
                        Object value = type.parseValue(argument);
                        if (value != null) {
                            arglist.remove();
                            // move back to option and remove it too
                            arglist.previous(); arglist.remove();
                            setValue(value);
                            return true;
                        } else {
                            // parse failure; put not-argument back
                            arglist.previous();
                            // Note that we do not call `arglist.remove()`
                            // here since we want the "leftover" option in
                            // the error message
                            return false;
                        }
                    }
                }
            }
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

        public Long longValue() {
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
    // We only call getLongValue() after checking wasSet(), so we get
    // away with a `null` default value
    public final Option modelapiPort =
        addOption(LONG, "modelapiPort", Set.of("-p", "--port"), "turns on the Model API on the given port", null);
    public final Option clockLimit =
        addOption(LONG, "timelimit", Set.of("-l", "--clock-limit"), "set the limit for the model clock", null);
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
        addOption(STRING, "loglevel", "--loglevel", "sets the logging level (severe, warning, info, config, fine, finer, finest)", "warning");
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
        if (args == null) return;
        ArrayList<String> arguments = new ArrayList<>(Arrays.asList(args));
        ListIterator<String> argslist = arguments.listIterator();
        ArrayList<Option> remainingOptions = new ArrayList<>(options);

        while (argslist.hasNext()) {
            boolean success = false;
            for (Option o : remainingOptions) {
                if (o.parseCommandLineArg(argslist)) {
                    success = true;
                    // If we remove the next line, "last option wins";
                    // otherwise, duplicate options are ignored
                    remainingOptions.remove(o);
                    break;
                } else if (!argslist.hasNext()) {
                    // Corner case: missing argument for the last option
                    break;
                }
            }
            if (!success && argslist.hasNext()) {
                argslist.next();
            }
        }
        if (arguments.size() > 0) {
            System.err.print("The following arguments could not be parsed:");
            arguments.forEach(s -> System.err.print(" " + s));
            System.err.println();
            System.exit(1);
        }
    }
}
