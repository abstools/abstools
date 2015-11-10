/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved.
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.backend.java.lib.runtime;

import java.io.BufferedReader;
import java.io.File;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.PrintStream;
import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.net.MalformedURLException;
import java.net.URL;
import java.net.URLClassLoader;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Properties;
import java.util.Random;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.logging.Logger;

import abs.backend.java.lib.types.ABSRef;
import abs.backend.java.observing.SystemObserver;
import abs.backend.java.scheduling.DefaultTaskScheduler;
import abs.backend.java.scheduling.GlobalScheduler;
import abs.backend.java.scheduling.GlobalSchedulingStrategy;
import abs.backend.java.scheduling.ScheduableTasksFilter;
import abs.backend.java.scheduling.ScheduableTasksFilterDefault;
import abs.backend.java.scheduling.ScheduleAction;
import abs.backend.java.scheduling.SimpleTaskScheduler;
import abs.backend.java.scheduling.TaskScheduler;
import abs.backend.java.scheduling.TaskSchedulerFactory;
import abs.backend.java.scheduling.TaskSchedulingStrategy;
import abs.backend.java.scheduling.TotalSchedulingStrategy;
import abs.backend.java.scheduling.UsesRandomSeed;

public class ABSRuntime {
    private static final String ABS_RUNSINOWNPROCESS_PROPERTY = "abs.runsinownprocess";

    private static final String FLI_CLASS_SUFFIX = "_fli";

    private static final String ABSFLI_PROPERTIES = "absfli.properties";

    private static final Logger logger = Logging.getLogger(ABSRuntime.class.getName());

    private static final boolean DEBUG_FLI = Boolean.parseBoolean(System.getProperty("abs.fli.debug", "false"));

    private final ABSThreadManager threadManager = new ABSThreadManager(this);
    private final AtomicInteger cogCounter = new AtomicInteger();
    private final AtomicInteger taskCounter = new AtomicInteger();

    /** classloader for loading the translated code and FLI classes */
    private ClassLoader classLoader = ABSRuntime.class.getClassLoader();


    /** URIs for loading foreign classes */
    private List<URL> classPath = new ArrayList<URL>();

    private PrintStream outStream = System.out;
    private PrintStream errStream = System.err;

    /** whether to output an error message when no
     * Java class is found for a class annotated with [Foreign] */
    private boolean ignoreMissingFLIClasses = false;

    private ScheduableTasksFilter scheduableTasksFilter = new ScheduableTasksFilterDefault();

    /**
     * Starts a new ABS program by giving a generated Main class
     * @param mainClass the Main class to be used
     * @throws InstantiationException if the Main class could not be instantiated
     * @throws IllegalAccessException if the Main class could not be accessed
     */
    public void start(Class<?> mainClass) throws InstantiationException, IllegalAccessException {
        systemStarted();
        COG cog = createCOG(mainClass, null);
        try {
            Constructor<?> constr = mainClass.getConstructor(COG.class);
            ABSObject mainObject = (ABSObject) constr.newInstance(cog);
            cogCreated(mainObject);
            asyncCall(new ABSMainCall(mainObject));
            doNextStep();
        } catch (SecurityException e) {
            e.printStackTrace();
        } catch (NoSuchMethodException e) {
            e.printStackTrace();
        } catch (IllegalArgumentException e) {
            e.printStackTrace();
        } catch (InvocationTargetException e) {
            e.printStackTrace();
        }
    }

    int freshTaskID() {
        return taskCounter.incrementAndGet();
    }

    int freshCOGID() {
        return cogCounter.incrementAndGet();
    }


    /**
     * Starts this runtime by using the Main class with name mainClassName (full qualified).
     * It uses the directory targetDir to search for that class.
     * Example: <code>start(new File("javatest"), "LeaderElection.Main");</code>

     * @param targetDir the directory that holds the generated class files
     * @param mainClassName the full qualified name of the class name.
     * @throws ClassNotFoundException
     * @throws IllegalAccessException
     * @throws InstantiationException
     */
    public void start(File targetDir, String mainClassName) throws ClassNotFoundException, InstantiationException, IllegalAccessException {
        if (!targetDir.canRead()) {
            throw new IllegalArgumentException("Directory "+targetDir+" cannot be read");
        }
        try {
            classPath.add(targetDir.toURI().toURL());
            URL[] urls = classPath.toArray(new URL[0]);
            classLoader = new URLClassLoader(urls, ABSRuntime.class.getClassLoader());
            Class<?> mainClass = classLoader.loadClass(mainClassName);
            start(mainClass);
        } catch (MalformedURLException e) {
            e.printStackTrace();
        }
    }

    private final List<SystemObserver> systemObserver = new ArrayList<SystemObserver>();

    private GlobalSchedulingStrategy globalSchedulingStrategy;
    private GlobalScheduler globalScheduler;
    private TaskSchedulingStrategy taskSchedulingStrategy;
    private TaskSchedulerFactory taskSchedulerFactory = DefaultTaskScheduler.getFactory();
    private volatile boolean debugging = false;
    private volatile boolean terminateOnException = false;
    private long randomSeed;
    private Random random;

    private volatile boolean isShutdown;


    public ABSRuntime() {
        setRandomSeed(System.nanoTime());
    }

    public void addSystemObserver(SystemObserver t) {
        this.systemObserver.add(t);
    }

    public synchronized long getRandomSeed() {
        return randomSeed;
    }

    public void enableDebugging(boolean debug) {
        debugging = debug;
    }

    public boolean debuggingEnabled() {
        return debugging;
    }

    /**
     * Terminate the whole ABS runtime when an exception occurs
     * when executing a task.
     * Default is false
     * @param b whether to terminate or not
     */
    public void terminateOnException(boolean b) {
        terminateOnException = b;
    }

    public synchronized void setRandomSeed(long seed) {
        randomSeed = seed;
        random = new Random(seed);
        logger.config("New Random Seed: " + randomSeed);

        if (globalSchedulingStrategy instanceof UsesRandomSeed) {
            ((UsesRandomSeed)globalSchedulingStrategy).setRandom(random);
        }

        if (taskSchedulingStrategy instanceof UsesRandomSeed) {
            ((UsesRandomSeed)taskSchedulingStrategy).setRandom(random);
        }

    }

    public synchronized Random getRandom() {
        return random;
    }

    public synchronized void setTotalSchedulingStrategy(TotalSchedulingStrategy strat) {
        setGlobalSchedulingStrategy(strat);
        setTaskSchedulingStrategy(strat);
    }

    public synchronized void setTaskSchedulingStrategy(TaskSchedulingStrategy strat) {
        taskSchedulingStrategy = strat;
        if (strat != null) {
            taskSchedulerFactory = SimpleTaskScheduler.getFactory();
        } else {
            taskSchedulerFactory = DefaultTaskScheduler.getFactory();
        }
    }

    public synchronized void setGlobalSchedulingStrategy(GlobalSchedulingStrategy strat) {
        globalSchedulingStrategy = strat;
        if (strat != null) {
            logger.config("Using global scheduling strategy defined by class "
                    + strat.getClass().getName());
            globalScheduler = new GlobalScheduler(this, strat);
        } else {
            globalScheduler = null;
        }

    }

    public synchronized void setTaskSchedulerFactory(TaskSchedulerFactory taskSchedulerFactory) {
        this.taskSchedulerFactory = taskSchedulerFactory;
    }

    public synchronized boolean hasGlobalScheduler() {
        return globalScheduler != null;
    }




    public void scheduleTaskDone() {
        if (hasGlobalScheduler()) {
            globalScheduler.doNextScheduleStep();
        }
    }

    public void nextStep(String fileName, int line) {
        if (isShutdown)
            return;

        if (debugging) {
            getCurrentTask().nextStep(fileName, line);
        }

        if (hasGlobalScheduler()) {
            try {
                globalScheduler.stepTask(getCurrentTask());
            } catch (InterruptedException e) {
                if (!isShutdown)
                    e.printStackTrace();
                else
                    throw new SystemTerminatedException();
            }
        }

    }

    public static <O> O checkForNull(O o) {
        if (o == null) {
            throw new ABSNullPointerException();
        }
        return o;
    }

    public void addScheduleAction(ScheduleAction action) {
        if (hasGlobalScheduler()) {
            globalScheduler.addAction(action);
        }
    }

    public void doNextStep() {
        if (!isShutdown && hasGlobalScheduler()) {
            globalScheduler.doNextScheduleStep();
        }
    }

    public void cogCreated(ABSObject o) {
        for (SystemObserver obs : systemObserver) {
            obs.newCOGCreated(o.getCOG().getView(), o.getView());
        }
    }

    public void systemStarted() {
        for (SystemObserver obs : systemObserver) {
             obs.systemStarted();
        }
    }

    public GlobalScheduler getGlobalScheduler() {
        return globalScheduler;
    }

    public void shutdown() {
        if (isShutdown)
            return;
        isShutdown = true;
        if (hasGlobalScheduler())
            globalScheduler.shutdown();
        threadManager.shutdownAllThreads();
    }

    public TaskScheduler createTaskScheduler(COG cog) {
        return taskSchedulerFactory.createTaskScheduler(this, cog, threadManager, scheduableTasksFilter);
    }

    public SimpleTaskScheduler createUserTaskScheduler(COG cog, TaskSchedulingStrategy strategy) {
        // Bypass the scheduler factory, as we specifically want a SimpleTaskScheduler
        return new SimpleTaskScheduler(cog, strategy, this, threadManager, scheduableTasksFilter);
    }

    public ABSThreadManager getThreadManager() {
        return threadManager;
    }

    public TaskSchedulingStrategy getTaskSchedulingStrategy() {
        return taskSchedulingStrategy;
    }


    public static ABSRuntime getCurrentRuntime() {
        if (getCurrentCOG() != null) {
            return getCurrentCOG().getRuntime();
        } else {
            return null;
        }
    }

    public static Task<?> getCurrentTask() {
        if (getCurrentCOG() != null) {
            return getCurrentCOG().getScheduler().getActiveTask();
        } else {
            return null;
        }
    }

    public static void suspend() {
        getCurrentCOG().getScheduler().await(new ABSTrueGuard());
    }

    public static void await(ABSGuard g) {
        if (g.isTrue()) return; // special case in the semantics
        getCurrentCOG().getScheduler().await(g);
    }

    public COG createCOG(Class<?> clazz, abs.backend.java.lib.types.ABSInterface dc) {
        return new COG(this, clazz, dc);
    }

    public COG createCOG(Class<?> clazz, abs.backend.java.lib.types.ABSInterface dc, TaskSchedulingStrategy strategy) {
        return new COG(this, clazz, dc, strategy);
    }

    public static COG getCurrentCOG() {
        final ABSThread thread = getCurrentThread();
        if (thread != null)
            return thread.getCOG();
        else
            return null;
    }

    public static ABSThread getCurrentThread() {
        Thread currentThread = Thread.currentThread();
        if (currentThread instanceof ABSThread)
            return (ABSThread) currentThread;
        else
            return null;
    }

    public <T extends ABSRef> ABSFut<?> asyncCall(AsyncCall<T> call) {
        Task<T> task = new Task<T>(call);
        task.schedule();
        return task.getFut();
    }

    private final Map<Class<?>,AtomicInteger> objectIds = new HashMap<Class<?>,AtomicInteger>();
    public long getFreshObjectID(Class<?> clazz) {
        AtomicInteger ai = null;
        synchronized (objectIds) {
            ai = objectIds.get(clazz);
            if (ai == null) {
                ai = new AtomicInteger();
                objectIds.put(clazz, ai);
            }
            return ai.incrementAndGet();
        }
    }

    public void systemFinished() {
        for (SystemObserver obs : systemObserver) {
            obs.systemFinished();
        }
    }

    public void setForeignClass(String absClassName, Class<?> javaClass) {
        foreignClasses.put(absClassName, javaClass);
    }

    /**
     * Defines the properties to be used for the foreign class mapping
     * @param the properties to be used
     */
    public void setFLIProperties(Properties p) {
        fliProperties = p;
    }

    /**
     * Maps ABS class names of foreign classes to Java class implementations
     */
    private final Map<String, Class<?>> foreignClasses = new HashMap<String, Class<?>>();

    private Properties fliProperties;
    private synchronized Class<?> getForeignClass(String name) {
        Class<?> clazz = foreignClasses.get(name);

        // this is a dummy entry to prevent reloading of classes each time
        if (clazz == ABSRuntime.class)
            return null;

        if (clazz == null) {
            clazz = loadForeignClass(name);
            if (clazz != null) {
                foreignClasses.put(name, clazz);
            } else {
                foreignClasses.put(name, ABSRuntime.class);
            }

            if (DEBUG_FLI)
                errStream.println("FLI: "+name+" = "+clazz);

        }
        return clazz;
    }

    private synchronized Class<?> loadForeignClass(String name) {
        String className = System.getProperty("abs.fli.class."+name);
        if (className == null) {
            if (fliProperties == null) {
                loadFLIProperties();
            }
            className = fliProperties.getProperty(name);
        }

        if (className == null) {
            // use conventions:
            className = name+FLI_CLASS_SUFFIX;
        }

        try {
            Class<?> result = classLoader.loadClass(className);
            return result;
        } catch (ClassNotFoundException e) {
            if (!ignoreMissingFLIClasses) {
                errStream.println("Could not load foreign class for " + name + "!");
            }
        }
        return null;
     }

    private void loadFLIProperties() {
        String propertiesFileName = System.getProperty("abs.fli.properties",ABSFLI_PROPERTIES);
        URL url = classLoader.getResource(propertiesFileName);
        fliProperties = new Properties();
        if (url == null) {
            if (!propertiesFileName.equals(ABSFLI_PROPERTIES)) {
                errStream.println("Could not find ABS-FLI properties file "+propertiesFileName);
            }
        } else {
            try {
                fliProperties.load(new BufferedReader(
                        new InputStreamReader(url.openStream())));

                if (DEBUG_FLI) {
                    errStream.println("FLI: Loaded properties from URL "+url);
                    errStream.println("FLI: Loaded properties: "+fliProperties.toString());
                }

            } catch (IOException e) {
                if (!propertiesFileName.equals(ABSFLI_PROPERTIES)) {
                    errStream.println("ABS Error while trying to read the FLI properties file "+propertiesFileName);
                    e.printStackTrace();
                }
            }
        }
    }

    public Object getForeignObject(String absClassName, Object... args) {
        return loadForeignObject(getForeignClass(absClassName), args);
    }

    private Object loadForeignObject(Class<?> foreignClass, Object... args) {
        if (foreignClass != null) {
           try {
              Constructor<?>[] cs = foreignClass.getConstructors();
              if (cs.length == 0) return foreignClass.newInstance();
              return cs[0].newInstance(args);
           } catch (InstantiationException e) {
              e.printStackTrace();
           } catch (IllegalAccessException e) {
              e.printStackTrace();
           } catch (IllegalArgumentException e) {
              e.printStackTrace();
           } catch (InvocationTargetException e) {
              e.printStackTrace();
           }
        }

        return null;
     }

    /**
     * Defines what to do in case an ABSException is thrown in a task
     * @param task the task that throwed the exception
     * @param e the exception that was thrown
     */
    public void handleABSException(Task<?> task, ABSException e) {
        if (isShutdown) {
            // ignore errors during shutdown
            return;
        }
        errStream.println("Error in " + this + ":\n" + e.getMessage());
        for (SystemObserver obs : systemObserver) {
            obs.systemError(e);
        }
        if (terminateOnException) {
            shutdown();
        }
    }

    public static boolean runsInOwnProcess() {
        return Boolean.parseBoolean(System.getProperty(ABS_RUNSINOWNPROCESS_PROPERTY, "false"));
    }

    public static void setRunsInOwnProcess(boolean b) {
        System.setProperty(ABS_RUNSINOWNPROCESS_PROPERTY, Boolean.toString(b));
    }

    public void addFLIClassPath(Collection<URL> fliClassPath) {
        classPath.addAll(fliClassPath);
    }

    public void setOutStream(PrintStream stream) {
        outStream = stream;
    }

    public PrintStream getOutStream() {
        return  outStream;
    }

    public PrintStream getErrStream() {
        return  errStream;
    }

    public void setErrStream(PrintStream stream) {
        this.errStream = stream;
    }

    public void setIgnoreMissingFLIClasses(boolean ignoreMissingFLIClasses) {
        this.ignoreMissingFLIClasses = ignoreMissingFLIClasses;
    }

    public void setScheduableTasksFilter(ScheduableTasksFilter filter) {
        scheduableTasksFilter = filter;
    }

    public boolean getTerminateOnException() {
        return terminateOnException;
    }

}
