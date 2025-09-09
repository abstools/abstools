/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved.
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.abs_models.backend.java.lib.runtime;

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
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.PriorityQueue;
import java.util.Properties;
import java.util.Random;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.logging.Logger;

import org.abs_models.backend.java.lib.types.ABSInterface;
import org.abs_models.backend.java.lib.types.ABSRef;
import org.abs_models.backend.java.observing.SystemObserver;
import org.abs_models.backend.java.scheduling.DefaultTaskScheduler;
import org.abs_models.backend.java.scheduling.GlobalScheduler;
import org.abs_models.backend.java.scheduling.GlobalSchedulingStrategy;
import org.abs_models.backend.java.scheduling.SchedulableTasksFilter;
import org.abs_models.backend.java.scheduling.SchedulableTasksFilterDefault;
import org.abs_models.backend.java.scheduling.ScheduleAction;
import org.abs_models.backend.java.scheduling.SimpleTaskScheduler;
import org.abs_models.backend.java.scheduling.TaskScheduler;
import org.abs_models.backend.java.scheduling.TaskSchedulerFactory;
import org.abs_models.backend.java.scheduling.TaskSchedulingStrategy;
import org.abs_models.backend.java.scheduling.TotalSchedulingStrategy;
import org.abs_models.backend.java.scheduling.UsesRandomSeed;
import org.apfloat.Apint;
import org.apfloat.Aprational;
import org.apfloat.AprationalMath;

/**
 * The singleton runtime class.
 * <p>
 * The single instance of this class or one of its subclasses is generated in
 * {@link StartUp#startup}.
 */
public class ABSRuntime {
    private static final String ABS_RUNSINOWNPROCESS_PROPERTY = "abs.runsinownprocess";

    private static final String FLI_CLASS_SUFFIX = "_fli";

    private static final String ABSFLI_PROPERTIES = "absfli.properties";

    private static final Logger log = Logging.getLogger(ABSRuntime.class.getName());

    private static final boolean DEBUG_FLI = Boolean.parseBoolean(System.getProperty("abs.fli.debug", "false"));

    /**
     * The singleton runtime instance, set by {@see #getRuntime}.  The value in
     * this field can be of class {@code ABSRuntime} or a subclass thereof, depending
     * on which class's {@code getRuntime} method is called.
     */
    protected static ABSRuntime runtimeSingleton = null;
    private final ABSThreadManager threadManager = new ABSThreadManager(this);
    private final AtomicInteger cogCounter = new AtomicInteger();
    private final AtomicInteger taskCounter = new AtomicInteger();

    /** The number of currently active cogs in the system. */
    private long nActiveCogs = 0;

    /** The number of cogs that need to wake up before the next clock advance. */
    private long nWakingCogs = 0;

    /**
     * The current clock value.
     */
    private Aprational clock = new Aprational(0);
    /**
     * The current time to advance to, as needed by the currently waiting
     * duration guards. This is the minimum of the maxima of all guards in the
     * {@see #duration_guards} queue and is calculated every time we add a
     * duration guard to the queue, or increment the clock.  Its value is only
     * meaningful when {@code duration_guards} is non-empty.
     * <p>
     * Note that the clock might be incremented to less than this time point
     * if there are also resource guards waiting.
     * <p>
     * NOTE: For safe access to this field, use {@ocde
     * synchronized(duration_guards)}.
     */
    private Aprational wake_time_for_duration_guards = new Aprational(0);
    /**
     * Queue of all guards waiting on the clock, ordered by minimum duration.
     * This means when advancing the clock, we can pop elements in order until
     * the first element's minimum wakeup time is greater than the current
     * clock.
     * <p>
     * NOTE: Protect all access to this field with {@code synchronized(duration_guards)}.
     */
    private final PriorityQueue<ABSDurationGuard> duration_guards
        = new PriorityQueue<>(Comparator.comparing(ABSDurationGuard::getMinTime));

    /**
     * All deployment components in the system.  Upon time advance, we need to
     * update resources and resource histories for all DCs, so we keep them
     * referenced here.
     */
    private final List<ABSDCMirror> deployment_components = new ArrayList<>();

    public List<ABSDCMirror> getDeploymentComponents() { return deployment_components; }

    /**
     * Queue of all pending resource requests.
     *
     * TODO: figure out locking scheme for this; it will be used from both
     * advanceClock and addResourceGuard.
     */
    private final Map<ABSInterface, List<ABSResourceGuard>> resource_guards
        = new HashMap<>();

    /**
     * classloader for loading the translated code and FLI classes
     */
    private ClassLoader classLoader = ABSRuntime.class.getClassLoader();


    /**
     * URIs for loading foreign classes
     */
    private final List<URL> classPath = new ArrayList<>();

    private PrintStream outStream = System.out;
    private PrintStream errStream = System.err;

    /** whether to output an error message when no Java class is found
     * for a class annotated with [Foreign] */
    private boolean ignoreMissingFLIClasses = false;

    private SchedulableTasksFilter schedulableTasksFilter = new SchedulableTasksFilterDefault();

    /** The port to run the Model API on, or {@code null} if API
     * should not be started. */
    private Long modelApiPort = null;

    /**
     * The clock time limit, as given initially by the command line,
     * or {@code null} if time should advance without limit.
     */
    private Apint clockLimit = null;

    /**
     * The directory that contains data files such as sqlite files;
     * defaults to the VM's notion of current directory.  Settable via
     * the {@code --datadir} CLI option or the {@code abs.datadir}
     * system property.
     */
    private Path datadir = Paths.get("").toAbsolutePath();

    /**
     * Starts a new ABS program by giving a generated Main class
     * @param mainClass the Main class to be used
     * @throws InstantiationException if the Main class could not be instantiated
     * @throws IllegalAccessException if the Main class could not be accessed
     * @throws IOException if the Model API could not be started
     */
    public void start(Class<?> mainClass) throws InstantiationException, IllegalAccessException, IOException {
        if (modelApiPort != null) {
            ModelApi.startModelApi(modelApiPort.intValue());
        }
        systemStarted();
        COG cog = createCOG(mainClass, null);
        try {
            Constructor<?> constr = mainClass.getConstructor(COG.class);
            ABSObject mainObject = (ABSObject) constr.newInstance(cog);
            cogCreated(mainObject);
            // Note that here, the DC of `cog` is still null; we set up the
            // global DC from the generated code--its constructor needs to run
            // inside an ABS task since it calls `getCurrentCog()`.
            log.fine("Starting Main block.");
            asyncCall(new ABSMainCall(mainObject));
            doNextStep();
        } catch (SecurityException | NoSuchMethodException | IllegalArgumentException | InvocationTargetException e) {
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
     * @throws IOException if Model API could not be started
     */
    public void start(File targetDir, String mainClassName) throws ClassNotFoundException, InstantiationException, IllegalAccessException, IOException {
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

    private final List<SystemObserver> systemObserver = new ArrayList<>();

    private GlobalSchedulingStrategy globalSchedulingStrategy;
    private GlobalScheduler globalScheduler;
    private TaskSchedulingStrategy taskSchedulingStrategy;
    private TaskSchedulerFactory taskSchedulerFactory = DefaultTaskScheduler.getFactory();
    private volatile boolean debugging = false;
    private volatile boolean terminateOnException = false;
    private long randomSeed;
    private Random random;

    private volatile boolean isShutdown;

    protected ABSRuntime() {
        setRandomSeed(System.nanoTime());
    }

    /**
     * Get the singleton runtime.  Note that subclasses can override this
     * method to instantiate a subclass instead; that object will then be
     * returned by all calls to this method, regardless on which class.
     * Therefore, the first call to this method should be in a place that can
     * decide which class the global runtime should be, typically {@link
     * StartUp#startup}.
     *
     * @return the runtime singleton
     */
    public static ABSRuntime getRuntime() {
        if (runtimeSingleton == null) {
            synchronized(ABSRuntime.class) {
                if (runtimeSingleton == null) {
                    runtimeSingleton = new ABSRuntime();
                }
            }
        }
        return runtimeSingleton;
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
     * Use {@code newDatadir} as the directory to resolve relative
     * paths named in ABS programs.  Does nothing if {@code
     * newDatadir} does not name an existing directory.
     */
    public void setDatadir(String newDatadir) {
        Path newPath = Path.of(newDatadir).toAbsolutePath().normalize();
        if (newPath.toFile().isDirectory()) {
            datadir = newPath;
        } else {
            log.warning("The value " + newDatadir + "is not a directory; ignoring");
        }
    }

    /**
     * Return the directory used to resolve relative paths named in
     * ABS programs.
     */
    public Path getDatadir() {
        return datadir;
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
        log.config("New Random Seed: " + randomSeed);

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
            log.config("Using global scheduling strategy defined by class "
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
            ABSThread.getCurrentTask().nextStep(fileName, line);
        }

        if (hasGlobalScheduler()) {
            try {
                globalScheduler.stepTask(ABSThread.getCurrentTask());
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
        return taskSchedulerFactory.createTaskScheduler(this, cog, threadManager, schedulableTasksFilter);
    }

    public SimpleTaskScheduler createUserTaskScheduler(COG cog, TaskSchedulingStrategy strategy) {
        // Bypass the scheduler factory, as we specifically want a SimpleTaskScheduler
        return new SimpleTaskScheduler(cog, strategy, this, threadManager, schedulableTasksFilter);
    }

    public ABSThreadManager getThreadManager() {
        return threadManager;
    }

    public TaskSchedulingStrategy getTaskSchedulingStrategy() {
        return taskSchedulingStrategy;
    }

    public static void suspend() {
        ABSThread.getCurrentCOG().getScheduler().await(new ABSTrueGuard());
    }

    public static void await(ABSGuard g) {
        if (g.isTrue()) return; // special case in the semantics
        ABSThread.getCurrentCOG().getScheduler().await(g);
    }

    public COG createCOG(Class<?> clazz, ABSInterface dc) {
        // NOTE: we don't increase `nActiveCogs` here, since the running task
        // (who created the fresh object and cog) will schedule an
        // initialization task immediately.
        return new COG(this, clazz, dc);
    }

    public COG createCOG(Class<?> clazz, ABSInterface dc, TaskSchedulingStrategy strategy) {
        // NOTE: we don't increase `nActiveCogs` here, since the running task
        // (who created the fresh object and cog) will schedule an
        // initialization task immediately.
        return new COG(this, clazz, dc, strategy);
    }

    public <T extends ABSRef> ABSFut<?> asyncCall(AsyncCall<T> call) {
        Task<T> task = new Task<>(call);
        task.schedule();
        log.fine(() -> "Asynccall " + call.methodName() + " from " + call.getSource() + " to " + call.getTarget() + " (" + task + ")");
        return task.getFut();
    }

    private final Map<Class<?>,AtomicInteger> objectIds = new HashMap<>();
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
     * @param p the properties to be used
     */
    public void setFLIProperties(Properties p) {
        fliProperties = p;
    }

    /**
     * Maps ABS class names of foreign classes to Java class implementations
     */
    private final Map<String, Class<?>> foreignClasses = new HashMap<>();

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
            return classLoader.loadClass(className);
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
            try (BufferedReader r = new BufferedReader(new InputStreamReader(url.openStream()))) {
                fliProperties.load(r);

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
              if (cs.length == 0) return foreignClass.getDeclaredConstructor().newInstance();
              return cs[0].newInstance(args);
           } catch (InstantiationException
               | IllegalAccessException
               | InvocationTargetException
               | IllegalArgumentException
               | NoSuchMethodException e) {
              e.printStackTrace();
           }
        }

        return null;
     }

    /**
     * Defines what to do in case an ABSException is thrown in a task
     * @param task the task that threw the exception
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

    public void setSchedulableTasksFilter(SchedulableTasksFilter filter) {
        schedulableTasksFilter = filter;
    }

    public void setModelApiPort(Long modelApiPort) {
        this.modelApiPort = modelApiPort;
    }

    public boolean getTerminateOnException() {
        return terminateOnException;
    }

    public synchronized void registerDC(ABSInterface dc) {
        deployment_components.add(new ABSDCMirror(dc));
    }

    /**
     * Return the current value of the global clock.  This is an absolute time,
     * increasing from 0.
     */
    public Aprational getClock() {
        return clock;
    }

    public Apint getClockLimit() {
        return clockLimit;
    }

    public void initializeClockLimit(Long amount) {
        if (amount != null) clockLimit = new Apint(amount);
    }

    /**
     * Add to current clock limit, if non-null, and return new limit.
     */
    public Long addToClockLimit(Long amount) {
        if (clockLimit == null) return null;
        log.finest(() -> "Adding to clock limit: " + amount);
        synchronized(this) {
            // Note: we can do exact comparison here, since the clock
            // will always jump to the nearest integer since
            // deployment components update on integer boundaries.
            boolean wasAtLimit = clock.compareTo(clockLimit) == 0;
            clockLimit = clockLimit.add(new Apint(amount));
            if (wasAtLimit && nActiveCogs == 0) {
                log.finest(() -> "Clock limit increased and no active cogs, trying to advance clock.");
                maybeAdvanceClock();
            }
        }
        return clockLimit.toBigInteger().longValue();
    }

    /**
     * Pass resources from DCs to waiting resource guards, and wake up tasks
     * if their resource requests are fulfilled.  Remove entries from the
     * {@code resource_guards} map if no guards are left waiting for the
     * specified DC.
     * <p>
     * NOTE: this method is supposed to be called during clock advance only.
     *
     * @return the number of guards woken
     */
    protected synchronized int handResourcesToWaitingGuards() {
        if (resource_guards.isEmpty()) {
            log.finest("No tasks waiting for resources");
            return 0;
        }
        int guardsWoken = 0;
        log.finest("Handing out resources to waiting tasks");
        var iterator = resource_guards.entrySet().iterator();
        while (iterator.hasNext()) {
            // we iterate through (dc, guards), so this loop is executed once
            // per dc that has resource requests.  If a dc fulfills all
            // requests, we remove its entry from `resource_guards`.
            Map.Entry<ABSInterface, List<ABSResourceGuard>> entry = iterator.next();
            var dc = new ABSDCMirror(entry.getKey());
            var guards = entry.getValue();
            log.finest(() -> "Processing " + dc.getWrappedDC() + " with " + guards.size() + " waiting guards");
            guard_loop:
            while (!guards.isEmpty()) {
                ABSResourceGuard guard = guards.get(0);
                Aprational needed = guard.getResourcesNeeded();
                Aprational consumed = dc.consumeCPU(needed);
                if (consumed.signum() == 0) {
                    // The DC ran out of resources; don't try to give
                    // any to the next entry
                    break guard_loop;
                }
                guard.consumeResources(consumed);
                log.finest(() -> guard + " consumed " + consumed + " of " + needed + " resources from DC " + dc.getWrappedDC() + "; guard is finished: " + guard.isTrue());
                if (guard.isTrue()) {
                    guards.remove(0);
                    synchronized(guard) {
                        guard.notify();
                    }
                    guardsWoken = guardsWoken + 1;
                } else {
                    // We got some resources but not everything the
                    // current guard wanted; next call to
                    // `dc.consumeCPU` will return consumed==0 so we
                    // might as well exit the loop here already
                    break guard_loop;
                }
            }
            if (guards.isEmpty()) {
                log.finest(() -> dc.getWrappedDC() + " has no more guards waiting; removing from to-do list");
                iterator.remove();
            }
        }
        return guardsWoken;
    }

    /**
     * Hand out resources and optionally advance the clock until one or more
     * processes waiting for either the clock or some resource become
     * runnable.  This method must only be called when all threads are
     * waiting, and updates the resource amounts and histories of all
     * deployment components as the clock advances.
     */
    protected synchronized void maybeAdvanceClock() {
        // We do not try to calculate the clock increase in advance, because
        // the time when a task becomes active depends on resource allocation
        // strategies when two or more tasks try to get resources from the
        // same deployment component.  Instead, we increment the clock to
        // where either a duration guard wakes up or a resource boundary
        // occurs (and hence, a resource guard might receive enough resources
        // to unblock), whichever comes earlier.
        if (nWakingCogs > 0) {
            log.finest(() -> "Not advancing clock before " + nWakingCogs + " more cogs have woken up");
            return;
        }
        if (duration_guards.isEmpty() && resource_guards.isEmpty()) {
            log.finest("Trying to advance the clock but no task is waiting for a duration or resource, exiting");
            return;
        }
        log.finest(() -> "Starting resource allocation and clock advance: clock = "
                         + clock + ", clockLimit = " + (clockLimit == null ? "none" : clockLimit)
                         + ", durationGuards: " + duration_guards.size()
                         + ", resourceGuards: " + resource_guards.size());
        int guardsWoken = handResourcesToWaitingGuards();
        while (guardsWoken == 0 && (clockLimit != null ? clock.compareTo(clockLimit) < 0 : true)) {
            int durationGuardsWoken = 0;
            int resourceGuardsWoken = 0;
            Aprational next_integer = clock.isInteger()
                ? clock.add(Aprational.ONE)
                : clock.ceil();
            if (duration_guards.isEmpty()) {
                // If no duration guards are waiting, do not consider
                // `wake_time_for_duration_guards`
                clock = next_integer;
            } else {
                clock = AprationalMath.min(wake_time_for_duration_guards, next_integer);
            }
            log.fine(() -> "Clock advanced to " + clock);
            if (clock.compareTo(next_integer) == 0) {
                deployment_components.forEach(ABSDCMirror::advanceTimeBy1Tick);
                resourceGuardsWoken = resourceGuardsWoken + handResourcesToWaitingGuards();
            }
            log.finest("Checking for threads to wake that are waiting on duration guards");
            while (!duration_guards.isEmpty() && clock.compareTo(duration_guards.peek().getMinTime()) >= 0) {
                ABSDurationGuard guard = duration_guards.remove();
                synchronized(guard) {
                    guard.notify();
                }
                durationGuardsWoken = durationGuardsWoken + 1;
            }
            if (durationGuardsWoken > 0 && !duration_guards.isEmpty()) {
                wake_time_for_duration_guards =
                    duration_guards.stream()
                    .map(ABSDurationGuard::getMaxTime)
                    .reduce(duration_guards.peek().getMaxTime(), (t1, t2) -> t1.compareTo(t2) < 0 ? t1 : t2);
            }
            guardsWoken = guardsWoken + resourceGuardsWoken + durationGuardsWoken;
        }
        this.nWakingCogs = guardsWoken;
    }

    /**
     * Add a duration guard to the list of guards waiting for time to advance.
     *
     * @param guard The duration guard.
     */
    public void addDurationGuard (ABSDurationGuard guard) {
        Aprational guard_max_time = guard.getMaxTime();
        if (guard_max_time.compareTo(clock) <= 0) {
            // Can't happen: this is called from DurationGuards after checking
            // for current time, and time is advanced only when all cogs are
            // idle.
            throw new RuntimeException("Trying to wait for a time that is less than or equal to current clock");
        }
        synchronized(duration_guards) {
            if (duration_guards.isEmpty()) {
                wake_time_for_duration_guards = guard_max_time;
            } else {
                wake_time_for_duration_guards = guard_max_time.compareTo(wake_time_for_duration_guards) < 0 ? guard_max_time : wake_time_for_duration_guards;
            }
            duration_guards.add(guard);
        }
    }

    /**
     * Add a resource guard that waits for a resource, together with the
     * deployment component that shall provide it.
     */
    public void addResourceGuard(ABSResourceGuard guard, ABSInterface dc) {
        synchronized(resource_guards) {
            resource_guards.computeIfAbsent(dc, k -> new ArrayList<>()).add(guard);
        }
    }

    public void notifyCogActive() {
        synchronized(this) {
            nActiveCogs++;
            // Note that this is still kind of an approximation: when we
            // signal all guards after clock advance, in theory one cog could
            // wake up, go to sleep, wake up, go to sleep before the other cog
            // has woken up a first time, which would result in a spurious
            // clock advance.
            nWakingCogs = Math.max(0, nWakingCogs - 1);
        }
        log.finest(() -> "Cog became active, now " + nActiveCogs + " active cogs.");
    }

    public void notifyCogInactive() {
        synchronized(this) {
            nActiveCogs--;
        }
        if (nActiveCogs == 0) {
            log.finest(() -> "Cog became inactive, no active cogs left.");
            if (nWakingCogs > 0) {
                log.finest(() -> "Waiting for " + nWakingCogs + " more cogs to wake before trying clock advance.");
            } else {
                maybeAdvanceClock();
            }
        } else if (nActiveCogs > 0) {
            log.finest(() -> "Cog became inactive, now " + nActiveCogs + " active cogs.");
        } else {
            log.severe(() -> "Count of active cogs became negative (" + nActiveCogs + "), this should never happen");
        }
    }
}
