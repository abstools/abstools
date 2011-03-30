/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.backend.java.lib.runtime;

import java.io.File;
import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.net.MalformedURLException;
import java.net.URL;
import java.net.URLClassLoader;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Random;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.logging.Logger;

import abs.backend.java.observing.SystemObserver;
import abs.backend.java.scheduling.DefaultTaskScheduler;
import abs.backend.java.scheduling.GlobalScheduler;
import abs.backend.java.scheduling.GlobalSchedulingStrategy;
import abs.backend.java.scheduling.RandomSchedulingStrategy;
import abs.backend.java.scheduling.ScheduleAction;
import abs.backend.java.scheduling.SimpleTaskScheduler;
import abs.backend.java.scheduling.TaskScheduler;
import abs.backend.java.scheduling.TaskSchedulerFactory;
import abs.backend.java.scheduling.TaskSchedulingStrategy;
import abs.backend.java.scheduling.TotalSchedulingStrategy;

public class ABSRuntime {
    private static final Logger logger = Logging.getLogger(ABSRuntime.class.getName());

    private final ABSThreadManager threadManager = new ABSThreadManager(this);
    private final AtomicInteger cogCounter = new AtomicInteger();
    private final AtomicInteger taskCounter = new AtomicInteger();

    /**
     * Starts a new ABS program by giving a generated Main class
     * @param mainClass the Main class to be used
     * @throws InstantiationException if the Main class could not be instantiated
     * @throws IllegalAccessException if the Main class could not be accessed
     */
    public void start(Class<?> mainClass) throws InstantiationException, IllegalAccessException {
        systemStarted();
        COG cog = new COG(this,mainClass);
        try {
            Constructor<?> constr = mainClass.getConstructor(COG.class);
            ABSObject mainObject = (ABSObject) constr.newInstance(cog);
            cogCreated(mainObject);
            asyncCall(new ABSMainTask(mainObject));
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
        
        URL[] urls;
        try {
            urls = new URL[] { targetDir.toURI().toURL() };
            ClassLoader classLoader = new URLClassLoader(urls, ABSRuntime.class.getClassLoader());
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

    public synchronized void setRandomSeed(long seed) {
        randomSeed = seed;
        random = new Random(seed);
        logger.config("New Random Seed: " + randomSeed);
        
        if (globalSchedulingStrategy instanceof RandomSchedulingStrategy) {
            ((RandomSchedulingStrategy)globalSchedulingStrategy).setRandom(random);
        }

        if (taskSchedulingStrategy instanceof RandomSchedulingStrategy) {
            ((RandomSchedulingStrategy)taskSchedulingStrategy).setRandom(random);
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
            throw new IllegalStateException("ABS Runtime was already shutdown");
        isShutdown = true;
        if (hasGlobalScheduler())
            globalScheduler.shutdown();
        threadManager.shutdownAllThreads();
    }

    public TaskScheduler createTaskScheduler(COG cog) {
        return taskSchedulerFactory.createTaskScheduler(this, cog, threadManager);
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
        await(new ABSTrueGuard());
    }

    public static void await(ABSGuard g) {
        getCurrentCOG().getScheduler().await(g);
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

    public static ABSFut<?> asyncCall(Task<?> task) {
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

}
