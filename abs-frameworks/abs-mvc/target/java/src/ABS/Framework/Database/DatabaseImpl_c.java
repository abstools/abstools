package ABS.Framework.Database;
// Database.abs:9:0: 
public class DatabaseImpl_c extends abs.backend.java.lib.runtime.ABSObject implements abs.backend.java.lib.types.ABSClass, ABS.Framework.Database.Database_i {
    private static final java.lang.String[] __fieldNames = new java.lang.String[] {  };
    public final java.util.List<java.lang.String> getFieldNames() { return java.util.Arrays.asList(__fieldNames); }
    public DatabaseImpl_c() {
        getCOG().objectCreated(this);
    }
    protected final void __ABS_init() {
        getCOG().objectInitialized(this);
    }
    protected final abs.backend.java.lib.types.ABSValue getFieldValue(java.lang.String __ABS_fieldName) throws java.lang.NoSuchFieldException {
        return super.getFieldValue(__ABS_fieldName);
    }
    public final java.lang.String getClassName() { return "DatabaseImpl"; }
    public static final <T extends DatabaseImpl_c> T createNewCOG() { return (T)DatabaseImpl_c.__ABS_createNewCOG(null, null); }
    public static final <T extends DatabaseImpl_c> T __ABS_createNewCOG(abs.backend.java.lib.runtime.ABSObject __ABS_source, abs.backend.java.scheduling.UserSchedulingStrategy strategy) {
        final abs.backend.java.lib.runtime.ABSRuntime __ABS_runtime = abs.backend.java.lib.runtime.ABSRuntime.getCurrentRuntime();
        final abs.backend.java.lib.runtime.COG __ABS_cog = strategy == null ? __ABS_runtime.createCOG(DatabaseImpl_c.class) : __ABS_runtime.createCOG(DatabaseImpl_c.class, strategy);
        final abs.backend.java.lib.runtime.ABSThread __ABS_thread = abs.backend.java.lib.runtime.ABSRuntime.getCurrentThread();
        final abs.backend.java.lib.runtime.COG __ABS_oldCOG = abs.backend.java.lib.runtime.ABSRuntime.getCurrentCOG();
        final abs.backend.java.lib.runtime.Task __ABS_sendingTask = abs.backend.java.lib.runtime.ABSRuntime.getCurrentTask();
        __ABS_thread.setCOG(__ABS_cog);
        try {
            DatabaseImpl_c __ABS_result = (DatabaseImpl_c) __ABS_runtime.getForeignObject
            ("ABS.Framework.Database.DatabaseImpl");
            if (__ABS_result == null) __ABS_result = new DatabaseImpl_c();
            ;
            __ABS_runtime.cogCreated(__ABS_result);
            __ABS_cog.getScheduler().addTask(new abs.backend.java.lib.runtime.Task(new abs.backend.java.lib.runtime.ABSInitObjectCall(__ABS_sendingTask,__ABS_source,__ABS_result)));
            return (T)__ABS_result;
        } finally {
            __ABS_thread.setCOG(__ABS_oldCOG);
        }
    }
    public static final <T extends DatabaseImpl_c> T createNewObject() { return (T)DatabaseImpl_c.__ABS_createNewObject(null); }
    public static final <T extends DatabaseImpl_c> T __ABS_createNewObject(abs.backend.java.lib.runtime.ABSObject __ABS_source) {
        DatabaseImpl_c __ABS_result = (DatabaseImpl_c) abs.backend.java.lib.runtime.ABSRuntime.getCurrentRuntime().getForeignObject
        ("ABS.Framework.Database.DatabaseImpl");
        if (__ABS_result == null) __ABS_result = new DatabaseImpl_c();
        __ABS_result.__ABS_init();
        return (T)__ABS_result;
    }
    // Database.abs:0:0: 
    public final abs.backend.java.lib.runtime.ABSFut<abs.backend.java.lib.types.ABSString> async_connect(abs.backend.java.lib.types.ABSString msg) {
        return (abs.backend.java.lib.runtime.ABSFut)abs.backend.java.lib.runtime.ABSRuntime.getCurrentRuntime().asyncCall(new abs.backend.java.lib.runtime.AbstractAsyncCallRT<ABS.Framework.Database.DatabaseImpl_c>(
            this,
            abs.backend.java.lib.runtime.ABSRuntime.checkForNull(this),
            new ABS.StdLib.Duration_InfDuration(),
            new ABS.StdLib.Duration_InfDuration(),
            abs.backend.java.lib.types.ABSBool.FALSE) {
                abs.backend.java.lib.types.ABSString arg0;
                public java.util.List<abs.backend.java.lib.types.ABSValue> getArgs() {
                    return java.util.Arrays.asList(new abs.backend.java.lib.types.ABSValue[] {
                        arg0});
                }
                public abs.backend.java.lib.runtime.AsyncCall<?> init(abs.backend.java.lib.types.ABSString _arg0) {
                    arg0 = _arg0;
                    return this;
                }
                public java.lang.String methodName() {
                    return "connect";
                }
                public Object execute() {
                    return target.connect(arg0
                    );
                }
            }.init(msg))
        ;
    }
    // Database.abs:0:0: 
    public final abs.backend.java.lib.types.ABSString connect(abs.backend.java.lib.types.ABSString msg) {
        __ABS_checkSameCOG(); 
        return this.fli_connect(msg);
    }
    // Database.abs:0:0: 
    public  abs.backend.java.lib.types.ABSString fli_connect(abs.backend.java.lib.types.ABSString msg) {
        __ABS_checkSameCOG(); 
        if (__ABS_getRuntime().debuggingEnabled()) {
            abs.backend.java.lib.runtime.Task<?> __ABS_currentTask = __ABS_getRuntime().getCurrentTask();
            __ABS_currentTask.newStackFrame(this, "connect");
            __ABS_currentTask.setLocalVariable("msg",msg);
        }
         {
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("C:\\Users\\ls\\Documents\\afifun\\abs-microservices-framework\\abstools\\abs-frameworks\\abs-mvc\\src\\abs\\framework\\Database.abs",0);
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("C:\\Users\\ls\\Documents\\afifun\\abs-microservices-framework\\abstools\\abs-frameworks\\abs-mvc\\src\\abs\\framework\\Database.abs",11);
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().getCurrentTask().popStackFrame();
            return abs.backend.java.lib.types.ABSString.fromString("default implementation");
        }
    }
}
