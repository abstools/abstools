package ABS.Framework.Http;
// ABSHttpRequest.abs:11:0: 
public final class ABSHttpRequestImpl_c extends abs.backend.java.lib.runtime.ABSObject implements abs.backend.java.lib.types.ABSClass, ABS.Framework.Http.ABSHttpRequest_i {
    private static final java.lang.String[] __fieldNames = new java.lang.String[] { "requestInput", "requestProperty" };
    public final java.util.List<java.lang.String> getFieldNames() { return java.util.Arrays.asList(__fieldNames); }
    private ABS.StdLib.Map<abs.backend.java.lib.types.ABSString,abs.backend.java.lib.types.ABSString> requestInput;
    private ABS.StdLib.Map<abs.backend.java.lib.types.ABSString,abs.backend.java.lib.types.ABSString> requestProperty;
    public ABSHttpRequestImpl_c(ABS.StdLib.Map<abs.backend.java.lib.types.ABSString,abs.backend.java.lib.types.ABSString> requestInput, ABS.StdLib.Map<abs.backend.java.lib.types.ABSString,abs.backend.java.lib.types.ABSString> requestProperty) {
        this.requestInput = requestInput;
        this.requestProperty = requestProperty;
        getCOG().objectCreated(this);
    }
    protected final void __ABS_init() {
        getCOG().objectInitialized(this);
    }
    protected final abs.backend.java.lib.types.ABSValue getFieldValue(java.lang.String __ABS_fieldName) throws java.lang.NoSuchFieldException {
        if ("requestInput".equals(__ABS_fieldName)) return requestInput;
        if ("requestProperty".equals(__ABS_fieldName)) return requestProperty;
        return super.getFieldValue(__ABS_fieldName);
    }
    public final java.lang.String getClassName() { return "ABSHttpRequestImpl"; }
    public static final <T extends ABSHttpRequestImpl_c> T createNewCOG(ABS.StdLib.Map<abs.backend.java.lib.types.ABSString,abs.backend.java.lib.types.ABSString> requestInput, ABS.StdLib.Map<abs.backend.java.lib.types.ABSString,abs.backend.java.lib.types.ABSString> requestProperty) { return (T)ABSHttpRequestImpl_c.__ABS_createNewCOG(null, null, requestInput, requestProperty); }
    public static final <T extends ABSHttpRequestImpl_c> T __ABS_createNewCOG(abs.backend.java.lib.runtime.ABSObject __ABS_source, abs.backend.java.scheduling.UserSchedulingStrategy strategy, ABS.StdLib.Map<abs.backend.java.lib.types.ABSString,abs.backend.java.lib.types.ABSString> requestInput, ABS.StdLib.Map<abs.backend.java.lib.types.ABSString,abs.backend.java.lib.types.ABSString> requestProperty) {
        final abs.backend.java.lib.runtime.ABSRuntime __ABS_runtime = abs.backend.java.lib.runtime.ABSRuntime.getCurrentRuntime();
        final abs.backend.java.lib.runtime.COG __ABS_cog = strategy == null ? __ABS_runtime.createCOG(ABSHttpRequestImpl_c.class) : __ABS_runtime.createCOG(ABSHttpRequestImpl_c.class, strategy);
        final abs.backend.java.lib.runtime.ABSThread __ABS_thread = abs.backend.java.lib.runtime.ABSRuntime.getCurrentThread();
        final abs.backend.java.lib.runtime.COG __ABS_oldCOG = abs.backend.java.lib.runtime.ABSRuntime.getCurrentCOG();
        final abs.backend.java.lib.runtime.Task __ABS_sendingTask = abs.backend.java.lib.runtime.ABSRuntime.getCurrentTask();
        __ABS_thread.setCOG(__ABS_cog);
        try {
            ABSHttpRequestImpl_c __ABS_result = new ABSHttpRequestImpl_c(requestInput, requestProperty);
            ;
            __ABS_runtime.cogCreated(__ABS_result);
            __ABS_cog.getScheduler().addTask(new abs.backend.java.lib.runtime.Task(new abs.backend.java.lib.runtime.ABSInitObjectCall(__ABS_sendingTask,__ABS_source,__ABS_result)));
            return (T)__ABS_result;
        } finally {
            __ABS_thread.setCOG(__ABS_oldCOG);
        }
    }
    public static final <T extends ABSHttpRequestImpl_c> T createNewObject(ABS.StdLib.Map<abs.backend.java.lib.types.ABSString,abs.backend.java.lib.types.ABSString> requestInput, ABS.StdLib.Map<abs.backend.java.lib.types.ABSString,abs.backend.java.lib.types.ABSString> requestProperty) { return (T)ABSHttpRequestImpl_c.__ABS_createNewObject(null, requestInput, requestProperty); }
    public static final <T extends ABSHttpRequestImpl_c> T __ABS_createNewObject(abs.backend.java.lib.runtime.ABSObject __ABS_source, ABS.StdLib.Map<abs.backend.java.lib.types.ABSString,abs.backend.java.lib.types.ABSString> requestInput, ABS.StdLib.Map<abs.backend.java.lib.types.ABSString,abs.backend.java.lib.types.ABSString> requestProperty) {
        ABSHttpRequestImpl_c __ABS_result = new ABSHttpRequestImpl_c(requestInput, requestProperty);
        __ABS_result.__ABS_init();
        return (T)__ABS_result;
    }
    // ABSHttpRequest.abs:0:0: 
    public final abs.backend.java.lib.runtime.ABSFut<abs.backend.java.lib.types.ABSString> async_getInput(abs.backend.java.lib.types.ABSString key) {
        return (abs.backend.java.lib.runtime.ABSFut)abs.backend.java.lib.runtime.ABSRuntime.getCurrentRuntime().asyncCall(new abs.backend.java.lib.runtime.AbstractAsyncCallRT<ABS.Framework.Http.ABSHttpRequestImpl_c>(
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
                    return "getInput";
                }
                public Object execute() {
                    return target.getInput(arg0
                    );
                }
            }.init(key))
        ;
    }
    // ABSHttpRequest.abs:0:0: 
    public final abs.backend.java.lib.types.ABSString getInput(abs.backend.java.lib.types.ABSString key) {
        __ABS_checkSameCOG(); 
        if (__ABS_getRuntime().debuggingEnabled()) {
            abs.backend.java.lib.runtime.Task<?> __ABS_currentTask = __ABS_getRuntime().getCurrentTask();
            __ABS_currentTask.newStackFrame(this, "getInput");
            __ABS_currentTask.setLocalVariable("key",key);
        }
         {
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("C:\\Users\\ls\\Documents\\afifun\\abs-microservices-framework\\abstools\\abs-frameworks\\abs-mvc\\src\\abs\\framework\\ABSHttpRequest.abs",0);
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("C:\\Users\\ls\\Documents\\afifun\\abs-microservices-framework\\abstools\\abs-frameworks\\abs-mvc\\src\\abs\\framework\\ABSHttpRequest.abs",15);
            abs.backend.java.lib.types.ABSString value = ABS.StdLib.fromJust_f.apply(ABS.StdLib.lookup_f.apply(ABSHttpRequestImpl_c.this.requestInput, key));
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().getCurrentTask().setLocalVariable("value",value);
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("C:\\Users\\ls\\Documents\\afifun\\abs-microservices-framework\\abstools\\abs-frameworks\\abs-mvc\\src\\abs\\framework\\ABSHttpRequest.abs",16);
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().getCurrentTask().popStackFrame();
            return value;
        }
    }
    // ABSHttpRequest.abs:0:0: 
    public final abs.backend.java.lib.runtime.ABSFut<abs.backend.java.lib.types.ABSString> async_getRequestProperty(abs.backend.java.lib.types.ABSString key) {
        return (abs.backend.java.lib.runtime.ABSFut)abs.backend.java.lib.runtime.ABSRuntime.getCurrentRuntime().asyncCall(new abs.backend.java.lib.runtime.AbstractAsyncCallRT<ABS.Framework.Http.ABSHttpRequestImpl_c>(
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
                    return "getRequestProperty";
                }
                public Object execute() {
                    return target.getRequestProperty(arg0
                    );
                }
            }.init(key))
        ;
    }
    // ABSHttpRequest.abs:0:0: 
    public final abs.backend.java.lib.types.ABSString getRequestProperty(abs.backend.java.lib.types.ABSString key) {
        __ABS_checkSameCOG(); 
        if (__ABS_getRuntime().debuggingEnabled()) {
            abs.backend.java.lib.runtime.Task<?> __ABS_currentTask = __ABS_getRuntime().getCurrentTask();
            __ABS_currentTask.newStackFrame(this, "getRequestProperty");
            __ABS_currentTask.setLocalVariable("key",key);
        }
         {
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("C:\\Users\\ls\\Documents\\afifun\\abs-microservices-framework\\abstools\\abs-frameworks\\abs-mvc\\src\\abs\\framework\\ABSHttpRequest.abs",0);
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("C:\\Users\\ls\\Documents\\afifun\\abs-microservices-framework\\abstools\\abs-frameworks\\abs-mvc\\src\\abs\\framework\\ABSHttpRequest.abs",21);
            abs.backend.java.lib.types.ABSString value = ABS.StdLib.fromJust_f.apply(ABS.StdLib.lookup_f.apply(ABSHttpRequestImpl_c.this.requestProperty, key));
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().getCurrentTask().setLocalVariable("value",value);
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("C:\\Users\\ls\\Documents\\afifun\\abs-microservices-framework\\abstools\\abs-frameworks\\abs-mvc\\src\\abs\\framework\\ABSHttpRequest.abs",22);
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().getCurrentTask().popStackFrame();
            return value;
        }
    }
}
