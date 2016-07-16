package ABS.DC;
// abslang.abs:808:0: 
public final class DeploymentComponent_c extends abs.backend.java.lib.runtime.ABSObject implements abs.backend.java.lib.types.ABSClass, ABS.DC.DeploymentComponent_i {
    private static final java.lang.String[] __fieldNames = new java.lang.String[] { "description", "initconfig", "creationTime", "cpuhistory", "cpuhistorytotal", "bwhistory", "bwhistorytotal", "memoryhistory", "memoryhistorytotal", "cpuconsumed", "bwconsumed", "memoryconsumed", "initialized", "cpu", "cpunext", "bw", "bwnext", "memory", "memorynext", "paymentInterval", "costPerInterval", "startupDuration", "cloudprovider" };
    public final java.util.List<java.lang.String> getFieldNames() { return java.util.Arrays.asList(__fieldNames); }
    private abs.backend.java.lib.types.ABSString description;
    private ABS.StdLib.Map<ABS.DC.Resourcetype,abs.backend.java.lib.types.ABSRational> initconfig;
    // abslang.abs:812:4: 
    private ABS.StdLib.Time creationTime;
    // abslang.abs:815:4: 
    private ABS.StdLib.List<abs.backend.java.lib.types.ABSRational> cpuhistory;
    // abslang.abs:816:4: 
    private ABS.StdLib.List<abs.backend.java.lib.types.ABSRational> cpuhistorytotal;
    // abslang.abs:817:4: 
    private ABS.StdLib.List<abs.backend.java.lib.types.ABSRational> bwhistory;
    // abslang.abs:818:4: 
    private ABS.StdLib.List<abs.backend.java.lib.types.ABSRational> bwhistorytotal;
    // abslang.abs:819:4: 
    private ABS.StdLib.List<abs.backend.java.lib.types.ABSRational> memoryhistory;
    // abslang.abs:820:4: 
    private ABS.StdLib.List<abs.backend.java.lib.types.ABSRational> memoryhistorytotal;
    // abslang.abs:823:4: 
    private abs.backend.java.lib.types.ABSRational cpuconsumed;
    // abslang.abs:824:4: 
    private abs.backend.java.lib.types.ABSRational bwconsumed;
    // abslang.abs:825:4: 
    private abs.backend.java.lib.types.ABSRational memoryconsumed;
    // abslang.abs:827:4: 
    private abs.backend.java.lib.types.ABSBool initialized;
    // abslang.abs:829:4: 
    private ABS.DC.InfRat cpu;
    // abslang.abs:831:4: 
    private ABS.DC.InfRat cpunext;
    // abslang.abs:832:4: 
    private ABS.DC.InfRat bw;
    // abslang.abs:834:4: 
    private ABS.DC.InfRat bwnext;
    // abslang.abs:835:4: 
    private ABS.DC.InfRat memory;
    // abslang.abs:837:4: 
    private ABS.DC.InfRat memorynext;
    // abslang.abs:838:4: 
    private abs.backend.java.lib.types.ABSInteger paymentInterval;
    // abslang.abs:841:4: 
    private abs.backend.java.lib.types.ABSRational costPerInterval;
    // abslang.abs:843:4: 
    private abs.backend.java.lib.types.ABSRational startupDuration;
    // abslang.abs:846:4: 
    private ABS.DC.CloudProvider_i cloudprovider;
    public DeploymentComponent_c(abs.backend.java.lib.types.ABSString description, ABS.StdLib.Map<ABS.DC.Resourcetype,abs.backend.java.lib.types.ABSRational> initconfig) {
        this.description = description;
        this.initconfig = initconfig;
        getCOG().objectCreated(this);
    }
    protected final void __ABS_init() {
        this.creationTime = ABS.StdLib.now_f.apply();this.cpuhistory = new ABS.StdLib.List_Nil();this.cpuhistorytotal = new ABS.StdLib.List_Nil();this.bwhistory = new ABS.StdLib.List_Nil();this.bwhistorytotal = new ABS.StdLib.List_Nil();this.memoryhistory = new ABS.StdLib.List_Nil();this.memoryhistorytotal = new ABS.StdLib.List_Nil();this.cpuconsumed = abs.backend.java.lib.types.ABSInteger.fromString("0");this.bwconsumed = abs.backend.java.lib.types.ABSInteger.fromString("0");this.memoryconsumed = abs.backend.java.lib.types.ABSInteger.fromString("0");this.initialized = abs.backend.java.lib.types.ABSBool.FALSE;this.cpu = new abs.backend.java.lib.expr.Case() {
            public ABS.DC.InfRat of(final ABS.StdLib.Maybe<abs.backend.java.lib.types.ABSRational> __ABS_value) {
                final abs.backend.java.lib.expr.PatternBinding __ABS_binding0 = new abs.backend.java.lib.expr.PatternConstructor(ABS.StdLib.Maybe_Nothing.class).match(__ABS_value);
                if (__ABS_binding0 != null) return new Object() {
                    public ABS.DC.InfRat execute() { return new ABS.DC.InfRat_InfRat(); }
                }.execute();
                final abs.backend.java.lib.expr.PatternBinding __ABS_binding1 = new abs.backend.java.lib.expr.PatternConstructor(ABS.StdLib.Maybe_Just.class,new abs.backend.java.lib.expr.PatternVariable("v")).match(__ABS_value);
                if (__ABS_binding1 != null) return new Object() {
                    public ABS.DC.InfRat execute(final abs.backend.java.lib.types.ABSRational v) { return new ABS.DC.InfRat_Fin(v); }
                }.execute((abs.backend.java.lib.types.ABSRational) __ABS_binding1.getBinding(0));
                throw new abs.backend.java.lib.expr.UnmatchedCaseException("abslang.abs:829:17:  value " + __ABS_value + " did not match any pattern.");
            }
        }.of(ABS.StdLib.lookup_f.apply(DeploymentComponent_c.this.initconfig, new ABS.DC.Resourcetype_CPU()));this.cpunext = DeploymentComponent_c.this.cpu;this.bw = new abs.backend.java.lib.expr.Case() {
            public ABS.DC.InfRat of(final ABS.StdLib.Maybe<abs.backend.java.lib.types.ABSRational> __ABS_value) {
                final abs.backend.java.lib.expr.PatternBinding __ABS_binding0 = new abs.backend.java.lib.expr.PatternConstructor(ABS.StdLib.Maybe_Nothing.class).match(__ABS_value);
                if (__ABS_binding0 != null) return new Object() {
                    public ABS.DC.InfRat execute() { return new ABS.DC.InfRat_InfRat(); }
                }.execute();
                final abs.backend.java.lib.expr.PatternBinding __ABS_binding1 = new abs.backend.java.lib.expr.PatternConstructor(ABS.StdLib.Maybe_Just.class,new abs.backend.java.lib.expr.PatternVariable("v")).match(__ABS_value);
                if (__ABS_binding1 != null) return new Object() {
                    public ABS.DC.InfRat execute(final abs.backend.java.lib.types.ABSRational v) { return new ABS.DC.InfRat_Fin(v); }
                }.execute((abs.backend.java.lib.types.ABSRational) __ABS_binding1.getBinding(0));
                throw new abs.backend.java.lib.expr.UnmatchedCaseException("abslang.abs:832:16:  value " + __ABS_value + " did not match any pattern.");
            }
        }.of(ABS.StdLib.lookup_f.apply(DeploymentComponent_c.this.initconfig, new ABS.DC.Resourcetype_Bandwidth()));this.bwnext = DeploymentComponent_c.this.bw;this.memory = new abs.backend.java.lib.expr.Case() {
            public ABS.DC.InfRat of(final ABS.StdLib.Maybe<abs.backend.java.lib.types.ABSRational> __ABS_value) {
                final abs.backend.java.lib.expr.PatternBinding __ABS_binding0 = new abs.backend.java.lib.expr.PatternConstructor(ABS.StdLib.Maybe_Nothing.class).match(__ABS_value);
                if (__ABS_binding0 != null) return new Object() {
                    public ABS.DC.InfRat execute() { return new ABS.DC.InfRat_InfRat(); }
                }.execute();
                final abs.backend.java.lib.expr.PatternBinding __ABS_binding1 = new abs.backend.java.lib.expr.PatternConstructor(ABS.StdLib.Maybe_Just.class,new abs.backend.java.lib.expr.PatternVariable("m")).match(__ABS_value);
                if (__ABS_binding1 != null) return new Object() {
                    public ABS.DC.InfRat execute(final abs.backend.java.lib.types.ABSRational m) { return new ABS.DC.InfRat_Fin(m); }
                }.execute((abs.backend.java.lib.types.ABSRational) __ABS_binding1.getBinding(0));
                throw new abs.backend.java.lib.expr.UnmatchedCaseException("abslang.abs:835:20:  value " + __ABS_value + " did not match any pattern.");
            }
        }.of(ABS.StdLib.lookup_f.apply(DeploymentComponent_c.this.initconfig, new ABS.DC.Resourcetype_Memory()));this.memorynext = DeploymentComponent_c.this.memory;this.paymentInterval = new abs.backend.java.lib.expr.Case() {
            public abs.backend.java.lib.types.ABSInteger of(final ABS.StdLib.Maybe<abs.backend.java.lib.types.ABSRational> __ABS_value) {
                final abs.backend.java.lib.expr.PatternBinding __ABS_binding0 = new abs.backend.java.lib.expr.PatternConstructor(ABS.StdLib.Maybe_Nothing.class).match(__ABS_value);
                if (__ABS_binding0 != null) return new Object() {
                    public abs.backend.java.lib.types.ABSInteger execute() { return abs.backend.java.lib.types.ABSInteger.fromString("1"); }
                }.execute();
                final abs.backend.java.lib.expr.PatternBinding __ABS_binding1 = new abs.backend.java.lib.expr.PatternConstructor(ABS.StdLib.Maybe_Just.class,new abs.backend.java.lib.expr.PatternVariable("n")).match(__ABS_value);
                if (__ABS_binding1 != null) return new Object() {
                    public abs.backend.java.lib.types.ABSInteger execute(final abs.backend.java.lib.types.ABSRational n) { return abs.backend.java.lib.runtime.ABSBuiltInFunctions.truncate(n); }
                }.execute((abs.backend.java.lib.types.ABSRational) __ABS_binding1.getBinding(0));
                throw new abs.backend.java.lib.expr.UnmatchedCaseException("abslang.abs:839:8:  value " + __ABS_value + " did not match any pattern.");
            }
        }.of(ABS.StdLib.lookup_f.apply(DeploymentComponent_c.this.initconfig, new ABS.DC.Resourcetype_PaymentInterval()));this.costPerInterval = new abs.backend.java.lib.expr.Case() {
            public abs.backend.java.lib.types.ABSRational of(final ABS.StdLib.Maybe<abs.backend.java.lib.types.ABSRational> __ABS_value) {
                final abs.backend.java.lib.expr.PatternBinding __ABS_binding0 = new abs.backend.java.lib.expr.PatternConstructor(ABS.StdLib.Maybe_Nothing.class).match(__ABS_value);
                if (__ABS_binding0 != null) return new Object() {
                    public abs.backend.java.lib.types.ABSRational execute() { return abs.backend.java.lib.types.ABSInteger.fromString("0"); }
                }.execute();
                final abs.backend.java.lib.expr.PatternBinding __ABS_binding1 = new abs.backend.java.lib.expr.PatternConstructor(ABS.StdLib.Maybe_Just.class,new abs.backend.java.lib.expr.PatternVariable("o")).match(__ABS_value);
                if (__ABS_binding1 != null) return new Object() {
                    public abs.backend.java.lib.types.ABSRational execute(final abs.backend.java.lib.types.ABSRational o) { return o; }
                }.execute((abs.backend.java.lib.types.ABSRational) __ABS_binding1.getBinding(0));
                throw new abs.backend.java.lib.expr.UnmatchedCaseException("abslang.abs:841:26:  value " + __ABS_value + " did not match any pattern.");
            }
        }.of(ABS.StdLib.lookup_f.apply(DeploymentComponent_c.this.initconfig, new ABS.DC.Resourcetype_CostPerInterval()));this.startupDuration = new abs.backend.java.lib.expr.Case() {
            public abs.backend.java.lib.types.ABSRational of(final ABS.StdLib.Maybe<abs.backend.java.lib.types.ABSRational> __ABS_value) {
                final abs.backend.java.lib.expr.PatternBinding __ABS_binding0 = new abs.backend.java.lib.expr.PatternConstructor(ABS.StdLib.Maybe_Nothing.class).match(__ABS_value);
                if (__ABS_binding0 != null) return new Object() {
                    public abs.backend.java.lib.types.ABSRational execute() { return abs.backend.java.lib.types.ABSInteger.fromString("0"); }
                }.execute();
                final abs.backend.java.lib.expr.PatternBinding __ABS_binding1 = new abs.backend.java.lib.expr.PatternConstructor(ABS.StdLib.Maybe_Just.class,new abs.backend.java.lib.expr.PatternVariable("p")).match(__ABS_value);
                if (__ABS_binding1 != null) return new Object() {
                    public abs.backend.java.lib.types.ABSRational execute(final abs.backend.java.lib.types.ABSRational p) { return p; }
                }.execute((abs.backend.java.lib.types.ABSRational) __ABS_binding1.getBinding(0));
                throw new abs.backend.java.lib.expr.UnmatchedCaseException("abslang.abs:843:26:  value " + __ABS_value + " did not match any pattern.");
            }
        }.of(ABS.StdLib.lookup_f.apply(DeploymentComponent_c.this.initconfig, new ABS.DC.Resourcetype_Startupduration()));this.cloudprovider = null; {
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("abs\\lang\\abslang.abs",0);
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().getCurrentTask().newStackFrame(this,"init block");
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("abs\\lang\\abslang.abs",848);
            DeploymentComponent_c.this.initialized = abs.backend.java.lib.types.ABSBool.TRUE;
        }
        getCOG().objectInitialized(this);
    }
    protected final abs.backend.java.lib.types.ABSValue getFieldValue(java.lang.String __ABS_fieldName) throws java.lang.NoSuchFieldException {
        if ("description".equals(__ABS_fieldName)) return description;
        if ("initconfig".equals(__ABS_fieldName)) return initconfig;
        if ("creationTime".equals(__ABS_fieldName)) return creationTime;
        if ("cpuhistory".equals(__ABS_fieldName)) return cpuhistory;
        if ("cpuhistorytotal".equals(__ABS_fieldName)) return cpuhistorytotal;
        if ("bwhistory".equals(__ABS_fieldName)) return bwhistory;
        if ("bwhistorytotal".equals(__ABS_fieldName)) return bwhistorytotal;
        if ("memoryhistory".equals(__ABS_fieldName)) return memoryhistory;
        if ("memoryhistorytotal".equals(__ABS_fieldName)) return memoryhistorytotal;
        if ("cpuconsumed".equals(__ABS_fieldName)) return cpuconsumed;
        if ("bwconsumed".equals(__ABS_fieldName)) return bwconsumed;
        if ("memoryconsumed".equals(__ABS_fieldName)) return memoryconsumed;
        if ("initialized".equals(__ABS_fieldName)) return initialized;
        if ("cpu".equals(__ABS_fieldName)) return cpu;
        if ("cpunext".equals(__ABS_fieldName)) return cpunext;
        if ("bw".equals(__ABS_fieldName)) return bw;
        if ("bwnext".equals(__ABS_fieldName)) return bwnext;
        if ("memory".equals(__ABS_fieldName)) return memory;
        if ("memorynext".equals(__ABS_fieldName)) return memorynext;
        if ("paymentInterval".equals(__ABS_fieldName)) return paymentInterval;
        if ("costPerInterval".equals(__ABS_fieldName)) return costPerInterval;
        if ("startupDuration".equals(__ABS_fieldName)) return startupDuration;
        if ("cloudprovider".equals(__ABS_fieldName)) return cloudprovider;
        return super.getFieldValue(__ABS_fieldName);
    }
    public final java.lang.String getClassName() { return "DeploymentComponent"; }
    public static final <T extends DeploymentComponent_c> T createNewCOG(abs.backend.java.lib.types.ABSString description, ABS.StdLib.Map<ABS.DC.Resourcetype,abs.backend.java.lib.types.ABSRational> initconfig) { return (T)DeploymentComponent_c.__ABS_createNewCOG(null, null, description, initconfig); }
    public static final <T extends DeploymentComponent_c> T __ABS_createNewCOG(abs.backend.java.lib.runtime.ABSObject __ABS_source, abs.backend.java.scheduling.UserSchedulingStrategy strategy, abs.backend.java.lib.types.ABSString description, ABS.StdLib.Map<ABS.DC.Resourcetype,abs.backend.java.lib.types.ABSRational> initconfig) {
        final abs.backend.java.lib.runtime.ABSRuntime __ABS_runtime = abs.backend.java.lib.runtime.ABSRuntime.getCurrentRuntime();
        final abs.backend.java.lib.runtime.COG __ABS_cog = strategy == null ? __ABS_runtime.createCOG(DeploymentComponent_c.class) : __ABS_runtime.createCOG(DeploymentComponent_c.class, strategy);
        final abs.backend.java.lib.runtime.ABSThread __ABS_thread = abs.backend.java.lib.runtime.ABSRuntime.getCurrentThread();
        final abs.backend.java.lib.runtime.COG __ABS_oldCOG = abs.backend.java.lib.runtime.ABSRuntime.getCurrentCOG();
        final abs.backend.java.lib.runtime.Task __ABS_sendingTask = abs.backend.java.lib.runtime.ABSRuntime.getCurrentTask();
        __ABS_thread.setCOG(__ABS_cog);
        try {
            DeploymentComponent_c __ABS_result = new DeploymentComponent_c(description, initconfig);
            ;
            __ABS_runtime.cogCreated(__ABS_result);
            __ABS_cog.getScheduler().addTask(new abs.backend.java.lib.runtime.Task(new abs.backend.java.lib.runtime.ABSInitObjectCall(__ABS_sendingTask,__ABS_source,__ABS_result)));
            return (T)__ABS_result;
        } finally {
            __ABS_thread.setCOG(__ABS_oldCOG);
        }
    }
    public static final <T extends DeploymentComponent_c> T createNewObject(abs.backend.java.lib.types.ABSString description, ABS.StdLib.Map<ABS.DC.Resourcetype,abs.backend.java.lib.types.ABSRational> initconfig) { return (T)DeploymentComponent_c.__ABS_createNewObject(null, description, initconfig); }
    public static final <T extends DeploymentComponent_c> T __ABS_createNewObject(abs.backend.java.lib.runtime.ABSObject __ABS_source, abs.backend.java.lib.types.ABSString description, ABS.StdLib.Map<ABS.DC.Resourcetype,abs.backend.java.lib.types.ABSRational> initconfig) {
        DeploymentComponent_c __ABS_result = new DeploymentComponent_c(description, initconfig);
        __ABS_result.__ABS_init();
        return (T)__ABS_result;
    }
    // abslang.abs:0:0: 
    public final abs.backend.java.lib.runtime.ABSFut<abs.backend.java.lib.types.ABSRational> async_load(ABS.DC.Resourcetype rtype, abs.backend.java.lib.types.ABSInteger periods) {
        return (abs.backend.java.lib.runtime.ABSFut)abs.backend.java.lib.runtime.ABSRuntime.getCurrentRuntime().asyncCall(new abs.backend.java.lib.runtime.AbstractAsyncCallRT<ABS.DC.DeploymentComponent_c>(
            this,
            abs.backend.java.lib.runtime.ABSRuntime.checkForNull(this),
            new ABS.StdLib.Duration_InfDuration(),
            new ABS.StdLib.Duration_InfDuration(),
            abs.backend.java.lib.types.ABSBool.FALSE) {
                ABS.DC.Resourcetype arg0;
                abs.backend.java.lib.types.ABSInteger arg1;
                public java.util.List<abs.backend.java.lib.types.ABSValue> getArgs() {
                    return java.util.Arrays.asList(new abs.backend.java.lib.types.ABSValue[] {
                        arg0,arg1});
                }
                public abs.backend.java.lib.runtime.AsyncCall<?> init(ABS.DC.Resourcetype _arg0,abs.backend.java.lib.types.ABSInteger _arg1) {
                    arg0 = _arg0;
                    arg1 = _arg1;
                    return this;
                }
                public java.lang.String methodName() {
                    return "load";
                }
                public Object execute() {
                    return target.load(arg0
                    ,arg1
                    .truncate());
                }
            }.init(rtype, periods))
        ;
    }
    // abslang.abs:0:0: 
    public final abs.backend.java.lib.types.ABSRational load(ABS.DC.Resourcetype rtype, abs.backend.java.lib.types.ABSInteger periods) {
        __ABS_checkSameCOG(); 
        if (__ABS_getRuntime().debuggingEnabled()) {
            abs.backend.java.lib.runtime.Task<?> __ABS_currentTask = __ABS_getRuntime().getCurrentTask();
            __ABS_currentTask.newStackFrame(this, "load");
            __ABS_currentTask.setLocalVariable("rtype",rtype);
            __ABS_currentTask.setLocalVariable("periods",periods);
        }
         {
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("abs\\lang\\abslang.abs",0);
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("abs\\lang\\abslang.abs",851);
            abs.backend.java.lib.types.ABSRational result = abs.backend.java.lib.types.ABSInteger.fromString("0");
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().getCurrentTask().setLocalVariable("result",result);
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("abs\\lang\\abslang.abs",852);
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("abs\\lang\\abslang.abs",869);
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().getCurrentTask().popStackFrame();
            return result;
        }
    }
    // abslang.abs:0:0: 
    public final abs.backend.java.lib.runtime.ABSFut<ABS.DC.InfRat> async_total(ABS.DC.Resourcetype rtype) {
        return (abs.backend.java.lib.runtime.ABSFut)abs.backend.java.lib.runtime.ABSRuntime.getCurrentRuntime().asyncCall(new abs.backend.java.lib.runtime.AbstractAsyncCallRT<ABS.DC.DeploymentComponent_c>(
            this,
            abs.backend.java.lib.runtime.ABSRuntime.checkForNull(this),
            new ABS.StdLib.Duration_InfDuration(),
            new ABS.StdLib.Duration_InfDuration(),
            abs.backend.java.lib.types.ABSBool.FALSE) {
                ABS.DC.Resourcetype arg0;
                public java.util.List<abs.backend.java.lib.types.ABSValue> getArgs() {
                    return java.util.Arrays.asList(new abs.backend.java.lib.types.ABSValue[] {
                        arg0});
                }
                public abs.backend.java.lib.runtime.AsyncCall<?> init(ABS.DC.Resourcetype _arg0) {
                    arg0 = _arg0;
                    return this;
                }
                public java.lang.String methodName() {
                    return "total";
                }
                public Object execute() {
                    return target.total(arg0
                    );
                }
            }.init(rtype))
        ;
    }
    // abslang.abs:0:0: 
    public final ABS.DC.InfRat total(ABS.DC.Resourcetype rtype) {
        __ABS_checkSameCOG(); 
        if (__ABS_getRuntime().debuggingEnabled()) {
            abs.backend.java.lib.runtime.Task<?> __ABS_currentTask = __ABS_getRuntime().getCurrentTask();
            __ABS_currentTask.newStackFrame(this, "total");
            __ABS_currentTask.setLocalVariable("rtype",rtype);
        }
         {
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("abs\\lang\\abslang.abs",0);
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("abs\\lang\\abslang.abs",872);
            ABS.DC.InfRat result = new ABS.DC.InfRat_InfRat();
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().getCurrentTask().setLocalVariable("result",result);
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("abs\\lang\\abslang.abs",873);
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("abs\\lang\\abslang.abs",878);
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().getCurrentTask().popStackFrame();
            return result;
        }
    }
    // abslang.abs:0:0: 
    public final abs.backend.java.lib.runtime.ABSFut<abs.backend.java.lib.types.ABSUnit> async_transfer(ABS.DC.DeploymentComponent_i target, abs.backend.java.lib.types.ABSRational amount, ABS.DC.Resourcetype rtype) {
        return (abs.backend.java.lib.runtime.ABSFut)abs.backend.java.lib.runtime.ABSRuntime.getCurrentRuntime().asyncCall(new abs.backend.java.lib.runtime.AbstractAsyncCallRT<ABS.DC.DeploymentComponent_c>(
            this,
            abs.backend.java.lib.runtime.ABSRuntime.checkForNull(this),
            new ABS.StdLib.Duration_InfDuration(),
            new ABS.StdLib.Duration_InfDuration(),
            abs.backend.java.lib.types.ABSBool.FALSE) {
                ABS.DC.DeploymentComponent_i arg0;
                abs.backend.java.lib.types.ABSRational arg1;
                ABS.DC.Resourcetype arg2;
                public java.util.List<abs.backend.java.lib.types.ABSValue> getArgs() {
                    return java.util.Arrays.asList(new abs.backend.java.lib.types.ABSValue[] {
                        arg0,arg1,arg2});
                }
                public abs.backend.java.lib.runtime.AsyncCall<?> init(ABS.DC.DeploymentComponent_i _arg0,abs.backend.java.lib.types.ABSRational _arg1,ABS.DC.Resourcetype _arg2) {
                    arg0 = _arg0;
                    arg1 = _arg1;
                    arg2 = _arg2;
                    return this;
                }
                public java.lang.String methodName() {
                    return "transfer";
                }
                public Object execute() {
                    return target.transfer(arg0
                    ,arg1
                    ,arg2
                    );
                }
            }.init(target, amount, rtype))
        ;
    }
    // abslang.abs:0:0: 
    public final abs.backend.java.lib.types.ABSUnit transfer(ABS.DC.DeploymentComponent_i target, abs.backend.java.lib.types.ABSRational amount, ABS.DC.Resourcetype rtype) {
        __ABS_checkSameCOG(); 
        if (__ABS_getRuntime().debuggingEnabled()) {
            abs.backend.java.lib.runtime.Task<?> __ABS_currentTask = __ABS_getRuntime().getCurrentTask();
            __ABS_currentTask.newStackFrame(this, "transfer");
            __ABS_currentTask.setLocalVariable("target",target);
            __ABS_currentTask.setLocalVariable("amount",amount);
            __ABS_currentTask.setLocalVariable("rtype",rtype);
        }
         {
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("abs\\lang\\abslang.abs",0);
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("abs\\lang\\abslang.abs",885);
            decrementResources(amount, rtype);
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("abs\\lang\\abslang.abs",886);
            abs.backend.java.lib.runtime.ABSRuntime.getCurrentRuntime().asyncCall(new abs.backend.java.lib.runtime.AbstractAsyncCallRT<ABS.DC.DeploymentComponent_i>(
                this,
                abs.backend.java.lib.runtime.ABSRuntime.checkForNull(target),
                new ABS.StdLib.Duration_InfDuration(),
                new ABS.StdLib.Duration_InfDuration(),
                abs.backend.java.lib.types.ABSBool.FALSE) {
                    abs.backend.java.lib.types.ABSRational arg0;
                    ABS.DC.Resourcetype arg1;
                    public java.util.List<abs.backend.java.lib.types.ABSValue> getArgs() {
                        return java.util.Arrays.asList(new abs.backend.java.lib.types.ABSValue[] {
                            arg0,arg1});
                    }
                    public abs.backend.java.lib.runtime.AsyncCall<?> init(abs.backend.java.lib.types.ABSRational _arg0,ABS.DC.Resourcetype _arg1) {
                        arg0 = _arg0;
                        arg1 = _arg1;
                        return this;
                    }
                    public java.lang.String methodName() {
                        return "incrementResources";
                    }
                    public Object execute() {
                        return target.incrementResources(arg0
                        ,arg1
                        );
                    }
                }.init(amount, rtype))
            ;
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("abs\\lang\\abslang.abs",0);
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().getCurrentTask().popStackFrame();
            return abs.backend.java.lib.types.ABSUnit.UNIT;
        }
    }
    // abslang.abs:0:0: 
    public final abs.backend.java.lib.runtime.ABSFut<abs.backend.java.lib.types.ABSUnit> async_decrementResources(abs.backend.java.lib.types.ABSRational amount, ABS.DC.Resourcetype rtype) {
        return (abs.backend.java.lib.runtime.ABSFut)abs.backend.java.lib.runtime.ABSRuntime.getCurrentRuntime().asyncCall(new abs.backend.java.lib.runtime.AbstractAsyncCallRT<ABS.DC.DeploymentComponent_c>(
            this,
            abs.backend.java.lib.runtime.ABSRuntime.checkForNull(this),
            new ABS.StdLib.Duration_InfDuration(),
            new ABS.StdLib.Duration_InfDuration(),
            abs.backend.java.lib.types.ABSBool.FALSE) {
                abs.backend.java.lib.types.ABSRational arg0;
                ABS.DC.Resourcetype arg1;
                public java.util.List<abs.backend.java.lib.types.ABSValue> getArgs() {
                    return java.util.Arrays.asList(new abs.backend.java.lib.types.ABSValue[] {
                        arg0,arg1});
                }
                public abs.backend.java.lib.runtime.AsyncCall<?> init(abs.backend.java.lib.types.ABSRational _arg0,ABS.DC.Resourcetype _arg1) {
                    arg0 = _arg0;
                    arg1 = _arg1;
                    return this;
                }
                public java.lang.String methodName() {
                    return "decrementResources";
                }
                public Object execute() {
                    return target.decrementResources(arg0
                    ,arg1
                    );
                }
            }.init(amount, rtype))
        ;
    }
    // abslang.abs:0:0: 
    public final abs.backend.java.lib.types.ABSUnit decrementResources(abs.backend.java.lib.types.ABSRational amount, ABS.DC.Resourcetype rtype) {
        __ABS_checkSameCOG(); 
        if (__ABS_getRuntime().debuggingEnabled()) {
            abs.backend.java.lib.runtime.Task<?> __ABS_currentTask = __ABS_getRuntime().getCurrentTask();
            __ABS_currentTask.newStackFrame(this, "decrementResources");
            __ABS_currentTask.setLocalVariable("amount",amount);
            __ABS_currentTask.setLocalVariable("rtype",rtype);
        }
         {
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("abs\\lang\\abslang.abs",0);
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("abs\\lang\\abslang.abs",890);
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("abs\\lang\\abslang.abs",0);
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().getCurrentTask().popStackFrame();
            return abs.backend.java.lib.types.ABSUnit.UNIT;
        }
    }
    // abslang.abs:0:0: 
    public final abs.backend.java.lib.runtime.ABSFut<abs.backend.java.lib.types.ABSUnit> async_incrementResources(abs.backend.java.lib.types.ABSRational amount, ABS.DC.Resourcetype rtype) {
        return (abs.backend.java.lib.runtime.ABSFut)abs.backend.java.lib.runtime.ABSRuntime.getCurrentRuntime().asyncCall(new abs.backend.java.lib.runtime.AbstractAsyncCallRT<ABS.DC.DeploymentComponent_c>(
            this,
            abs.backend.java.lib.runtime.ABSRuntime.checkForNull(this),
            new ABS.StdLib.Duration_InfDuration(),
            new ABS.StdLib.Duration_InfDuration(),
            abs.backend.java.lib.types.ABSBool.FALSE) {
                abs.backend.java.lib.types.ABSRational arg0;
                ABS.DC.Resourcetype arg1;
                public java.util.List<abs.backend.java.lib.types.ABSValue> getArgs() {
                    return java.util.Arrays.asList(new abs.backend.java.lib.types.ABSValue[] {
                        arg0,arg1});
                }
                public abs.backend.java.lib.runtime.AsyncCall<?> init(abs.backend.java.lib.types.ABSRational _arg0,ABS.DC.Resourcetype _arg1) {
                    arg0 = _arg0;
                    arg1 = _arg1;
                    return this;
                }
                public java.lang.String methodName() {
                    return "incrementResources";
                }
                public Object execute() {
                    return target.incrementResources(arg0
                    ,arg1
                    );
                }
            }.init(amount, rtype))
        ;
    }
    // abslang.abs:0:0: 
    public final abs.backend.java.lib.types.ABSUnit incrementResources(abs.backend.java.lib.types.ABSRational amount, ABS.DC.Resourcetype rtype) {
        __ABS_checkSameCOG(); 
        if (__ABS_getRuntime().debuggingEnabled()) {
            abs.backend.java.lib.runtime.Task<?> __ABS_currentTask = __ABS_getRuntime().getCurrentTask();
            __ABS_currentTask.newStackFrame(this, "incrementResources");
            __ABS_currentTask.setLocalVariable("amount",amount);
            __ABS_currentTask.setLocalVariable("rtype",rtype);
        }
         {
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("abs\\lang\\abslang.abs",0);
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("abs\\lang\\abslang.abs",908);
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("abs\\lang\\abslang.abs",0);
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().getCurrentTask().popStackFrame();
            return abs.backend.java.lib.types.ABSUnit.UNIT;
        }
    }
    // abslang.abs:0:0: 
    public final abs.backend.java.lib.runtime.ABSFut<abs.backend.java.lib.types.ABSUnit> async_setProvider(ABS.DC.CloudProvider_i provider) {
        return (abs.backend.java.lib.runtime.ABSFut)abs.backend.java.lib.runtime.ABSRuntime.getCurrentRuntime().asyncCall(new abs.backend.java.lib.runtime.AbstractAsyncCallRT<ABS.DC.DeploymentComponent_c>(
            this,
            abs.backend.java.lib.runtime.ABSRuntime.checkForNull(this),
            new ABS.StdLib.Duration_InfDuration(),
            new ABS.StdLib.Duration_InfDuration(),
            abs.backend.java.lib.types.ABSBool.FALSE) {
                ABS.DC.CloudProvider_i arg0;
                public java.util.List<abs.backend.java.lib.types.ABSValue> getArgs() {
                    return java.util.Arrays.asList(new abs.backend.java.lib.types.ABSValue[] {
                        arg0});
                }
                public abs.backend.java.lib.runtime.AsyncCall<?> init(ABS.DC.CloudProvider_i _arg0) {
                    arg0 = _arg0;
                    return this;
                }
                public java.lang.String methodName() {
                    return "setProvider";
                }
                public Object execute() {
                    return target.setProvider(arg0
                    );
                }
            }.init(provider))
        ;
    }
    // abslang.abs:0:0: 
    public final abs.backend.java.lib.types.ABSUnit setProvider(ABS.DC.CloudProvider_i provider) {
        __ABS_checkSameCOG(); 
        if (__ABS_getRuntime().debuggingEnabled()) {
            abs.backend.java.lib.runtime.Task<?> __ABS_currentTask = __ABS_getRuntime().getCurrentTask();
            __ABS_currentTask.newStackFrame(this, "setProvider");
            __ABS_currentTask.setLocalVariable("provider",provider);
        }
         {
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("abs\\lang\\abslang.abs",0);
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("abs\\lang\\abslang.abs",923);
            DeploymentComponent_c.this.cloudprovider = provider;
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("abs\\lang\\abslang.abs",0);
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().getCurrentTask().popStackFrame();
            return abs.backend.java.lib.types.ABSUnit.UNIT;
        }
    }
    // abslang.abs:0:0: 
    public final abs.backend.java.lib.runtime.ABSFut<ABS.DC.CloudProvider_i> async_getProvider() {
        return (abs.backend.java.lib.runtime.ABSFut)abs.backend.java.lib.runtime.ABSRuntime.getCurrentRuntime().asyncCall(new abs.backend.java.lib.runtime.AbstractAsyncCallRT<ABS.DC.DeploymentComponent_c>(
            this,
            abs.backend.java.lib.runtime.ABSRuntime.checkForNull(this),
            new ABS.StdLib.Duration_InfDuration(),
            new ABS.StdLib.Duration_InfDuration(),
            abs.backend.java.lib.types.ABSBool.FALSE) {
                public java.util.List<abs.backend.java.lib.types.ABSValue> getArgs() {
                    return java.util.Arrays.asList(new abs.backend.java.lib.types.ABSValue[] {
                    });
                }
                public abs.backend.java.lib.runtime.AsyncCall<?> init() {
                    return this;
                }
                public java.lang.String methodName() {
                    return "getProvider";
                }
                public Object execute() {
                    return target.getProvider();
                }
            }.init())
        ;
    }
    // abslang.abs:0:0: 
    public final ABS.DC.CloudProvider_i getProvider() {
        __ABS_checkSameCOG(); 
        if (__ABS_getRuntime().debuggingEnabled()) {
            abs.backend.java.lib.runtime.Task<?> __ABS_currentTask = __ABS_getRuntime().getCurrentTask();
            __ABS_currentTask.newStackFrame(this, "getProvider");
        }
         {
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("abs\\lang\\abslang.abs",0);
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("abs\\lang\\abslang.abs",925);
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().getCurrentTask().popStackFrame();
            return DeploymentComponent_c.this.cloudprovider;
        }
    }
    // abslang.abs:0:0: 
    public final abs.backend.java.lib.runtime.ABSFut<abs.backend.java.lib.types.ABSBool> async_acquire() {
        return (abs.backend.java.lib.runtime.ABSFut)abs.backend.java.lib.runtime.ABSRuntime.getCurrentRuntime().asyncCall(new abs.backend.java.lib.runtime.AbstractAsyncCallRT<ABS.DC.DeploymentComponent_c>(
            this,
            abs.backend.java.lib.runtime.ABSRuntime.checkForNull(this),
            new ABS.StdLib.Duration_InfDuration(),
            new ABS.StdLib.Duration_InfDuration(),
            abs.backend.java.lib.types.ABSBool.FALSE) {
                public java.util.List<abs.backend.java.lib.types.ABSValue> getArgs() {
                    return java.util.Arrays.asList(new abs.backend.java.lib.types.ABSValue[] {
                    });
                }
                public abs.backend.java.lib.runtime.AsyncCall<?> init() {
                    return this;
                }
                public java.lang.String methodName() {
                    return "acquire";
                }
                public Object execute() {
                    return target.acquire();
                }
            }.init())
        ;
    }
    // abslang.abs:0:0: 
    public final abs.backend.java.lib.types.ABSBool acquire() {
        __ABS_checkSameCOG(); 
        if (__ABS_getRuntime().debuggingEnabled()) {
            abs.backend.java.lib.runtime.Task<?> __ABS_currentTask = __ABS_getRuntime().getCurrentTask();
            __ABS_currentTask.newStackFrame(this, "acquire");
        }
         {
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("abs\\lang\\abslang.abs",0);
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("abs\\lang\\abslang.abs",927);
            abs.backend.java.lib.types.ABSBool result = abs.backend.java.lib.types.ABSBool.TRUE;
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().getCurrentTask().setLocalVariable("result",result);
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("abs\\lang\\abslang.abs",928);
            if (abs.backend.java.lib.expr.BinOp.notEq(DeploymentComponent_c.this.cloudprovider,null).toBoolean()) {
                 {
                    if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("abs\\lang\\abslang.abs",928);
                    if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("abs\\lang\\abslang.abs",929);
                    abs.backend.java.lib.runtime.ABSFut tmp326692514 = abs.backend.java.lib.runtime.ABSRuntime.getCurrentRuntime().asyncCall(new abs.backend.java.lib.runtime.AbstractAsyncCallRT<ABS.DC.CloudProvider_i>(
                        this,
                        abs.backend.java.lib.runtime.ABSRuntime.checkForNull(DeploymentComponent_c.this.cloudprovider),
                        new ABS.StdLib.Duration_InfDuration(),
                        new ABS.StdLib.Duration_InfDuration(),
                        abs.backend.java.lib.types.ABSBool.FALSE) {
                            ABS.DC.DeploymentComponent_i arg0;
                            public java.util.List<abs.backend.java.lib.types.ABSValue> getArgs() {
                                return java.util.Arrays.asList(new abs.backend.java.lib.types.ABSValue[] {
                                    arg0});
                            }
                            public abs.backend.java.lib.runtime.AsyncCall<?> init(ABS.DC.DeploymentComponent_i _arg0) {
                                arg0 = _arg0;
                                return this;
                            }
                            public java.lang.String methodName() {
                                return "acquireInstance";
                            }
                            public Object execute() {
                                return target.acquireInstance(arg0
                                );
                            }
                        }.init(this))
                    ;
                    if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().getCurrentTask().setLocalVariable("tmp326692514",tmp326692514);
                    if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("abs\\lang\\abslang.abs",929);
                    abs.backend.java.lib.runtime.ABSRuntime.await(new abs.backend.java.lib.runtime.ABSFutureGuard(tmp326692514));
                    if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("abs\\lang\\abslang.abs",929);
                    result = (abs.backend.java.lib.types.ABSBool)tmp326692514.get();
                    if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().getCurrentTask().setLocalVariable("result", result);}
            }
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("abs\\lang\\abslang.abs",931);
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().getCurrentTask().popStackFrame();
            return result;
        }
    }
    // abslang.abs:0:0: 
    public final abs.backend.java.lib.runtime.ABSFut<abs.backend.java.lib.types.ABSBool> async_release() {
        return (abs.backend.java.lib.runtime.ABSFut)abs.backend.java.lib.runtime.ABSRuntime.getCurrentRuntime().asyncCall(new abs.backend.java.lib.runtime.AbstractAsyncCallRT<ABS.DC.DeploymentComponent_c>(
            this,
            abs.backend.java.lib.runtime.ABSRuntime.checkForNull(this),
            new ABS.StdLib.Duration_InfDuration(),
            new ABS.StdLib.Duration_InfDuration(),
            abs.backend.java.lib.types.ABSBool.FALSE) {
                public java.util.List<abs.backend.java.lib.types.ABSValue> getArgs() {
                    return java.util.Arrays.asList(new abs.backend.java.lib.types.ABSValue[] {
                    });
                }
                public abs.backend.java.lib.runtime.AsyncCall<?> init() {
                    return this;
                }
                public java.lang.String methodName() {
                    return "release";
                }
                public Object execute() {
                    return target.release();
                }
            }.init())
        ;
    }
    // abslang.abs:0:0: 
    public final abs.backend.java.lib.types.ABSBool release() {
        __ABS_checkSameCOG(); 
        if (__ABS_getRuntime().debuggingEnabled()) {
            abs.backend.java.lib.runtime.Task<?> __ABS_currentTask = __ABS_getRuntime().getCurrentTask();
            __ABS_currentTask.newStackFrame(this, "release");
        }
         {
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("abs\\lang\\abslang.abs",0);
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("abs\\lang\\abslang.abs",934);
            abs.backend.java.lib.types.ABSBool result = abs.backend.java.lib.types.ABSBool.TRUE;
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().getCurrentTask().setLocalVariable("result",result);
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("abs\\lang\\abslang.abs",935);
            if (abs.backend.java.lib.expr.BinOp.notEq(DeploymentComponent_c.this.cloudprovider,null).toBoolean()) {
                 {
                    if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("abs\\lang\\abslang.abs",935);
                    if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("abs\\lang\\abslang.abs",936);
                    abs.backend.java.lib.runtime.ABSFut tmp34150451 = abs.backend.java.lib.runtime.ABSRuntime.getCurrentRuntime().asyncCall(new abs.backend.java.lib.runtime.AbstractAsyncCallRT<ABS.DC.CloudProvider_i>(
                        this,
                        abs.backend.java.lib.runtime.ABSRuntime.checkForNull(DeploymentComponent_c.this.cloudprovider),
                        new ABS.StdLib.Duration_InfDuration(),
                        new ABS.StdLib.Duration_InfDuration(),
                        abs.backend.java.lib.types.ABSBool.FALSE) {
                            ABS.DC.DeploymentComponent_i arg0;
                            public java.util.List<abs.backend.java.lib.types.ABSValue> getArgs() {
                                return java.util.Arrays.asList(new abs.backend.java.lib.types.ABSValue[] {
                                    arg0});
                            }
                            public abs.backend.java.lib.runtime.AsyncCall<?> init(ABS.DC.DeploymentComponent_i _arg0) {
                                arg0 = _arg0;
                                return this;
                            }
                            public java.lang.String methodName() {
                                return "releaseInstance";
                            }
                            public Object execute() {
                                return target.releaseInstance(arg0
                                );
                            }
                        }.init(this))
                    ;
                    if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().getCurrentTask().setLocalVariable("tmp34150451",tmp34150451);
                    if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("abs\\lang\\abslang.abs",936);
                    abs.backend.java.lib.runtime.ABSRuntime.await(new abs.backend.java.lib.runtime.ABSFutureGuard(tmp34150451));
                    if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("abs\\lang\\abslang.abs",936);
                    result = (abs.backend.java.lib.types.ABSBool)tmp34150451.get();
                    if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().getCurrentTask().setLocalVariable("result", result);}
            }
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("abs\\lang\\abslang.abs",938);
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().getCurrentTask().popStackFrame();
            return result;
        }
    }
    // abslang.abs:0:0: 
    public final abs.backend.java.lib.runtime.ABSFut<ABS.StdLib.Time> async_getCreationTime() {
        return (abs.backend.java.lib.runtime.ABSFut)abs.backend.java.lib.runtime.ABSRuntime.getCurrentRuntime().asyncCall(new abs.backend.java.lib.runtime.AbstractAsyncCallRT<ABS.DC.DeploymentComponent_c>(
            this,
            abs.backend.java.lib.runtime.ABSRuntime.checkForNull(this),
            new ABS.StdLib.Duration_InfDuration(),
            new ABS.StdLib.Duration_InfDuration(),
            abs.backend.java.lib.types.ABSBool.FALSE) {
                public java.util.List<abs.backend.java.lib.types.ABSValue> getArgs() {
                    return java.util.Arrays.asList(new abs.backend.java.lib.types.ABSValue[] {
                    });
                }
                public abs.backend.java.lib.runtime.AsyncCall<?> init() {
                    return this;
                }
                public java.lang.String methodName() {
                    return "getCreationTime";
                }
                public Object execute() {
                    return target.getCreationTime();
                }
            }.init())
        ;
    }
    // abslang.abs:0:0: 
    public final ABS.StdLib.Time getCreationTime() {
        __ABS_checkSameCOG(); 
        if (__ABS_getRuntime().debuggingEnabled()) {
            abs.backend.java.lib.runtime.Task<?> __ABS_currentTask = __ABS_getRuntime().getCurrentTask();
            __ABS_currentTask.newStackFrame(this, "getCreationTime");
        }
         {
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("abs\\lang\\abslang.abs",0);
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("abs\\lang\\abslang.abs",941);
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().getCurrentTask().popStackFrame();
            return DeploymentComponent_c.this.creationTime;
        }
    }
    // abslang.abs:0:0: 
    public final abs.backend.java.lib.runtime.ABSFut<abs.backend.java.lib.types.ABSRational> async_getStartupDuration() {
        return (abs.backend.java.lib.runtime.ABSFut)abs.backend.java.lib.runtime.ABSRuntime.getCurrentRuntime().asyncCall(new abs.backend.java.lib.runtime.AbstractAsyncCallRT<ABS.DC.DeploymentComponent_c>(
            this,
            abs.backend.java.lib.runtime.ABSRuntime.checkForNull(this),
            new ABS.StdLib.Duration_InfDuration(),
            new ABS.StdLib.Duration_InfDuration(),
            abs.backend.java.lib.types.ABSBool.FALSE) {
                public java.util.List<abs.backend.java.lib.types.ABSValue> getArgs() {
                    return java.util.Arrays.asList(new abs.backend.java.lib.types.ABSValue[] {
                    });
                }
                public abs.backend.java.lib.runtime.AsyncCall<?> init() {
                    return this;
                }
                public java.lang.String methodName() {
                    return "getStartupDuration";
                }
                public Object execute() {
                    return target.getStartupDuration();
                }
            }.init())
        ;
    }
    // abslang.abs:0:0: 
    public final abs.backend.java.lib.types.ABSRational getStartupDuration() {
        __ABS_checkSameCOG(); 
        if (__ABS_getRuntime().debuggingEnabled()) {
            abs.backend.java.lib.runtime.Task<?> __ABS_currentTask = __ABS_getRuntime().getCurrentTask();
            __ABS_currentTask.newStackFrame(this, "getStartupDuration");
        }
         {
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("abs\\lang\\abslang.abs",0);
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("abs\\lang\\abslang.abs",942);
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().getCurrentTask().popStackFrame();
            return DeploymentComponent_c.this.startupDuration;
        }
    }
    // abslang.abs:0:0: 
    public final abs.backend.java.lib.runtime.ABSFut<abs.backend.java.lib.types.ABSInteger> async_getPaymentInterval() {
        return (abs.backend.java.lib.runtime.ABSFut)abs.backend.java.lib.runtime.ABSRuntime.getCurrentRuntime().asyncCall(new abs.backend.java.lib.runtime.AbstractAsyncCallRT<ABS.DC.DeploymentComponent_c>(
            this,
            abs.backend.java.lib.runtime.ABSRuntime.checkForNull(this),
            new ABS.StdLib.Duration_InfDuration(),
            new ABS.StdLib.Duration_InfDuration(),
            abs.backend.java.lib.types.ABSBool.FALSE) {
                public java.util.List<abs.backend.java.lib.types.ABSValue> getArgs() {
                    return java.util.Arrays.asList(new abs.backend.java.lib.types.ABSValue[] {
                    });
                }
                public abs.backend.java.lib.runtime.AsyncCall<?> init() {
                    return this;
                }
                public java.lang.String methodName() {
                    return "getPaymentInterval";
                }
                public Object execute() {
                    return target.getPaymentInterval();
                }
            }.init())
        ;
    }
    // abslang.abs:0:0: 
    public final abs.backend.java.lib.types.ABSInteger getPaymentInterval() {
        __ABS_checkSameCOG(); 
        if (__ABS_getRuntime().debuggingEnabled()) {
            abs.backend.java.lib.runtime.Task<?> __ABS_currentTask = __ABS_getRuntime().getCurrentTask();
            __ABS_currentTask.newStackFrame(this, "getPaymentInterval");
        }
         {
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("abs\\lang\\abslang.abs",0);
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("abs\\lang\\abslang.abs",943);
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().getCurrentTask().popStackFrame();
            return DeploymentComponent_c.this.paymentInterval;
        }
    }
    // abslang.abs:0:0: 
    public final abs.backend.java.lib.runtime.ABSFut<abs.backend.java.lib.types.ABSRational> async_getCostPerInterval() {
        return (abs.backend.java.lib.runtime.ABSFut)abs.backend.java.lib.runtime.ABSRuntime.getCurrentRuntime().asyncCall(new abs.backend.java.lib.runtime.AbstractAsyncCallRT<ABS.DC.DeploymentComponent_c>(
            this,
            abs.backend.java.lib.runtime.ABSRuntime.checkForNull(this),
            new ABS.StdLib.Duration_InfDuration(),
            new ABS.StdLib.Duration_InfDuration(),
            abs.backend.java.lib.types.ABSBool.FALSE) {
                public java.util.List<abs.backend.java.lib.types.ABSValue> getArgs() {
                    return java.util.Arrays.asList(new abs.backend.java.lib.types.ABSValue[] {
                    });
                }
                public abs.backend.java.lib.runtime.AsyncCall<?> init() {
                    return this;
                }
                public java.lang.String methodName() {
                    return "getCostPerInterval";
                }
                public Object execute() {
                    return target.getCostPerInterval();
                }
            }.init())
        ;
    }
    // abslang.abs:0:0: 
    public final abs.backend.java.lib.types.ABSRational getCostPerInterval() {
        __ABS_checkSameCOG(); 
        if (__ABS_getRuntime().debuggingEnabled()) {
            abs.backend.java.lib.runtime.Task<?> __ABS_currentTask = __ABS_getRuntime().getCurrentTask();
            __ABS_currentTask.newStackFrame(this, "getCostPerInterval");
        }
         {
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("abs\\lang\\abslang.abs",0);
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("abs\\lang\\abslang.abs",944);
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().getCurrentTask().popStackFrame();
            return DeploymentComponent_c.this.costPerInterval;
        }
    }
    // abslang.abs:0:0: 
    public final abs.backend.java.lib.runtime.ABSFut<abs.backend.java.lib.types.ABSBool> async_matchesDescription(ABS.StdLib.Map<ABS.DC.Resourcetype,abs.backend.java.lib.types.ABSRational> description) {
        return (abs.backend.java.lib.runtime.ABSFut)abs.backend.java.lib.runtime.ABSRuntime.getCurrentRuntime().asyncCall(new abs.backend.java.lib.runtime.AbstractAsyncCallRT<ABS.DC.DeploymentComponent_c>(
            this,
            abs.backend.java.lib.runtime.ABSRuntime.checkForNull(this),
            new ABS.StdLib.Duration_InfDuration(),
            new ABS.StdLib.Duration_InfDuration(),
            abs.backend.java.lib.types.ABSBool.FALSE) {
                ABS.StdLib.Map<ABS.DC.Resourcetype,abs.backend.java.lib.types.ABSRational> arg0;
                public java.util.List<abs.backend.java.lib.types.ABSValue> getArgs() {
                    return java.util.Arrays.asList(new abs.backend.java.lib.types.ABSValue[] {
                        arg0});
                }
                public abs.backend.java.lib.runtime.AsyncCall<?> init(ABS.StdLib.Map<ABS.DC.Resourcetype,abs.backend.java.lib.types.ABSRational> _arg0) {
                    arg0 = _arg0;
                    return this;
                }
                public java.lang.String methodName() {
                    return "matchesDescription";
                }
                public Object execute() {
                    return target.matchesDescription(arg0
                    );
                }
            }.init(description))
        ;
    }
    // abslang.abs:0:0: 
    public final abs.backend.java.lib.types.ABSBool matchesDescription(ABS.StdLib.Map<ABS.DC.Resourcetype,abs.backend.java.lib.types.ABSRational> description) {
        __ABS_checkSameCOG(); 
        if (__ABS_getRuntime().debuggingEnabled()) {
            abs.backend.java.lib.runtime.Task<?> __ABS_currentTask = __ABS_getRuntime().getCurrentTask();
            __ABS_currentTask.newStackFrame(this, "matchesDescription");
            __ABS_currentTask.setLocalVariable("description",description);
        }
         {
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("abs\\lang\\abslang.abs",0);
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("abs\\lang\\abslang.abs",949);
            abs.backend.java.lib.types.ABSBool result = abs.backend.java.lib.types.ABSBool.TRUE;
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().getCurrentTask().setLocalVariable("result",result);
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("abs\\lang\\abslang.abs",950);
            ABS.StdLib.Set<ABS.DC.Resourcetype> keys = ABS.StdLib.keys_f.apply(description);
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().getCurrentTask().setLocalVariable("keys",keys);
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("abs\\lang\\abslang.abs",951);
            while (abs.backend.java.lib.expr.BinOp.eq(result,abs.backend.java.lib.types.ABSBool.TRUE).and(ABS.StdLib.emptySet_f.apply(keys).negate()).toBoolean()) {
                if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("abs\\lang\\abslang.abs",951);
                if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("abs\\lang\\abslang.abs",952);
                ABS.DC.Resourcetype key = ABS.StdLib.take_f.apply(keys);
                if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().getCurrentTask().setLocalVariable("key",key);
                if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("abs\\lang\\abslang.abs",953);
                keys = ABS.StdLib.remove_f.apply(keys, key);
                if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().getCurrentTask().setLocalVariable("keys", keys);if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("abs\\lang\\abslang.abs",954);
                abs.backend.java.lib.types.ABSRational value = ABS.StdLib.lookupUnsafe_f.apply(description, key);
                if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().getCurrentTask().setLocalVariable("value",value);
                if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("abs\\lang\\abslang.abs",955);
            }
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("abs\\lang\\abslang.abs",970);
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().getCurrentTask().popStackFrame();
            return result;
        }
    }
}
