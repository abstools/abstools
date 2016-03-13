package ABS.DC;
// abslang.abs:686:0: 
public final class CloudProvider_c extends abs.backend.java.lib.runtime.ABSObject implements abs.backend.java.lib.types.ABSClass, ABS.DC.CloudProvider_i {
    private static final java.lang.String[] __fieldNames = new java.lang.String[] { "name", "launchedInstances", "acquiredInstances", "killedInstances", "nextInstanceId", "accumulatedCost", "keeprunning" };
    public final java.util.List<java.lang.String> getFieldNames() { return java.util.Arrays.asList(__fieldNames); }
    private abs.backend.java.lib.types.ABSString name;
    // abslang.abs:690:4: 
    private ABS.StdLib.Set<ABS.DC.DeploymentComponent_i> launchedInstances;
    // abslang.abs:692:4: 
    private ABS.StdLib.Set<ABS.DC.DeploymentComponent_i> acquiredInstances;
    // abslang.abs:693:4: 
    private ABS.StdLib.Set<ABS.DC.DeploymentComponent_i> killedInstances;
    // abslang.abs:694:4: 
    private abs.backend.java.lib.types.ABSInteger nextInstanceId;
    // abslang.abs:695:4: 
    private abs.backend.java.lib.types.ABSRational accumulatedCost;
    // abslang.abs:696:4: 
    private abs.backend.java.lib.types.ABSBool keeprunning;
    public CloudProvider_c(abs.backend.java.lib.types.ABSString name) {
        this.name = name;
        getCOG().objectCreated(this);
    }
    protected final void __ABS_init() {
        this.launchedInstances = ABS.StdLib.set_f.apply(new ABS.StdLib.List_Nil());this.acquiredInstances = ABS.StdLib.set_f.apply(new ABS.StdLib.List_Nil());this.killedInstances = ABS.StdLib.set_f.apply(new ABS.StdLib.List_Nil());this.nextInstanceId = abs.backend.java.lib.types.ABSInteger.fromString("0");this.accumulatedCost = abs.backend.java.lib.types.ABSInteger.fromString("0");this.keeprunning = abs.backend.java.lib.types.ABSBool.TRUE;getCOG().objectInitialized(this);
    }
    protected final abs.backend.java.lib.types.ABSValue getFieldValue(java.lang.String __ABS_fieldName) throws java.lang.NoSuchFieldException {
        if ("name".equals(__ABS_fieldName)) return name;
        if ("launchedInstances".equals(__ABS_fieldName)) return launchedInstances;
        if ("acquiredInstances".equals(__ABS_fieldName)) return acquiredInstances;
        if ("killedInstances".equals(__ABS_fieldName)) return killedInstances;
        if ("nextInstanceId".equals(__ABS_fieldName)) return nextInstanceId;
        if ("accumulatedCost".equals(__ABS_fieldName)) return accumulatedCost;
        if ("keeprunning".equals(__ABS_fieldName)) return keeprunning;
        return super.getFieldValue(__ABS_fieldName);
    }
    public final java.lang.String getClassName() { return "CloudProvider"; }
    public static final <T extends CloudProvider_c> T createNewCOG(abs.backend.java.lib.types.ABSString name) { return (T)CloudProvider_c.__ABS_createNewCOG(null, null, name); }
    public static final <T extends CloudProvider_c> T __ABS_createNewCOG(abs.backend.java.lib.runtime.ABSObject __ABS_source, abs.backend.java.scheduling.UserSchedulingStrategy strategy, abs.backend.java.lib.types.ABSString name) {
        final abs.backend.java.lib.runtime.ABSRuntime __ABS_runtime = abs.backend.java.lib.runtime.ABSRuntime.getCurrentRuntime();
        final abs.backend.java.lib.runtime.COG __ABS_cog = strategy == null ? __ABS_runtime.createCOG(CloudProvider_c.class) : __ABS_runtime.createCOG(CloudProvider_c.class, strategy);
        final abs.backend.java.lib.runtime.ABSThread __ABS_thread = abs.backend.java.lib.runtime.ABSRuntime.getCurrentThread();
        final abs.backend.java.lib.runtime.COG __ABS_oldCOG = abs.backend.java.lib.runtime.ABSRuntime.getCurrentCOG();
        final abs.backend.java.lib.runtime.Task __ABS_sendingTask = abs.backend.java.lib.runtime.ABSRuntime.getCurrentTask();
        __ABS_thread.setCOG(__ABS_cog);
        try {
            CloudProvider_c __ABS_result = new CloudProvider_c(name);
            ;
            __ABS_runtime.cogCreated(__ABS_result);
            __ABS_cog.getScheduler().addTask(new abs.backend.java.lib.runtime.Task(new abs.backend.java.lib.runtime.ABSInitObjectCall(__ABS_sendingTask,__ABS_source,__ABS_result)));
            return (T)__ABS_result;
        } finally {
            __ABS_thread.setCOG(__ABS_oldCOG);
        }
    }
    public static final <T extends CloudProvider_c> T createNewObject(abs.backend.java.lib.types.ABSString name) { return (T)CloudProvider_c.__ABS_createNewObject(null, name); }
    public static final <T extends CloudProvider_c> T __ABS_createNewObject(abs.backend.java.lib.runtime.ABSObject __ABS_source, abs.backend.java.lib.types.ABSString name) {
        CloudProvider_c __ABS_result = new CloudProvider_c(name);
        __ABS_result.__ABS_init();
        return (T)__ABS_result;
    }
    // abslang.abs:0:0: 
    public final abs.backend.java.lib.runtime.ABSFut<abs.backend.java.lib.types.ABSUnit> async_startAccounting() {
        return (abs.backend.java.lib.runtime.ABSFut)abs.backend.java.lib.runtime.ABSRuntime.getCurrentRuntime().asyncCall(new abs.backend.java.lib.runtime.AbstractAsyncCallRT<ABS.DC.CloudProvider_c>(
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
                    return "startAccounting";
                }
                public Object execute() {
                    return target.startAccounting();
                }
            }.init())
        ;
    }
    // abslang.abs:0:0: 
    public final abs.backend.java.lib.types.ABSUnit startAccounting() {
        __ABS_checkSameCOG(); 
        if (__ABS_getRuntime().debuggingEnabled()) {
            abs.backend.java.lib.runtime.Task<?> __ABS_currentTask = __ABS_getRuntime().getCurrentTask();
            __ABS_currentTask.newStackFrame(this, "startAccounting");
        }
         {
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("abs/lang/abslang.abs",0);
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("abs/lang/abslang.abs",699);
            while (CloudProvider_c.this.keeprunning.and(ABS.StdLib.emptySet_f.apply(CloudProvider_c.this.launchedInstances).negate()).toBoolean()) {
                if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("abs/lang/abslang.abs",699);
                if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("abs/lang/abslang.abs",700);
                abs.backend.java.lib.runtime.ABSRuntime.await(new abs.backend.java.lib.runtime.ABSExpGuard() { public abs.backend.java.lib.types.ABSBool evaluateExp() { return abs.backend.java.lib.types.ABSBool.TRUE; }});
                if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("abs/lang/abslang.abs",701);
                abs.backend.java.lib.types.ABSRational cost = sumOfCosts(CloudProvider_c.this.launchedInstances);
                if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().getCurrentTask().setLocalVariable("cost",cost);
                if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("abs/lang/abslang.abs",702);
                CloudProvider_c.this.accumulatedCost = CloudProvider_c.this.accumulatedCost.add(cost);
            }
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("abs/lang/abslang.abs",0);
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().getCurrentTask().popStackFrame();
            return abs.backend.java.lib.types.ABSUnit.UNIT;
        }
    }
    // abslang.abs:0:0: 
    public final abs.backend.java.lib.runtime.ABSFut<abs.backend.java.lib.types.ABSUnit> async_shutdown() {
        return (abs.backend.java.lib.runtime.ABSFut)abs.backend.java.lib.runtime.ABSRuntime.getCurrentRuntime().asyncCall(new abs.backend.java.lib.runtime.AbstractAsyncCallRT<ABS.DC.CloudProvider_c>(
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
                    return "shutdown";
                }
                public Object execute() {
                    return target.shutdown();
                }
            }.init())
        ;
    }
    // abslang.abs:0:0: 
    public final abs.backend.java.lib.types.ABSUnit shutdown() {
        __ABS_checkSameCOG(); 
        if (__ABS_getRuntime().debuggingEnabled()) {
            abs.backend.java.lib.runtime.Task<?> __ABS_currentTask = __ABS_getRuntime().getCurrentTask();
            __ABS_currentTask.newStackFrame(this, "shutdown");
        }
         {
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("abs/lang/abslang.abs",0);
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("abs/lang/abslang.abs",705);
            CloudProvider_c.this.keeprunning = abs.backend.java.lib.types.ABSBool.FALSE;
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("abs/lang/abslang.abs",0);
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().getCurrentTask().popStackFrame();
            return abs.backend.java.lib.types.ABSUnit.UNIT;
        }
    }
    // abslang.abs:0:0: 
    public final abs.backend.java.lib.runtime.ABSFut<abs.backend.java.lib.types.ABSRational> async_sumOfCosts(ABS.StdLib.Set<ABS.DC.DeploymentComponent_i> dcs) {
        return (abs.backend.java.lib.runtime.ABSFut)abs.backend.java.lib.runtime.ABSRuntime.getCurrentRuntime().asyncCall(new abs.backend.java.lib.runtime.AbstractAsyncCallRT<ABS.DC.CloudProvider_c>(
            this,
            abs.backend.java.lib.runtime.ABSRuntime.checkForNull(this),
            new ABS.StdLib.Duration_InfDuration(),
            new ABS.StdLib.Duration_InfDuration(),
            abs.backend.java.lib.types.ABSBool.FALSE) {
                ABS.StdLib.Set<ABS.DC.DeploymentComponent_i> arg0;
                public java.util.List<abs.backend.java.lib.types.ABSValue> getArgs() {
                    return java.util.Arrays.asList(new abs.backend.java.lib.types.ABSValue[] {
                        arg0});
                }
                public abs.backend.java.lib.runtime.AsyncCall<?> init(ABS.StdLib.Set<ABS.DC.DeploymentComponent_i> _arg0) {
                    arg0 = _arg0;
                    return this;
                }
                public java.lang.String methodName() {
                    return "sumOfCosts";
                }
                public Object execute() {
                    return target.sumOfCosts(arg0
                    );
                }
            }.init(dcs))
        ;
    }
    // abslang.abs:0:0: 
    public final abs.backend.java.lib.types.ABSRational sumOfCosts(ABS.StdLib.Set<ABS.DC.DeploymentComponent_i> dcs) {
        __ABS_checkSameCOG(); 
        if (__ABS_getRuntime().debuggingEnabled()) {
            abs.backend.java.lib.runtime.Task<?> __ABS_currentTask = __ABS_getRuntime().getCurrentTask();
            __ABS_currentTask.newStackFrame(this, "sumOfCosts");
            __ABS_currentTask.setLocalVariable("dcs",dcs);
        }
         {
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("abs/lang/abslang.abs",0);
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("abs/lang/abslang.abs",708);
            abs.backend.java.lib.types.ABSRational result = abs.backend.java.lib.types.ABSInteger.fromString("0");
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().getCurrentTask().setLocalVariable("result",result);
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("abs/lang/abslang.abs",709);
            ABS.StdLib.Time t = ABS.StdLib.now_f.apply();
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().getCurrentTask().setLocalVariable("t",t);
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("abs/lang/abslang.abs",710);
            while (ABS.StdLib.emptySet_f.apply(dcs).negate().toBoolean()) {
                if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("abs/lang/abslang.abs",710);
                if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("abs/lang/abslang.abs",711);
                ABS.DC.DeploymentComponent_i dc = ABS.StdLib.take_f.apply(dcs);
                if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().getCurrentTask().setLocalVariable("dc",dc);
                if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("abs/lang/abslang.abs",712);
                dcs = ABS.StdLib.remove_f.apply(dcs, dc);
                if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().getCurrentTask().setLocalVariable("dcs", dcs);if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("abs/lang/abslang.abs",713);
                abs.backend.java.lib.runtime.ABSFut tmp53301881 = abs.backend.java.lib.runtime.ABSRuntime.getCurrentRuntime().asyncCall(new abs.backend.java.lib.runtime.AbstractAsyncCallRT<ABS.DC.DeploymentComponent_i>(
                    this,
                    abs.backend.java.lib.runtime.ABSRuntime.checkForNull(dc),
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
                if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().getCurrentTask().setLocalVariable("tmp53301881",tmp53301881);
                if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("abs/lang/abslang.abs",713);
                abs.backend.java.lib.runtime.ABSRuntime.await(new abs.backend.java.lib.runtime.ABSFutureGuard(tmp53301881));
                if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("abs/lang/abslang.abs",713);
                abs.backend.java.lib.types.ABSRational cost = (abs.backend.java.lib.types.ABSRational)tmp53301881.get();
                if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().getCurrentTask().setLocalVariable("cost",cost);
                if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("abs/lang/abslang.abs",714);
                if (abs.backend.java.lib.expr.BinOp.gt(cost,abs.backend.java.lib.types.ABSInteger.fromString("0")).toBoolean()) {
                     {
                        if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("abs/lang/abslang.abs",714);
                        if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("abs/lang/abslang.abs",715);
                        abs.backend.java.lib.runtime.ABSFut tmp470187021 = abs.backend.java.lib.runtime.ABSRuntime.getCurrentRuntime().asyncCall(new abs.backend.java.lib.runtime.AbstractAsyncCallRT<ABS.DC.DeploymentComponent_i>(
                            this,
                            abs.backend.java.lib.runtime.ABSRuntime.checkForNull(dc),
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
                        if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().getCurrentTask().setLocalVariable("tmp470187021",tmp470187021);
                        if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("abs/lang/abslang.abs",715);
                        abs.backend.java.lib.runtime.ABSRuntime.await(new abs.backend.java.lib.runtime.ABSFutureGuard(tmp470187021));
                        if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("abs/lang/abslang.abs",715);
                        ABS.StdLib.Time creationtime = (ABS.StdLib.Time)tmp470187021.get();
                        if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().getCurrentTask().setLocalVariable("creationtime",creationtime);
                        if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("abs/lang/abslang.abs",716);
                        abs.backend.java.lib.types.ABSRational time_elapse = ABS.StdLib.timeDifference_f.apply(t, creationtime);
                        if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().getCurrentTask().setLocalVariable("time_elapse",time_elapse);
                        if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("abs/lang/abslang.abs",717);
                        abs.backend.java.lib.runtime.ABSFut tmp1561005241 = abs.backend.java.lib.runtime.ABSRuntime.getCurrentRuntime().asyncCall(new abs.backend.java.lib.runtime.AbstractAsyncCallRT<ABS.DC.DeploymentComponent_i>(
                            this,
                            abs.backend.java.lib.runtime.ABSRuntime.checkForNull(dc),
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
                        if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().getCurrentTask().setLocalVariable("tmp1561005241",tmp1561005241);
                        if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("abs/lang/abslang.abs",717);
                        abs.backend.java.lib.runtime.ABSRuntime.await(new abs.backend.java.lib.runtime.ABSFutureGuard(tmp1561005241));
                        if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("abs/lang/abslang.abs",717);
                        abs.backend.java.lib.types.ABSInteger interval = (abs.backend.java.lib.types.ABSInteger)tmp1561005241.get();
                        if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().getCurrentTask().setLocalVariable("interval",interval);
                        if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("abs/lang/abslang.abs",718);
                        if (abs.backend.java.lib.expr.BinOp.eq(time_elapse.mod(interval),abs.backend.java.lib.types.ABSInteger.fromString("0")).toBoolean()) {
                             {
                                if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("abs/lang/abslang.abs",718);
                                if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("abs/lang/abslang.abs",719);
                                result = result.add(cost);
                                if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().getCurrentTask().setLocalVariable("result", result);}
                        }
                    }
                }
            }
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("abs/lang/abslang.abs",723);
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().getCurrentTask().popStackFrame();
            return result;
        }
    }
    // abslang.abs:0:0: 
    public final abs.backend.java.lib.runtime.ABSFut<ABS.DC.DeploymentComponent_i> async_prelaunchInstance(ABS.StdLib.Map<ABS.DC.Resourcetype,abs.backend.java.lib.types.ABSRational> d) {
        return (abs.backend.java.lib.runtime.ABSFut)abs.backend.java.lib.runtime.ABSRuntime.getCurrentRuntime().asyncCall(new abs.backend.java.lib.runtime.AbstractAsyncCallRT<ABS.DC.CloudProvider_c>(
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
                    return "prelaunchInstance";
                }
                public Object execute() {
                    return target.prelaunchInstance(arg0
                    );
                }
            }.init(d))
        ;
    }
    // abslang.abs:0:0: 
    public final ABS.DC.DeploymentComponent_i prelaunchInstance(ABS.StdLib.Map<ABS.DC.Resourcetype,abs.backend.java.lib.types.ABSRational> d) {
        __ABS_checkSameCOG(); 
        if (__ABS_getRuntime().debuggingEnabled()) {
            abs.backend.java.lib.runtime.Task<?> __ABS_currentTask = __ABS_getRuntime().getCurrentTask();
            __ABS_currentTask.newStackFrame(this, "prelaunchInstance");
            __ABS_currentTask.setLocalVariable("d",d);
        }
         {
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("abs/lang/abslang.abs",0);
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("abs/lang/abslang.abs",729);
            abs.backend.java.lib.types.ABSBool needToStartAccounting = ABS.StdLib.emptySet_f.apply(CloudProvider_c.this.launchedInstances);
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().getCurrentTask().setLocalVariable("needToStartAccounting",needToStartAccounting);
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("abs/lang/abslang.abs",732);
            ABS.DC.DeploymentComponent_i result = findMatchingInstance(CloudProvider_c.this.killedInstances, d);
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().getCurrentTask().setLocalVariable("result",result);
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("abs/lang/abslang.abs",733);
            if (abs.backend.java.lib.expr.BinOp.notEq(result,null).toBoolean()) {
                 {
                    if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("abs/lang/abslang.abs",733);
                    if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("abs/lang/abslang.abs",734);
                    CloudProvider_c.this.killedInstances = ABS.StdLib.remove_f.apply(CloudProvider_c.this.killedInstances, result);
                    if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("abs/lang/abslang.abs",735);
                    CloudProvider_c.this.launchedInstances = ABS.StdLib.insertElement_f.apply(CloudProvider_c.this.launchedInstances, result);
                }
            }
            else {
                 {
                    if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("abs/lang/abslang.abs",736);
                    if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("abs/lang/abslang.abs",737);
                    result = ABS.DC.DeploymentComponent_c.__ABS_createNewCOG(this, null, CloudProvider_c.this.name.add(abs.backend.java.lib.types.ABSString.fromString("-")).add(abs.backend.java.lib.runtime.ABSBuiltInFunctions.toString(CloudProvider_c.this.nextInstanceId)), d);
                    if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().getCurrentTask().setLocalVariable("result", result);if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("abs/lang/abslang.abs",739);
                    CloudProvider_c.this.nextInstanceId = CloudProvider_c.this.nextInstanceId.add(abs.backend.java.lib.types.ABSInteger.fromString("1"));
                    if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("abs/lang/abslang.abs",740);
                    abs.backend.java.lib.runtime.ABSFut tmp27294710 = abs.backend.java.lib.runtime.ABSRuntime.getCurrentRuntime().asyncCall(new abs.backend.java.lib.runtime.AbstractAsyncCallRT<ABS.DC.DeploymentComponent_i>(
                        this,
                        abs.backend.java.lib.runtime.ABSRuntime.checkForNull(result),
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
                    if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().getCurrentTask().setLocalVariable("tmp27294710",tmp27294710);
                    if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("abs/lang/abslang.abs",740);
                    abs.backend.java.lib.runtime.ABSRuntime.await(new abs.backend.java.lib.runtime.ABSFutureGuard(tmp27294710));
                    if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("abs/lang/abslang.abs",740);
                    abs.backend.java.lib.types.ABSRational startup_duration = (abs.backend.java.lib.types.ABSRational)tmp27294710.get();
                    if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().getCurrentTask().setLocalVariable("startup_duration",startup_duration);
                    if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("abs/lang/abslang.abs",741);
                    abs.backend.java.lib.runtime.ABSRuntime.await(new abs.backend.java.lib.runtime.ABSExpGuard() { public abs.backend.java.lib.types.ABSBool evaluateExp() { return abs.backend.java.lib.types.ABSBool.TRUE; }});
                    if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("abs/lang/abslang.abs",742);
                    CloudProvider_c.this.launchedInstances = ABS.StdLib.insertElement_f.apply(CloudProvider_c.this.launchedInstances, result);
                    if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("abs/lang/abslang.abs",743);
                    abs.backend.java.lib.runtime.ABSRuntime.getCurrentRuntime().asyncCall(new abs.backend.java.lib.runtime.AbstractAsyncCallRT<ABS.DC.DeploymentComponent_i>(
                        this,
                        abs.backend.java.lib.runtime.ABSRuntime.checkForNull(result),
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
                        }.init(this))
                    ;
                }
            }
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("abs/lang/abslang.abs",745);
            if (needToStartAccounting.toBoolean()) {
                if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("abs/lang/abslang.abs",745);
                abs.backend.java.lib.runtime.ABSRuntime.getCurrentRuntime().asyncCall(new abs.backend.java.lib.runtime.AbstractAsyncCallRT<ABS.DC.CloudProvider_c>(
                    this,
                    this,
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
                            return "startAccounting";
                        }
                        public Object execute() {
                            return target.startAccounting();
                        }
                    }.init())
                ;
            }
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("abs/lang/abslang.abs",746);
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().getCurrentTask().popStackFrame();
            return result;
        }
    }
    // abslang.abs:0:0: 
    public final abs.backend.java.lib.runtime.ABSFut<ABS.DC.DeploymentComponent_i> async_launchInstance(ABS.StdLib.Map<ABS.DC.Resourcetype,abs.backend.java.lib.types.ABSRational> d) {
        return (abs.backend.java.lib.runtime.ABSFut)abs.backend.java.lib.runtime.ABSRuntime.getCurrentRuntime().asyncCall(new abs.backend.java.lib.runtime.AbstractAsyncCallRT<ABS.DC.CloudProvider_c>(
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
                    return "launchInstance";
                }
                public Object execute() {
                    return target.launchInstance(arg0
                    );
                }
            }.init(d))
        ;
    }
    // abslang.abs:0:0: 
    public final ABS.DC.DeploymentComponent_i launchInstance(ABS.StdLib.Map<ABS.DC.Resourcetype,abs.backend.java.lib.types.ABSRational> d) {
        __ABS_checkSameCOG(); 
        if (__ABS_getRuntime().debuggingEnabled()) {
            abs.backend.java.lib.runtime.Task<?> __ABS_currentTask = __ABS_getRuntime().getCurrentTask();
            __ABS_currentTask.newStackFrame(this, "launchInstance");
            __ABS_currentTask.setLocalVariable("d",d);
        }
         {
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("abs/lang/abslang.abs",0);
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("abs/lang/abslang.abs",752);
            ABS.DC.DeploymentComponent_i result = prelaunchInstance(d);
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().getCurrentTask().setLocalVariable("result",result);
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("abs/lang/abslang.abs",753);
            CloudProvider_c.this.acquiredInstances = ABS.StdLib.insertElement_f.apply(CloudProvider_c.this.acquiredInstances, result);
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("abs/lang/abslang.abs",754);
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().getCurrentTask().popStackFrame();
            return result;
        }
    }
    // abslang.abs:0:0: 
    public final abs.backend.java.lib.runtime.ABSFut<abs.backend.java.lib.types.ABSBool> async_acquireInstance(ABS.DC.DeploymentComponent_i instance) {
        return (abs.backend.java.lib.runtime.ABSFut)abs.backend.java.lib.runtime.ABSRuntime.getCurrentRuntime().asyncCall(new abs.backend.java.lib.runtime.AbstractAsyncCallRT<ABS.DC.CloudProvider_c>(
            this,
            abs.backend.java.lib.runtime.ABSRuntime.checkForNull(this),
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
            }.init(instance))
        ;
    }
    // abslang.abs:0:0: 
    public final abs.backend.java.lib.types.ABSBool acquireInstance(ABS.DC.DeploymentComponent_i instance) {
        __ABS_checkSameCOG(); 
        if (__ABS_getRuntime().debuggingEnabled()) {
            abs.backend.java.lib.runtime.Task<?> __ABS_currentTask = __ABS_getRuntime().getCurrentTask();
            __ABS_currentTask.newStackFrame(this, "acquireInstance");
            __ABS_currentTask.setLocalVariable("instance",instance);
        }
         {
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("abs/lang/abslang.abs",0);
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("abs/lang/abslang.abs",758);
            abs.backend.java.lib.types.ABSBool result = abs.backend.java.lib.types.ABSBool.TRUE;
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().getCurrentTask().setLocalVariable("result",result);
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("abs/lang/abslang.abs",759);
            if (ABS.StdLib.contains_f.apply(CloudProvider_c.this.acquiredInstances, instance).toBoolean()) {
                 {
                    if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("abs/lang/abslang.abs",759);
                    if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("abs/lang/abslang.abs",760);
                    result = abs.backend.java.lib.types.ABSBool.FALSE;
                    if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().getCurrentTask().setLocalVariable("result", result);}
            }
            else {
                 {
                    if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("abs/lang/abslang.abs",761);
                    if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("abs/lang/abslang.abs",762);
                    CloudProvider_c.this.acquiredInstances = ABS.StdLib.insertElement_f.apply(CloudProvider_c.this.acquiredInstances, instance);
                }
            }
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("abs/lang/abslang.abs",764);
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().getCurrentTask().popStackFrame();
            return result;
        }
    }
    // abslang.abs:0:0: 
    public final abs.backend.java.lib.runtime.ABSFut<abs.backend.java.lib.types.ABSBool> async_releaseInstance(ABS.DC.DeploymentComponent_i instance) {
        return (abs.backend.java.lib.runtime.ABSFut)abs.backend.java.lib.runtime.ABSRuntime.getCurrentRuntime().asyncCall(new abs.backend.java.lib.runtime.AbstractAsyncCallRT<ABS.DC.CloudProvider_c>(
            this,
            abs.backend.java.lib.runtime.ABSRuntime.checkForNull(this),
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
            }.init(instance))
        ;
    }
    // abslang.abs:0:0: 
    public final abs.backend.java.lib.types.ABSBool releaseInstance(ABS.DC.DeploymentComponent_i instance) {
        __ABS_checkSameCOG(); 
        if (__ABS_getRuntime().debuggingEnabled()) {
            abs.backend.java.lib.runtime.Task<?> __ABS_currentTask = __ABS_getRuntime().getCurrentTask();
            __ABS_currentTask.newStackFrame(this, "releaseInstance");
            __ABS_currentTask.setLocalVariable("instance",instance);
        }
         {
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("abs/lang/abslang.abs",0);
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("abs/lang/abslang.abs",771);
            if (ABS.StdLib.contains_f.apply(CloudProvider_c.this.acquiredInstances, instance).toBoolean()) {
                 {
                    if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("abs/lang/abslang.abs",771);
                    if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("abs/lang/abslang.abs",772);
                    CloudProvider_c.this.acquiredInstances = ABS.StdLib.remove_f.apply(CloudProvider_c.this.acquiredInstances, instance);
                }
            }
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("abs/lang/abslang.abs",774);
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().getCurrentTask().popStackFrame();
            return abs.backend.java.lib.types.ABSBool.TRUE;
        }
    }
    // abslang.abs:0:0: 
    public final abs.backend.java.lib.runtime.ABSFut<abs.backend.java.lib.types.ABSBool> async_killInstance(ABS.DC.DeploymentComponent_i instance) {
        return (abs.backend.java.lib.runtime.ABSFut)abs.backend.java.lib.runtime.ABSRuntime.getCurrentRuntime().asyncCall(new abs.backend.java.lib.runtime.AbstractAsyncCallRT<ABS.DC.CloudProvider_c>(
            this,
            abs.backend.java.lib.runtime.ABSRuntime.checkForNull(this),
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
                    return "killInstance";
                }
                public Object execute() {
                    return target.killInstance(arg0
                    );
                }
            }.init(instance))
        ;
    }
    // abslang.abs:0:0: 
    public final abs.backend.java.lib.types.ABSBool killInstance(ABS.DC.DeploymentComponent_i instance) {
        __ABS_checkSameCOG(); 
        if (__ABS_getRuntime().debuggingEnabled()) {
            abs.backend.java.lib.runtime.Task<?> __ABS_currentTask = __ABS_getRuntime().getCurrentTask();
            __ABS_currentTask.newStackFrame(this, "killInstance");
            __ABS_currentTask.setLocalVariable("instance",instance);
        }
         {
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("abs/lang/abslang.abs",0);
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("abs/lang/abslang.abs",780);
            CloudProvider_c.this.acquiredInstances = ABS.StdLib.remove_f.apply(CloudProvider_c.this.acquiredInstances, instance);
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("abs/lang/abslang.abs",781);
            CloudProvider_c.this.launchedInstances = ABS.StdLib.remove_f.apply(CloudProvider_c.this.launchedInstances, instance);
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("abs/lang/abslang.abs",782);
            CloudProvider_c.this.killedInstances = ABS.StdLib.insertElement_f.apply(CloudProvider_c.this.killedInstances, instance);
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("abs/lang/abslang.abs",783);
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().getCurrentTask().popStackFrame();
            return abs.backend.java.lib.types.ABSBool.TRUE;
        }
    }
    // abslang.abs:0:0: 
    public final abs.backend.java.lib.runtime.ABSFut<abs.backend.java.lib.types.ABSRational> async_getAccumulatedCost() {
        return (abs.backend.java.lib.runtime.ABSFut)abs.backend.java.lib.runtime.ABSRuntime.getCurrentRuntime().asyncCall(new abs.backend.java.lib.runtime.AbstractAsyncCallRT<ABS.DC.CloudProvider_c>(
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
                    return "getAccumulatedCost";
                }
                public Object execute() {
                    return target.getAccumulatedCost();
                }
            }.init())
        ;
    }
    // abslang.abs:0:0: 
    public final abs.backend.java.lib.types.ABSRational getAccumulatedCost() {
        __ABS_checkSameCOG(); 
        if (__ABS_getRuntime().debuggingEnabled()) {
            abs.backend.java.lib.runtime.Task<?> __ABS_currentTask = __ABS_getRuntime().getCurrentTask();
            __ABS_currentTask.newStackFrame(this, "getAccumulatedCost");
        }
         {
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("abs/lang/abslang.abs",0);
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("abs/lang/abslang.abs",787);
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().getCurrentTask().popStackFrame();
            return CloudProvider_c.this.accumulatedCost;
        }
    }
    // abslang.abs:0:0: 
    public final abs.backend.java.lib.runtime.ABSFut<ABS.DC.DeploymentComponent_i> async_findMatchingInstance(ABS.StdLib.Set<ABS.DC.DeploymentComponent_i> instances, ABS.StdLib.Map<ABS.DC.Resourcetype,abs.backend.java.lib.types.ABSRational> description) {
        return (abs.backend.java.lib.runtime.ABSFut)abs.backend.java.lib.runtime.ABSRuntime.getCurrentRuntime().asyncCall(new abs.backend.java.lib.runtime.AbstractAsyncCallRT<ABS.DC.CloudProvider_c>(
            this,
            abs.backend.java.lib.runtime.ABSRuntime.checkForNull(this),
            new ABS.StdLib.Duration_InfDuration(),
            new ABS.StdLib.Duration_InfDuration(),
            abs.backend.java.lib.types.ABSBool.FALSE) {
                ABS.StdLib.Set<ABS.DC.DeploymentComponent_i> arg0;
                ABS.StdLib.Map<ABS.DC.Resourcetype,abs.backend.java.lib.types.ABSRational> arg1;
                public java.util.List<abs.backend.java.lib.types.ABSValue> getArgs() {
                    return java.util.Arrays.asList(new abs.backend.java.lib.types.ABSValue[] {
                        arg0,arg1});
                }
                public abs.backend.java.lib.runtime.AsyncCall<?> init(ABS.StdLib.Set<ABS.DC.DeploymentComponent_i> _arg0,ABS.StdLib.Map<ABS.DC.Resourcetype,abs.backend.java.lib.types.ABSRational> _arg1) {
                    arg0 = _arg0;
                    arg1 = _arg1;
                    return this;
                }
                public java.lang.String methodName() {
                    return "findMatchingInstance";
                }
                public Object execute() {
                    return target.findMatchingInstance(arg0
                    ,arg1
                    );
                }
            }.init(instances, description))
        ;
    }
    // abslang.abs:0:0: 
    public final ABS.DC.DeploymentComponent_i findMatchingInstance(ABS.StdLib.Set<ABS.DC.DeploymentComponent_i> instances, ABS.StdLib.Map<ABS.DC.Resourcetype,abs.backend.java.lib.types.ABSRational> description) {
        __ABS_checkSameCOG(); 
        if (__ABS_getRuntime().debuggingEnabled()) {
            abs.backend.java.lib.runtime.Task<?> __ABS_currentTask = __ABS_getRuntime().getCurrentTask();
            __ABS_currentTask.newStackFrame(this, "findMatchingInstance");
            __ABS_currentTask.setLocalVariable("instances",instances);
            __ABS_currentTask.setLocalVariable("description",description);
        }
         {
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("abs/lang/abslang.abs",0);
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("abs/lang/abslang.abs",793);
            ABS.DC.DeploymentComponent_i result = null;
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().getCurrentTask().setLocalVariable("result",result);
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("abs/lang/abslang.abs",794);
            while (ABS.StdLib.emptySet_f.apply(instances).negate().and(abs.backend.java.lib.expr.BinOp.eq(result,null)).toBoolean()) {
                if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("abs/lang/abslang.abs",794);
                if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("abs/lang/abslang.abs",795);
                ABS.DC.DeploymentComponent_i instance = ABS.StdLib.take_f.apply(instances);
                if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().getCurrentTask().setLocalVariable("instance",instance);
                if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("abs/lang/abslang.abs",796);
                instances = ABS.StdLib.remove_f.apply(instances, instance);
                if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().getCurrentTask().setLocalVariable("instances", instances);if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("abs/lang/abslang.abs",797);
                abs.backend.java.lib.runtime.ABSFut tmp1692036285 = abs.backend.java.lib.runtime.ABSRuntime.getCurrentRuntime().asyncCall(new abs.backend.java.lib.runtime.AbstractAsyncCallRT<ABS.DC.DeploymentComponent_i>(
                    this,
                    abs.backend.java.lib.runtime.ABSRuntime.checkForNull(instance),
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
                if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().getCurrentTask().setLocalVariable("tmp1692036285",tmp1692036285);
                if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("abs/lang/abslang.abs",797);
                abs.backend.java.lib.runtime.ABSRuntime.await(new abs.backend.java.lib.runtime.ABSFutureGuard(tmp1692036285));
                if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("abs/lang/abslang.abs",797);
                abs.backend.java.lib.types.ABSBool matches = (abs.backend.java.lib.types.ABSBool)tmp1692036285.get();
                if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().getCurrentTask().setLocalVariable("matches",matches);
                if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("abs/lang/abslang.abs",798);
                if (matches.toBoolean()) {
                     {
                        if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("abs/lang/abslang.abs",798);
                        if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("abs/lang/abslang.abs",799);
                        result = instance;
                        if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().getCurrentTask().setLocalVariable("result", result);}
                }
            }
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("abs/lang/abslang.abs",802);
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().getCurrentTask().popStackFrame();
            return result;
        }
    }
}
