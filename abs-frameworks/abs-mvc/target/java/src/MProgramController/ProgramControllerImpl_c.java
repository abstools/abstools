package MProgramController;
// ProgramController.abs:17:0: 
public final class ProgramControllerImpl_c extends abs.backend.java.lib.runtime.ABSObject implements abs.backend.java.lib.types.ABSClass, MProgramController.ProgramController_i {
    private static final java.lang.String[] __fieldNames = new java.lang.String[] {  };
    public final java.util.List<java.lang.String> getFieldNames() { return java.util.Arrays.asList(__fieldNames); }
    public ProgramControllerImpl_c() {
        getCOG().objectCreated(this);
    }
    protected final void __ABS_init() {
        getCOG().objectInitialized(this);
    }
    protected final abs.backend.java.lib.types.ABSValue getFieldValue(java.lang.String __ABS_fieldName) throws java.lang.NoSuchFieldException {
        return super.getFieldValue(__ABS_fieldName);
    }
    public final java.lang.String getClassName() { return "ProgramControllerImpl"; }
    public static final <T extends ProgramControllerImpl_c> T createNewCOG() { return (T)ProgramControllerImpl_c.__ABS_createNewCOG(null, null); }
    public static final <T extends ProgramControllerImpl_c> T __ABS_createNewCOG(abs.backend.java.lib.runtime.ABSObject __ABS_source, abs.backend.java.scheduling.UserSchedulingStrategy strategy) {
        final abs.backend.java.lib.runtime.ABSRuntime __ABS_runtime = abs.backend.java.lib.runtime.ABSRuntime.getCurrentRuntime();
        final abs.backend.java.lib.runtime.COG __ABS_cog = strategy == null ? __ABS_runtime.createCOG(ProgramControllerImpl_c.class) : __ABS_runtime.createCOG(ProgramControllerImpl_c.class, strategy);
        final abs.backend.java.lib.runtime.ABSThread __ABS_thread = abs.backend.java.lib.runtime.ABSRuntime.getCurrentThread();
        final abs.backend.java.lib.runtime.COG __ABS_oldCOG = abs.backend.java.lib.runtime.ABSRuntime.getCurrentCOG();
        final abs.backend.java.lib.runtime.Task __ABS_sendingTask = abs.backend.java.lib.runtime.ABSRuntime.getCurrentTask();
        __ABS_thread.setCOG(__ABS_cog);
        try {
            ProgramControllerImpl_c __ABS_result = new ProgramControllerImpl_c();
            ;
            __ABS_runtime.cogCreated(__ABS_result);
            __ABS_cog.getScheduler().addTask(new abs.backend.java.lib.runtime.Task(new abs.backend.java.lib.runtime.ABSInitObjectCall(__ABS_sendingTask,__ABS_source,__ABS_result)));
            return (T)__ABS_result;
        } finally {
            __ABS_thread.setCOG(__ABS_oldCOG);
        }
    }
    public static final <T extends ProgramControllerImpl_c> T createNewObject() { return (T)ProgramControllerImpl_c.__ABS_createNewObject(null); }
    public static final <T extends ProgramControllerImpl_c> T __ABS_createNewObject(abs.backend.java.lib.runtime.ABSObject __ABS_source) {
        ProgramControllerImpl_c __ABS_result = new ProgramControllerImpl_c();
        __ABS_result.__ABS_init();
        return (T)__ABS_result;
    }
    // ProgramController.abs:0:0: 
    public final abs.backend.java.lib.runtime.ABSFut<ABS.StdLib.Pair<abs.backend.java.lib.types.ABSString,ABS.StdLib.List<MProgramModel.Program_i>>> async_list(ABS.Framework.Http.ABSHttpRequest_i request) {
        return (abs.backend.java.lib.runtime.ABSFut)abs.backend.java.lib.runtime.ABSRuntime.getCurrentRuntime().asyncCall(new abs.backend.java.lib.runtime.AbstractAsyncCallRT<MProgramController.ProgramControllerImpl_c>(
            this,
            abs.backend.java.lib.runtime.ABSRuntime.checkForNull(this),
            new ABS.StdLib.Duration_InfDuration(),
            new ABS.StdLib.Duration_InfDuration(),
            abs.backend.java.lib.types.ABSBool.FALSE) {
                ABS.Framework.Http.ABSHttpRequest_i arg0;
                public java.util.List<abs.backend.java.lib.types.ABSValue> getArgs() {
                    return java.util.Arrays.asList(new abs.backend.java.lib.types.ABSValue[] {
                        arg0});
                }
                public abs.backend.java.lib.runtime.AsyncCall<?> init(ABS.Framework.Http.ABSHttpRequest_i _arg0) {
                    arg0 = _arg0;
                    return this;
                }
                public java.lang.String methodName() {
                    return "list";
                }
                public Object execute() {
                    return target.list(arg0
                    );
                }
            }.init(request))
        ;
    }
    // ProgramController.abs:0:0: 
    public final ABS.StdLib.Pair<abs.backend.java.lib.types.ABSString,ABS.StdLib.List<MProgramModel.Program_i>> list(ABS.Framework.Http.ABSHttpRequest_i request) {
        __ABS_checkSameCOG(); 
        if (__ABS_getRuntime().debuggingEnabled()) {
            abs.backend.java.lib.runtime.Task<?> __ABS_currentTask = __ABS_getRuntime().getCurrentTask();
            __ABS_currentTask.newStackFrame(this, "list");
            __ABS_currentTask.setLocalVariable("request",request);
        }
         {
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("/home/afifun/abs/abstools-niken/abstools/abs-frameworks/abs-mvc/src/abs/controller/ProgramController.abs",0);
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("/home/afifun/abs/abstools-niken/abstools/abs-frameworks/abs-mvc/src/abs/controller/ProgramController.abs",21);
            MProgramModel.ProgramDb_i orm = MProgramModel.ProgramDbImpl_c.__ABS_createNewObject(this);
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().getCurrentTask().setLocalVariable("orm",orm);
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("/home/afifun/abs/abstools-niken/abstools/abs-frameworks/abs-mvc/src/abs/controller/ProgramController.abs",23);
            ABS.StdLib.List<MProgramModel.Program_i> programs = abs.backend.java.lib.runtime.ABSRuntime.checkForNull(orm).findAll(abs.backend.java.lib.types.ABSString.fromString("MProgramModel.ProgramImpl_c"));
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().getCurrentTask().setLocalVariable("programs",programs);
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("/home/afifun/abs/abstools-niken/abstools/abs-frameworks/abs-mvc/src/abs/controller/ProgramController.abs",25);
            ABS.StdLib.List<MProgramModel.Program_i> dataModel = new ABS.StdLib.List_Nil();
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().getCurrentTask().setLocalVariable("dataModel",dataModel);
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("/home/afifun/abs/abstools-niken/abstools/abs-frameworks/abs-mvc/src/abs/controller/ProgramController.abs",27);
            abs.backend.java.lib.types.ABSInteger index = abs.backend.java.lib.types.ABSInteger.fromString("0");
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().getCurrentTask().setLocalVariable("index",index);
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("/home/afifun/abs/abstools-niken/abstools/abs-frameworks/abs-mvc/src/abs/controller/ProgramController.abs",28);
            while (abs.backend.java.lib.expr.BinOp.lt(index,ABS.StdLib.length_f.apply(programs)).toBoolean()) {
                if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("/home/afifun/abs/abstools-niken/abstools/abs-frameworks/abs-mvc/src/abs/controller/ProgramController.abs",28);
                if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("/home/afifun/abs/abstools-niken/abstools/abs-frameworks/abs-mvc/src/abs/controller/ProgramController.abs",29);
                MProgramModel.Program_i p = ABS.StdLib.nth_f.apply(programs, index);
                if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().getCurrentTask().setLocalVariable("p",p);
                if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("/home/afifun/abs/abstools-niken/abstools/abs-frameworks/abs-mvc/src/abs/controller/ProgramController.abs",30);
                dataModel = ABS.StdLib.appendright_f.apply(dataModel, p);
                if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().getCurrentTask().setLocalVariable("dataModel", dataModel);if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("/home/afifun/abs/abstools-niken/abstools/abs-frameworks/abs-mvc/src/abs/controller/ProgramController.abs",31);
                index = index.add(abs.backend.java.lib.types.ABSInteger.fromString("1"));
                if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().getCurrentTask().setLocalVariable("index", index);}
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("/home/afifun/abs/abstools-niken/abstools/abs-frameworks/abs-mvc/src/abs/controller/ProgramController.abs",34);
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().getCurrentTask().popStackFrame();
            return new ABS.StdLib.Pair_Pair(abs.backend.java.lib.types.ABSString.fromString("program/list"), dataModel);
        }
    }
    // DFormSourceController.abs:0:0: 
    public final abs.backend.java.lib.runtime.ABSFut<ABS.StdLib.Pair<abs.backend.java.lib.types.ABSString,ABS.StdLib.List<MProgramModel.Program_i>>> async_add(ABS.Framework.Http.ABSHttpRequest_i request) {
        return (abs.backend.java.lib.runtime.ABSFut)abs.backend.java.lib.runtime.ABSRuntime.getCurrentRuntime().asyncCall(new abs.backend.java.lib.runtime.AbstractAsyncCallRT<MProgramController.ProgramControllerImpl_c>(
            this,
            abs.backend.java.lib.runtime.ABSRuntime.checkForNull(this),
            new ABS.StdLib.Duration_InfDuration(),
            new ABS.StdLib.Duration_InfDuration(),
            abs.backend.java.lib.types.ABSBool.FALSE) {
                ABS.Framework.Http.ABSHttpRequest_i arg0;
                public java.util.List<abs.backend.java.lib.types.ABSValue> getArgs() {
                    return java.util.Arrays.asList(new abs.backend.java.lib.types.ABSValue[] {
                        arg0});
                }
                public abs.backend.java.lib.runtime.AsyncCall<?> init(ABS.Framework.Http.ABSHttpRequest_i _arg0) {
                    arg0 = _arg0;
                    return this;
                }
                public java.lang.String methodName() {
                    return "add";
                }
                public Object execute() {
                    return target.add(arg0
                    );
                }
            }.init(request))
        ;
    }
    // DFormSourceController.abs:0:0: 
    public final ABS.StdLib.Pair<abs.backend.java.lib.types.ABSString,ABS.StdLib.List<MProgramModel.Program_i>> add(ABS.Framework.Http.ABSHttpRequest_i request) {
        __ABS_checkSameCOG(); 
        if (__ABS_getRuntime().debuggingEnabled()) {
            abs.backend.java.lib.runtime.Task<?> __ABS_currentTask = __ABS_getRuntime().getCurrentTask();
            __ABS_currentTask.newStackFrame(this, "add");
            __ABS_currentTask.setLocalVariable("request",request);
        }
         {
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("/home/afifun/abs/abstools-niken/abstools/abs-frameworks/abs-mvc/src/abs/controller/ProgramController.abs",0);
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("/home/afifun/abs/abstools-niken/abstools/abs-frameworks/abs-mvc/src/abs/controller/ProgramController.abs",8);
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().getCurrentTask().popStackFrame();
            return new ABS.StdLib.Pair_Pair(abs.backend.java.lib.types.ABSString.fromString("program/delta/addSource"), new ABS.StdLib.List_Nil());
        }
    }
    // DFormSourceController.abs:0:0: 
    public final abs.backend.java.lib.runtime.ABSFut<ABS.StdLib.Pair<abs.backend.java.lib.types.ABSString,ABS.StdLib.List<MProgramModel.Program_i>>> async_save(ABS.Framework.Http.ABSHttpRequest_i request) {
        return (abs.backend.java.lib.runtime.ABSFut)abs.backend.java.lib.runtime.ABSRuntime.getCurrentRuntime().asyncCall(new abs.backend.java.lib.runtime.AbstractAsyncCallRT<MProgramController.ProgramControllerImpl_c>(
            this,
            abs.backend.java.lib.runtime.ABSRuntime.checkForNull(this),
            new ABS.StdLib.Duration_InfDuration(),
            new ABS.StdLib.Duration_InfDuration(),
            abs.backend.java.lib.types.ABSBool.FALSE) {
                ABS.Framework.Http.ABSHttpRequest_i arg0;
                public java.util.List<abs.backend.java.lib.types.ABSValue> getArgs() {
                    return java.util.Arrays.asList(new abs.backend.java.lib.types.ABSValue[] {
                        arg0});
                }
                public abs.backend.java.lib.runtime.AsyncCall<?> init(ABS.Framework.Http.ABSHttpRequest_i _arg0) {
                    arg0 = _arg0;
                    return this;
                }
                public java.lang.String methodName() {
                    return "save";
                }
                public Object execute() {
                    return target.save(arg0
                    );
                }
            }.init(request))
        ;
    }
    // DFormSourceController.abs:0:0: 
    public final ABS.StdLib.Pair<abs.backend.java.lib.types.ABSString,ABS.StdLib.List<MProgramModel.Program_i>> save(ABS.Framework.Http.ABSHttpRequest_i request) {
        __ABS_checkSameCOG(); 
        if (__ABS_getRuntime().debuggingEnabled()) {
            abs.backend.java.lib.runtime.Task<?> __ABS_currentTask = __ABS_getRuntime().getCurrentTask();
            __ABS_currentTask.newStackFrame(this, "save");
            __ABS_currentTask.setLocalVariable("request",request);
        }
         {
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("/home/afifun/abs/abstools-niken/abstools/abs-frameworks/abs-mvc/src/abs/controller/ProgramController.abs",0);
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("/home/afifun/abs/abstools-niken/abstools/abs-frameworks/abs-mvc/src/abs/controller/ProgramController.abs",12);
            ABS.Framework.Utility.Utility_i utility = ABS.Framework.Utility.UtilityImpl_c.__ABS_createNewObject(this);
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().getCurrentTask().setLocalVariable("utility",utility);
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("/home/afifun/abs/abstools-niken/abstools/abs-frameworks/abs-mvc/src/abs/controller/ProgramController.abs",14);
            MProgramModel.ProgramDb_i orm = MProgramModel.ProgramDbImpl_c.__ABS_createNewObject(this);
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().getCurrentTask().setLocalVariable("orm",orm);
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("/home/afifun/abs/abstools-niken/abstools/abs-frameworks/abs-mvc/src/abs/controller/ProgramController.abs",15);
            MProgramModel.Program_i program = MProgramModel.ProgramImpl_c.__ABS_createNewObject(this);
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().getCurrentTask().setLocalVariable("program",program);
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("/home/afifun/abs/abstools-niken/abstools/abs-frameworks/abs-mvc/src/abs/controller/ProgramController.abs",18);
            abs.backend.java.lib.types.ABSString namaProgram = abs.backend.java.lib.runtime.ABSRuntime.checkForNull(request).getInput(abs.backend.java.lib.types.ABSString.fromString("namaProgram"));
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().getCurrentTask().setLocalVariable("namaProgram",namaProgram);
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("/home/afifun/abs/abstools-niken/abstools/abs-frameworks/abs-mvc/src/abs/controller/ProgramController.abs",19);
            abs.backend.java.lib.types.ABSString departemen = abs.backend.java.lib.runtime.ABSRuntime.checkForNull(request).getInput(abs.backend.java.lib.types.ABSString.fromString("departemen"));
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().getCurrentTask().setLocalVariable("departemen",departemen);
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("/home/afifun/abs/abstools-niken/abstools/abs-frameworks/abs-mvc/src/abs/controller/ProgramController.abs",20);
            abs.backend.java.lib.types.ABSString tempat = abs.backend.java.lib.runtime.ABSRuntime.checkForNull(request).getInput(abs.backend.java.lib.types.ABSString.fromString("tempat"));
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().getCurrentTask().setLocalVariable("tempat",tempat);
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("/home/afifun/abs/abstools-niken/abstools/abs-frameworks/abs-mvc/src/abs/controller/ProgramController.abs",21);
            abs.backend.java.lib.types.ABSString tanggal = abs.backend.java.lib.runtime.ABSRuntime.checkForNull(request).getInput(abs.backend.java.lib.types.ABSString.fromString("tanggal"));
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().getCurrentTask().setLocalVariable("tanggal",tanggal);
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("/home/afifun/abs/abstools-niken/abstools/abs-frameworks/abs-mvc/src/abs/controller/ProgramController.abs",22);
            abs.backend.java.lib.types.ABSString sumberDana = abs.backend.java.lib.runtime.ABSRuntime.checkForNull(request).getInput(abs.backend.java.lib.types.ABSString.fromString("sumberDana"));
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().getCurrentTask().setLocalVariable("sumberDana",sumberDana);
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("/home/afifun/abs/abstools-niken/abstools/abs-frameworks/abs-mvc/src/abs/controller/ProgramController.abs",24);
            abs.backend.java.lib.types.ABSString pesertaString = abs.backend.java.lib.runtime.ABSRuntime.checkForNull(request).getInput(abs.backend.java.lib.types.ABSString.fromString("peserta"));
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().getCurrentTask().setLocalVariable("pesertaString",pesertaString);
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("/home/afifun/abs/abstools-niken/abstools/abs-frameworks/abs-mvc/src/abs/controller/ProgramController.abs",25);
            abs.backend.java.lib.types.ABSInteger peserta = abs.backend.java.lib.runtime.ABSRuntime.checkForNull(utility).stringToInteger(pesertaString);
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().getCurrentTask().setLocalVariable("peserta",peserta);
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("/home/afifun/abs/abstools-niken/abstools/abs-frameworks/abs-mvc/src/abs/controller/ProgramController.abs",27);
            abs.backend.java.lib.types.ABSString penanggungJawab = abs.backend.java.lib.runtime.ABSRuntime.checkForNull(request).getInput(abs.backend.java.lib.types.ABSString.fromString("penanggungJawab"));
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().getCurrentTask().setLocalVariable("penanggungJawab",penanggungJawab);
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("/home/afifun/abs/abstools-niken/abstools/abs-frameworks/abs-mvc/src/abs/controller/ProgramController.abs",29);
            abs.backend.java.lib.types.ABSString biayaString = abs.backend.java.lib.runtime.ABSRuntime.checkForNull(request).getInput(abs.backend.java.lib.types.ABSString.fromString("biaya"));
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().getCurrentTask().setLocalVariable("biayaString",biayaString);
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("/home/afifun/abs/abstools-niken/abstools/abs-frameworks/abs-mvc/src/abs/controller/ProgramController.abs",30);
            abs.backend.java.lib.types.ABSRational biaya = abs.backend.java.lib.runtime.ABSRuntime.checkForNull(utility).stringToRational(biayaString);
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().getCurrentTask().setLocalVariable("biaya",biaya);
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("/home/afifun/abs/abstools-niken/abstools/abs-frameworks/abs-mvc/src/abs/controller/ProgramController.abs",32);
            abs.backend.java.lib.runtime.ABSRuntime.checkForNull(program).setNamaProgram(namaProgram);
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("/home/afifun/abs/abstools-niken/abstools/abs-frameworks/abs-mvc/src/abs/controller/ProgramController.abs",33);
            abs.backend.java.lib.runtime.ABSRuntime.checkForNull(program).setDepartemen(departemen);
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("/home/afifun/abs/abstools-niken/abstools/abs-frameworks/abs-mvc/src/abs/controller/ProgramController.abs",34);
            abs.backend.java.lib.runtime.ABSRuntime.checkForNull(program).setTempat(tempat);
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("/home/afifun/abs/abstools-niken/abstools/abs-frameworks/abs-mvc/src/abs/controller/ProgramController.abs",35);
            abs.backend.java.lib.runtime.ABSRuntime.checkForNull(program).setTanggal(tanggal);
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("/home/afifun/abs/abstools-niken/abstools/abs-frameworks/abs-mvc/src/abs/controller/ProgramController.abs",36);
            abs.backend.java.lib.runtime.ABSRuntime.checkForNull(program).setPeserta(peserta);
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("/home/afifun/abs/abstools-niken/abstools/abs-frameworks/abs-mvc/src/abs/controller/ProgramController.abs",37);
            abs.backend.java.lib.runtime.ABSRuntime.checkForNull(program).setPenanggungJawab(penanggungJawab);
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("/home/afifun/abs/abstools-niken/abstools/abs-frameworks/abs-mvc/src/abs/controller/ProgramController.abs",38);
            abs.backend.java.lib.runtime.ABSRuntime.checkForNull(program).setBiaya(biaya);
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("/home/afifun/abs/abstools-niken/abstools/abs-frameworks/abs-mvc/src/abs/controller/ProgramController.abs",39);
            abs.backend.java.lib.runtime.ABSRuntime.checkForNull(program).setSumberDana(sumberDana);
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("/home/afifun/abs/abstools-niken/abstools/abs-frameworks/abs-mvc/src/abs/controller/ProgramController.abs",40);
            abs.backend.java.lib.runtime.ABSRuntime.checkForNull(orm).save(program);
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("/home/afifun/abs/abstools-niken/abstools/abs-frameworks/abs-mvc/src/abs/controller/ProgramController.abs",41);
            abs.backend.java.lib.runtime.ABSRuntime.checkForNull(orm).log(namaProgram);
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("/home/afifun/abs/abstools-niken/abstools/abs-frameworks/abs-mvc/src/abs/controller/ProgramController.abs",43);
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().getCurrentTask().popStackFrame();
            return new ABS.StdLib.Pair_Pair(abs.backend.java.lib.types.ABSString.fromString("program/delta/addSource"), new ABS.StdLib.List_Nil());
        }
    }
    // DFormSourceController.abs:0:0: 
    public final abs.backend.java.lib.runtime.ABSFut<ABS.StdLib.Pair<abs.backend.java.lib.types.ABSString,ABS.StdLib.List<MProgramModel.Program_i>>> async_edit(ABS.Framework.Http.ABSHttpRequest_i request) {
        return (abs.backend.java.lib.runtime.ABSFut)abs.backend.java.lib.runtime.ABSRuntime.getCurrentRuntime().asyncCall(new abs.backend.java.lib.runtime.AbstractAsyncCallRT<MProgramController.ProgramControllerImpl_c>(
            this,
            abs.backend.java.lib.runtime.ABSRuntime.checkForNull(this),
            new ABS.StdLib.Duration_InfDuration(),
            new ABS.StdLib.Duration_InfDuration(),
            abs.backend.java.lib.types.ABSBool.FALSE) {
                ABS.Framework.Http.ABSHttpRequest_i arg0;
                public java.util.List<abs.backend.java.lib.types.ABSValue> getArgs() {
                    return java.util.Arrays.asList(new abs.backend.java.lib.types.ABSValue[] {
                        arg0});
                }
                public abs.backend.java.lib.runtime.AsyncCall<?> init(ABS.Framework.Http.ABSHttpRequest_i _arg0) {
                    arg0 = _arg0;
                    return this;
                }
                public java.lang.String methodName() {
                    return "edit";
                }
                public Object execute() {
                    return target.edit(arg0
                    );
                }
            }.init(request))
        ;
    }
    // DFormSourceController.abs:0:0: 
    public final ABS.StdLib.Pair<abs.backend.java.lib.types.ABSString,ABS.StdLib.List<MProgramModel.Program_i>> edit(ABS.Framework.Http.ABSHttpRequest_i request) {
        __ABS_checkSameCOG(); 
        if (__ABS_getRuntime().debuggingEnabled()) {
            abs.backend.java.lib.runtime.Task<?> __ABS_currentTask = __ABS_getRuntime().getCurrentTask();
            __ABS_currentTask.newStackFrame(this, "edit");
            __ABS_currentTask.setLocalVariable("request",request);
        }
         {
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("/home/afifun/abs/abstools-niken/abstools/abs-frameworks/abs-mvc/src/abs/controller/ProgramController.abs",0);
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("/home/afifun/abs/abstools-niken/abstools/abs-frameworks/abs-mvc/src/abs/controller/ProgramController.abs",47);
            MProgramModel.ProgramDb_i orm = MProgramModel.ProgramDbImpl_c.__ABS_createNewObject(this);
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().getCurrentTask().setLocalVariable("orm",orm);
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("/home/afifun/abs/abstools-niken/abstools/abs-frameworks/abs-mvc/src/abs/controller/ProgramController.abs",49);
            abs.backend.java.lib.types.ABSString id = abs.backend.java.lib.runtime.ABSRuntime.checkForNull(request).getInput(abs.backend.java.lib.types.ABSString.fromString("id"));
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().getCurrentTask().setLocalVariable("id",id);
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("/home/afifun/abs/abstools-niken/abstools/abs-frameworks/abs-mvc/src/abs/controller/ProgramController.abs",51);
            abs.backend.java.lib.types.ABSString condition = abs.backend.java.lib.types.ABSString.fromString("idProgram=").add(id);
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().getCurrentTask().setLocalVariable("condition",condition);
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("/home/afifun/abs/abstools-niken/abstools/abs-frameworks/abs-mvc/src/abs/controller/ProgramController.abs",53);
            ABS.StdLib.List<MProgramModel.Program_i> dataModel = new ABS.StdLib.List_Nil();
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().getCurrentTask().setLocalVariable("dataModel",dataModel);
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("/home/afifun/abs/abstools-niken/abstools/abs-frameworks/abs-mvc/src/abs/controller/ProgramController.abs",54);
            MProgramModel.Program_i p = abs.backend.java.lib.runtime.ABSRuntime.checkForNull(orm).findByAttributes(abs.backend.java.lib.types.ABSString.fromString("MProgramModel.ProgramImpl_c"), condition);
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().getCurrentTask().setLocalVariable("p",p);
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("/home/afifun/abs/abstools-niken/abstools/abs-frameworks/abs-mvc/src/abs/controller/ProgramController.abs",55);
            dataModel = ABS.StdLib.appendright_f.apply(dataModel, p);
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().getCurrentTask().setLocalVariable("dataModel", dataModel);if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("/home/afifun/abs/abstools-niken/abstools/abs-frameworks/abs-mvc/src/abs/controller/ProgramController.abs",57);
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().getCurrentTask().popStackFrame();
            return new ABS.StdLib.Pair_Pair(abs.backend.java.lib.types.ABSString.fromString("program/delta/editSource"), dataModel);
        }
    }
    // DFormSourceController.abs:0:0: 
    public final abs.backend.java.lib.runtime.ABSFut<ABS.StdLib.Pair<abs.backend.java.lib.types.ABSString,ABS.StdLib.List<MProgramModel.Program_i>>> async_update(ABS.Framework.Http.ABSHttpRequest_i request) {
        return (abs.backend.java.lib.runtime.ABSFut)abs.backend.java.lib.runtime.ABSRuntime.getCurrentRuntime().asyncCall(new abs.backend.java.lib.runtime.AbstractAsyncCallRT<MProgramController.ProgramControllerImpl_c>(
            this,
            abs.backend.java.lib.runtime.ABSRuntime.checkForNull(this),
            new ABS.StdLib.Duration_InfDuration(),
            new ABS.StdLib.Duration_InfDuration(),
            abs.backend.java.lib.types.ABSBool.FALSE) {
                ABS.Framework.Http.ABSHttpRequest_i arg0;
                public java.util.List<abs.backend.java.lib.types.ABSValue> getArgs() {
                    return java.util.Arrays.asList(new abs.backend.java.lib.types.ABSValue[] {
                        arg0});
                }
                public abs.backend.java.lib.runtime.AsyncCall<?> init(ABS.Framework.Http.ABSHttpRequest_i _arg0) {
                    arg0 = _arg0;
                    return this;
                }
                public java.lang.String methodName() {
                    return "update";
                }
                public Object execute() {
                    return target.update(arg0
                    );
                }
            }.init(request))
        ;
    }
    // DFormSourceController.abs:0:0: 
    public final ABS.StdLib.Pair<abs.backend.java.lib.types.ABSString,ABS.StdLib.List<MProgramModel.Program_i>> update(ABS.Framework.Http.ABSHttpRequest_i request) {
        __ABS_checkSameCOG(); 
        if (__ABS_getRuntime().debuggingEnabled()) {
            abs.backend.java.lib.runtime.Task<?> __ABS_currentTask = __ABS_getRuntime().getCurrentTask();
            __ABS_currentTask.newStackFrame(this, "update");
            __ABS_currentTask.setLocalVariable("request",request);
        }
         {
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("/home/afifun/abs/abstools-niken/abstools/abs-frameworks/abs-mvc/src/abs/controller/ProgramController.abs",0);
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("/home/afifun/abs/abstools-niken/abstools/abs-frameworks/abs-mvc/src/abs/controller/ProgramController.abs",62);
            ABS.Framework.Utility.Utility_i utility = ABS.Framework.Utility.UtilityImpl_c.__ABS_createNewObject(this);
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().getCurrentTask().setLocalVariable("utility",utility);
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("/home/afifun/abs/abstools-niken/abstools/abs-frameworks/abs-mvc/src/abs/controller/ProgramController.abs",64);
            abs.backend.java.lib.types.ABSString id = abs.backend.java.lib.runtime.ABSRuntime.checkForNull(request).getInput(abs.backend.java.lib.types.ABSString.fromString("idProgram"));
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().getCurrentTask().setLocalVariable("id",id);
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("/home/afifun/abs/abstools-niken/abstools/abs-frameworks/abs-mvc/src/abs/controller/ProgramController.abs",66);
            abs.backend.java.lib.types.ABSString condition = abs.backend.java.lib.types.ABSString.fromString("idProgram=").add(id);
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().getCurrentTask().setLocalVariable("condition",condition);
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("/home/afifun/abs/abstools-niken/abstools/abs-frameworks/abs-mvc/src/abs/controller/ProgramController.abs",68);
            MProgramModel.ProgramDb_i orm = MProgramModel.ProgramDbImpl_c.__ABS_createNewObject(this);
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().getCurrentTask().setLocalVariable("orm",orm);
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("/home/afifun/abs/abstools-niken/abstools/abs-frameworks/abs-mvc/src/abs/controller/ProgramController.abs",69);
            MProgramModel.Program_i program = abs.backend.java.lib.runtime.ABSRuntime.checkForNull(orm).findByAttributes(abs.backend.java.lib.types.ABSString.fromString("MProgramModel.ProgramImpl_c"), condition);
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().getCurrentTask().setLocalVariable("program",program);
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("/home/afifun/abs/abstools-niken/abstools/abs-frameworks/abs-mvc/src/abs/controller/ProgramController.abs",72);
            abs.backend.java.lib.types.ABSString namaProgram = abs.backend.java.lib.runtime.ABSRuntime.checkForNull(request).getInput(abs.backend.java.lib.types.ABSString.fromString("namaProgram"));
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().getCurrentTask().setLocalVariable("namaProgram",namaProgram);
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("/home/afifun/abs/abstools-niken/abstools/abs-frameworks/abs-mvc/src/abs/controller/ProgramController.abs",73);
            abs.backend.java.lib.types.ABSString departemen = abs.backend.java.lib.runtime.ABSRuntime.checkForNull(request).getInput(abs.backend.java.lib.types.ABSString.fromString("departemen"));
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().getCurrentTask().setLocalVariable("departemen",departemen);
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("/home/afifun/abs/abstools-niken/abstools/abs-frameworks/abs-mvc/src/abs/controller/ProgramController.abs",74);
            abs.backend.java.lib.types.ABSString tempat = abs.backend.java.lib.runtime.ABSRuntime.checkForNull(request).getInput(abs.backend.java.lib.types.ABSString.fromString("tempat"));
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().getCurrentTask().setLocalVariable("tempat",tempat);
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("/home/afifun/abs/abstools-niken/abstools/abs-frameworks/abs-mvc/src/abs/controller/ProgramController.abs",75);
            abs.backend.java.lib.types.ABSString tanggal = abs.backend.java.lib.runtime.ABSRuntime.checkForNull(request).getInput(abs.backend.java.lib.types.ABSString.fromString("tanggal"));
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().getCurrentTask().setLocalVariable("tanggal",tanggal);
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("/home/afifun/abs/abstools-niken/abstools/abs-frameworks/abs-mvc/src/abs/controller/ProgramController.abs",76);
            abs.backend.java.lib.types.ABSString sumberDana = abs.backend.java.lib.runtime.ABSRuntime.checkForNull(request).getInput(abs.backend.java.lib.types.ABSString.fromString("sumberDana"));
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().getCurrentTask().setLocalVariable("sumberDana",sumberDana);
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("/home/afifun/abs/abstools-niken/abstools/abs-frameworks/abs-mvc/src/abs/controller/ProgramController.abs",78);
            abs.backend.java.lib.types.ABSString pesertaString = abs.backend.java.lib.runtime.ABSRuntime.checkForNull(request).getInput(abs.backend.java.lib.types.ABSString.fromString("peserta"));
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().getCurrentTask().setLocalVariable("pesertaString",pesertaString);
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("/home/afifun/abs/abstools-niken/abstools/abs-frameworks/abs-mvc/src/abs/controller/ProgramController.abs",79);
            abs.backend.java.lib.types.ABSInteger peserta = abs.backend.java.lib.runtime.ABSRuntime.checkForNull(utility).stringToInteger(pesertaString);
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().getCurrentTask().setLocalVariable("peserta",peserta);
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("/home/afifun/abs/abstools-niken/abstools/abs-frameworks/abs-mvc/src/abs/controller/ProgramController.abs",81);
            abs.backend.java.lib.types.ABSString penanggungJawab = abs.backend.java.lib.runtime.ABSRuntime.checkForNull(request).getInput(abs.backend.java.lib.types.ABSString.fromString("penanggungJawab"));
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().getCurrentTask().setLocalVariable("penanggungJawab",penanggungJawab);
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("/home/afifun/abs/abstools-niken/abstools/abs-frameworks/abs-mvc/src/abs/controller/ProgramController.abs",83);
            abs.backend.java.lib.types.ABSString biayaString = abs.backend.java.lib.runtime.ABSRuntime.checkForNull(request).getInput(abs.backend.java.lib.types.ABSString.fromString("biaya"));
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().getCurrentTask().setLocalVariable("biayaString",biayaString);
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("/home/afifun/abs/abstools-niken/abstools/abs-frameworks/abs-mvc/src/abs/controller/ProgramController.abs",84);
            abs.backend.java.lib.types.ABSRational biaya = abs.backend.java.lib.runtime.ABSRuntime.checkForNull(utility).stringToRational(biayaString);
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().getCurrentTask().setLocalVariable("biaya",biaya);
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("/home/afifun/abs/abstools-niken/abstools/abs-frameworks/abs-mvc/src/abs/controller/ProgramController.abs",86);
            abs.backend.java.lib.runtime.ABSRuntime.checkForNull(program).setNamaProgram(namaProgram);
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("/home/afifun/abs/abstools-niken/abstools/abs-frameworks/abs-mvc/src/abs/controller/ProgramController.abs",87);
            abs.backend.java.lib.runtime.ABSRuntime.checkForNull(program).setDepartemen(departemen);
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("/home/afifun/abs/abstools-niken/abstools/abs-frameworks/abs-mvc/src/abs/controller/ProgramController.abs",88);
            abs.backend.java.lib.runtime.ABSRuntime.checkForNull(program).setTempat(tempat);
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("/home/afifun/abs/abstools-niken/abstools/abs-frameworks/abs-mvc/src/abs/controller/ProgramController.abs",89);
            abs.backend.java.lib.runtime.ABSRuntime.checkForNull(program).setTanggal(tanggal);
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("/home/afifun/abs/abstools-niken/abstools/abs-frameworks/abs-mvc/src/abs/controller/ProgramController.abs",90);
            abs.backend.java.lib.runtime.ABSRuntime.checkForNull(program).setPeserta(peserta);
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("/home/afifun/abs/abstools-niken/abstools/abs-frameworks/abs-mvc/src/abs/controller/ProgramController.abs",91);
            abs.backend.java.lib.runtime.ABSRuntime.checkForNull(program).setPenanggungJawab(penanggungJawab);
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("/home/afifun/abs/abstools-niken/abstools/abs-frameworks/abs-mvc/src/abs/controller/ProgramController.abs",92);
            abs.backend.java.lib.runtime.ABSRuntime.checkForNull(program).setBiaya(biaya);
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("/home/afifun/abs/abstools-niken/abstools/abs-frameworks/abs-mvc/src/abs/controller/ProgramController.abs",93);
            abs.backend.java.lib.runtime.ABSRuntime.checkForNull(program).setSumberDana(sumberDana);
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("/home/afifun/abs/abstools-niken/abstools/abs-frameworks/abs-mvc/src/abs/controller/ProgramController.abs",94);
            abs.backend.java.lib.runtime.ABSRuntime.checkForNull(orm).update(program);
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("/home/afifun/abs/abstools-niken/abstools/abs-frameworks/abs-mvc/src/abs/controller/ProgramController.abs",95);
            abs.backend.java.lib.runtime.ABSRuntime.checkForNull(orm).log(namaProgram);
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("/home/afifun/abs/abstools-niken/abstools/abs-frameworks/abs-mvc/src/abs/controller/ProgramController.abs",98);
            ABS.StdLib.List<MProgramModel.Program_i> dataModel = new ABS.StdLib.List_Nil();
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().getCurrentTask().setLocalVariable("dataModel",dataModel);
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("/home/afifun/abs/abstools-niken/abstools/abs-frameworks/abs-mvc/src/abs/controller/ProgramController.abs",99);
            dataModel = ABS.StdLib.appendright_f.apply(dataModel, program);
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().getCurrentTask().setLocalVariable("dataModel", dataModel);if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("/home/afifun/abs/abstools-niken/abstools/abs-frameworks/abs-mvc/src/abs/controller/ProgramController.abs",101);
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().getCurrentTask().popStackFrame();
            return new ABS.StdLib.Pair_Pair(abs.backend.java.lib.types.ABSString.fromString("program/delta/editSource"), dataModel);
        }
    }
    // ProgramController.abs:0:0: 
    public final abs.backend.java.lib.runtime.ABSFut<ABS.StdLib.Pair<abs.backend.java.lib.types.ABSString,ABS.StdLib.List<MProgramModel.Program_i>>> async_delete(ABS.Framework.Http.ABSHttpRequest_i request) {
        return (abs.backend.java.lib.runtime.ABSFut)abs.backend.java.lib.runtime.ABSRuntime.getCurrentRuntime().asyncCall(new abs.backend.java.lib.runtime.AbstractAsyncCallRT<MProgramController.ProgramControllerImpl_c>(
            this,
            abs.backend.java.lib.runtime.ABSRuntime.checkForNull(this),
            new ABS.StdLib.Duration_InfDuration(),
            new ABS.StdLib.Duration_InfDuration(),
            abs.backend.java.lib.types.ABSBool.FALSE) {
                ABS.Framework.Http.ABSHttpRequest_i arg0;
                public java.util.List<abs.backend.java.lib.types.ABSValue> getArgs() {
                    return java.util.Arrays.asList(new abs.backend.java.lib.types.ABSValue[] {
                        arg0});
                }
                public abs.backend.java.lib.runtime.AsyncCall<?> init(ABS.Framework.Http.ABSHttpRequest_i _arg0) {
                    arg0 = _arg0;
                    return this;
                }
                public java.lang.String methodName() {
                    return "delete";
                }
                public Object execute() {
                    return target.delete(arg0
                    );
                }
            }.init(request))
        ;
    }
    // ProgramController.abs:0:0: 
    public final ABS.StdLib.Pair<abs.backend.java.lib.types.ABSString,ABS.StdLib.List<MProgramModel.Program_i>> delete(ABS.Framework.Http.ABSHttpRequest_i request) {
        __ABS_checkSameCOG(); 
        if (__ABS_getRuntime().debuggingEnabled()) {
            abs.backend.java.lib.runtime.Task<?> __ABS_currentTask = __ABS_getRuntime().getCurrentTask();
            __ABS_currentTask.newStackFrame(this, "delete");
            __ABS_currentTask.setLocalVariable("request",request);
        }
         {
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("/home/afifun/abs/abstools-niken/abstools/abs-frameworks/abs-mvc/src/abs/controller/ProgramController.abs",0);
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("/home/afifun/abs/abstools-niken/abstools/abs-frameworks/abs-mvc/src/abs/controller/ProgramController.abs",133);
            abs.backend.java.lib.types.ABSString id = abs.backend.java.lib.runtime.ABSRuntime.checkForNull(request).getInput(abs.backend.java.lib.types.ABSString.fromString("id"));
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().getCurrentTask().setLocalVariable("id",id);
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("/home/afifun/abs/abstools-niken/abstools/abs-frameworks/abs-mvc/src/abs/controller/ProgramController.abs",135);
            abs.backend.java.lib.types.ABSString condition = abs.backend.java.lib.types.ABSString.fromString("idProgram=").add(id);
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().getCurrentTask().setLocalVariable("condition",condition);
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("/home/afifun/abs/abstools-niken/abstools/abs-frameworks/abs-mvc/src/abs/controller/ProgramController.abs",136);
            MProgramModel.ProgramDb_i orm = MProgramModel.ProgramDbImpl_c.__ABS_createNewObject(this);
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().getCurrentTask().setLocalVariable("orm",orm);
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("/home/afifun/abs/abstools-niken/abstools/abs-frameworks/abs-mvc/src/abs/controller/ProgramController.abs",137);
            MProgramModel.Program_i program = abs.backend.java.lib.runtime.ABSRuntime.checkForNull(orm).findByAttributes(abs.backend.java.lib.types.ABSString.fromString("MProgramModel.ProgramImpl_c"), condition);
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().getCurrentTask().setLocalVariable("program",program);
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("/home/afifun/abs/abstools-niken/abstools/abs-frameworks/abs-mvc/src/abs/controller/ProgramController.abs",138);
            abs.backend.java.lib.runtime.ABSRuntime.checkForNull(orm).delete(program);
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("/home/afifun/abs/abstools-niken/abstools/abs-frameworks/abs-mvc/src/abs/controller/ProgramController.abs",140);
            ABS.StdLib.List<MProgramModel.Program_i> programs = abs.backend.java.lib.runtime.ABSRuntime.checkForNull(orm).findAll(abs.backend.java.lib.types.ABSString.fromString("MProgramModel.ProgramImpl_c"));
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().getCurrentTask().setLocalVariable("programs",programs);
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("/home/afifun/abs/abstools-niken/abstools/abs-frameworks/abs-mvc/src/abs/controller/ProgramController.abs",142);
            ABS.StdLib.List<MProgramModel.Program_i> dataModel = new ABS.StdLib.List_Nil();
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().getCurrentTask().setLocalVariable("dataModel",dataModel);
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("/home/afifun/abs/abstools-niken/abstools/abs-frameworks/abs-mvc/src/abs/controller/ProgramController.abs",144);
            abs.backend.java.lib.types.ABSInteger index = abs.backend.java.lib.types.ABSInteger.fromString("0");
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().getCurrentTask().setLocalVariable("index",index);
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("/home/afifun/abs/abstools-niken/abstools/abs-frameworks/abs-mvc/src/abs/controller/ProgramController.abs",145);
            while (abs.backend.java.lib.expr.BinOp.lt(index,ABS.StdLib.length_f.apply(programs)).toBoolean()) {
                if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("/home/afifun/abs/abstools-niken/abstools/abs-frameworks/abs-mvc/src/abs/controller/ProgramController.abs",145);
                if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("/home/afifun/abs/abstools-niken/abstools/abs-frameworks/abs-mvc/src/abs/controller/ProgramController.abs",146);
                MProgramModel.Program_i p = ABS.StdLib.nth_f.apply(programs, index);
                if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().getCurrentTask().setLocalVariable("p",p);
                if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("/home/afifun/abs/abstools-niken/abstools/abs-frameworks/abs-mvc/src/abs/controller/ProgramController.abs",147);
                dataModel = ABS.StdLib.appendright_f.apply(dataModel, p);
                if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().getCurrentTask().setLocalVariable("dataModel", dataModel);if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("/home/afifun/abs/abstools-niken/abstools/abs-frameworks/abs-mvc/src/abs/controller/ProgramController.abs",148);
                index = index.add(abs.backend.java.lib.types.ABSInteger.fromString("1"));
                if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().getCurrentTask().setLocalVariable("index", index);}
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("/home/afifun/abs/abstools-niken/abstools/abs-frameworks/abs-mvc/src/abs/controller/ProgramController.abs",151);
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().getCurrentTask().popStackFrame();
            return new ABS.StdLib.Pair_Pair(abs.backend.java.lib.types.ABSString.fromString("program/list"), dataModel);
        }
    }
    // DFormSourceController.abs:0:0: 
    public final abs.backend.java.lib.runtime.ABSFut<ABS.StdLib.Pair<abs.backend.java.lib.types.ABSString,ABS.StdLib.List<MProgramModel.Program_i>>> async_detail(ABS.Framework.Http.ABSHttpRequest_i request) {
        return (abs.backend.java.lib.runtime.ABSFut)abs.backend.java.lib.runtime.ABSRuntime.getCurrentRuntime().asyncCall(new abs.backend.java.lib.runtime.AbstractAsyncCallRT<MProgramController.ProgramControllerImpl_c>(
            this,
            abs.backend.java.lib.runtime.ABSRuntime.checkForNull(this),
            new ABS.StdLib.Duration_InfDuration(),
            new ABS.StdLib.Duration_InfDuration(),
            abs.backend.java.lib.types.ABSBool.FALSE) {
                ABS.Framework.Http.ABSHttpRequest_i arg0;
                public java.util.List<abs.backend.java.lib.types.ABSValue> getArgs() {
                    return java.util.Arrays.asList(new abs.backend.java.lib.types.ABSValue[] {
                        arg0});
                }
                public abs.backend.java.lib.runtime.AsyncCall<?> init(ABS.Framework.Http.ABSHttpRequest_i _arg0) {
                    arg0 = _arg0;
                    return this;
                }
                public java.lang.String methodName() {
                    return "detail";
                }
                public Object execute() {
                    return target.detail(arg0
                    );
                }
            }.init(request))
        ;
    }
    // DFormSourceController.abs:0:0: 
    public final ABS.StdLib.Pair<abs.backend.java.lib.types.ABSString,ABS.StdLib.List<MProgramModel.Program_i>> detail(ABS.Framework.Http.ABSHttpRequest_i request) {
        __ABS_checkSameCOG(); 
        if (__ABS_getRuntime().debuggingEnabled()) {
            abs.backend.java.lib.runtime.Task<?> __ABS_currentTask = __ABS_getRuntime().getCurrentTask();
            __ABS_currentTask.newStackFrame(this, "detail");
            __ABS_currentTask.setLocalVariable("request",request);
        }
         {
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("/home/afifun/abs/abstools-niken/abstools/abs-frameworks/abs-mvc/src/abs/controller/ProgramController.abs",0);
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("/home/afifun/abs/abstools-niken/abstools/abs-frameworks/abs-mvc/src/abs/controller/ProgramController.abs",105);
            MProgramModel.ProgramDb_i orm = MProgramModel.ProgramDbImpl_c.__ABS_createNewObject(this);
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().getCurrentTask().setLocalVariable("orm",orm);
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("/home/afifun/abs/abstools-niken/abstools/abs-frameworks/abs-mvc/src/abs/controller/ProgramController.abs",107);
            abs.backend.java.lib.types.ABSString id = abs.backend.java.lib.runtime.ABSRuntime.checkForNull(request).getInput(abs.backend.java.lib.types.ABSString.fromString("id"));
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().getCurrentTask().setLocalVariable("id",id);
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("/home/afifun/abs/abstools-niken/abstools/abs-frameworks/abs-mvc/src/abs/controller/ProgramController.abs",109);
            abs.backend.java.lib.types.ABSString condition = abs.backend.java.lib.types.ABSString.fromString("idProgram=").add(id);
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().getCurrentTask().setLocalVariable("condition",condition);
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("/home/afifun/abs/abstools-niken/abstools/abs-frameworks/abs-mvc/src/abs/controller/ProgramController.abs",111);
            ABS.StdLib.List<MProgramModel.Program_i> dataModel = new ABS.StdLib.List_Nil();
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().getCurrentTask().setLocalVariable("dataModel",dataModel);
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("/home/afifun/abs/abstools-niken/abstools/abs-frameworks/abs-mvc/src/abs/controller/ProgramController.abs",112);
            MProgramModel.Program_i p = abs.backend.java.lib.runtime.ABSRuntime.checkForNull(orm).findByAttributes(abs.backend.java.lib.types.ABSString.fromString("MProgramModel.ProgramImpl_c"), condition);
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().getCurrentTask().setLocalVariable("p",p);
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("/home/afifun/abs/abstools-niken/abstools/abs-frameworks/abs-mvc/src/abs/controller/ProgramController.abs",113);
            dataModel = ABS.StdLib.appendright_f.apply(dataModel, p);
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().getCurrentTask().setLocalVariable("dataModel", dataModel);if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("/home/afifun/abs/abstools-niken/abstools/abs-frameworks/abs-mvc/src/abs/controller/ProgramController.abs",115);
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().getCurrentTask().popStackFrame();
            return new ABS.StdLib.Pair_Pair(abs.backend.java.lib.types.ABSString.fromString("program/delta/detailSource"), dataModel);
        }
    }
    // DGuidePageController.abs:0:0: 
    public final abs.backend.java.lib.runtime.ABSFut<ABS.StdLib.Pair<abs.backend.java.lib.types.ABSString,ABS.StdLib.List<abs.backend.java.lib.types.ABSString>>> async_guide(ABS.Framework.Http.ABSHttpRequest_i request) {
        return (abs.backend.java.lib.runtime.ABSFut)abs.backend.java.lib.runtime.ABSRuntime.getCurrentRuntime().asyncCall(new abs.backend.java.lib.runtime.AbstractAsyncCallRT<MProgramController.ProgramControllerImpl_c>(
            this,
            abs.backend.java.lib.runtime.ABSRuntime.checkForNull(this),
            new ABS.StdLib.Duration_InfDuration(),
            new ABS.StdLib.Duration_InfDuration(),
            abs.backend.java.lib.types.ABSBool.FALSE) {
                ABS.Framework.Http.ABSHttpRequest_i arg0;
                public java.util.List<abs.backend.java.lib.types.ABSValue> getArgs() {
                    return java.util.Arrays.asList(new abs.backend.java.lib.types.ABSValue[] {
                        arg0});
                }
                public abs.backend.java.lib.runtime.AsyncCall<?> init(ABS.Framework.Http.ABSHttpRequest_i _arg0) {
                    arg0 = _arg0;
                    return this;
                }
                public java.lang.String methodName() {
                    return "guide";
                }
                public Object execute() {
                    return target.guide(arg0
                    );
                }
            }.init(request))
        ;
    }
    // DGuidePageController.abs:0:0: 
    public final ABS.StdLib.Pair<abs.backend.java.lib.types.ABSString,ABS.StdLib.List<abs.backend.java.lib.types.ABSString>> guide(ABS.Framework.Http.ABSHttpRequest_i request) {
        __ABS_checkSameCOG(); 
        if (__ABS_getRuntime().debuggingEnabled()) {
            abs.backend.java.lib.runtime.Task<?> __ABS_currentTask = __ABS_getRuntime().getCurrentTask();
            __ABS_currentTask.newStackFrame(this, "guide");
            __ABS_currentTask.setLocalVariable("request",request);
        }
         {
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("/home/afifun/abs/abstools-niken/abstools/abs-frameworks/abs-mvc/src/abs/controller/ProgramController.abs",0);
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("/home/afifun/abs/abstools-niken/abstools/abs-frameworks/abs-mvc/src/abs/controller/ProgramController.abs",13);
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().getCurrentTask().popStackFrame();
            return new ABS.StdLib.Pair_Pair(abs.backend.java.lib.types.ABSString.fromString("guide"), new ABS.StdLib.List_Nil());
        }
    }
}
