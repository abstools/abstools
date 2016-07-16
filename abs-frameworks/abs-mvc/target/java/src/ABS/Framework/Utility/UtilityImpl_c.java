package ABS.Framework.Utility;
// Utility.abs:11:0: 
public final class UtilityImpl_c extends abs.backend.java.lib.runtime.ABSObject implements abs.backend.java.lib.types.ABSClass, ABS.Framework.Utility.Utility_i {
    private static final java.lang.String[] __fieldNames = new java.lang.String[] {  };
    public final java.util.List<java.lang.String> getFieldNames() { return java.util.Arrays.asList(__fieldNames); }
    public UtilityImpl_c() {
        getCOG().objectCreated(this);
    }
    protected final void __ABS_init() {
        getCOG().objectInitialized(this);
    }
    protected final abs.backend.java.lib.types.ABSValue getFieldValue(java.lang.String __ABS_fieldName) throws java.lang.NoSuchFieldException {
        return super.getFieldValue(__ABS_fieldName);
    }
    public final java.lang.String getClassName() { return "UtilityImpl"; }
    public static final <T extends UtilityImpl_c> T createNewCOG() { return (T)UtilityImpl_c.__ABS_createNewCOG(null, null); }
    public static final <T extends UtilityImpl_c> T __ABS_createNewCOG(abs.backend.java.lib.runtime.ABSObject __ABS_source, abs.backend.java.scheduling.UserSchedulingStrategy strategy) {
        final abs.backend.java.lib.runtime.ABSRuntime __ABS_runtime = abs.backend.java.lib.runtime.ABSRuntime.getCurrentRuntime();
        final abs.backend.java.lib.runtime.COG __ABS_cog = strategy == null ? __ABS_runtime.createCOG(UtilityImpl_c.class) : __ABS_runtime.createCOG(UtilityImpl_c.class, strategy);
        final abs.backend.java.lib.runtime.ABSThread __ABS_thread = abs.backend.java.lib.runtime.ABSRuntime.getCurrentThread();
        final abs.backend.java.lib.runtime.COG __ABS_oldCOG = abs.backend.java.lib.runtime.ABSRuntime.getCurrentCOG();
        final abs.backend.java.lib.runtime.Task __ABS_sendingTask = abs.backend.java.lib.runtime.ABSRuntime.getCurrentTask();
        __ABS_thread.setCOG(__ABS_cog);
        try {
            UtilityImpl_c __ABS_result = new UtilityImpl_c();
            ;
            __ABS_runtime.cogCreated(__ABS_result);
            __ABS_cog.getScheduler().addTask(new abs.backend.java.lib.runtime.Task(new abs.backend.java.lib.runtime.ABSInitObjectCall(__ABS_sendingTask,__ABS_source,__ABS_result)));
            return (T)__ABS_result;
        } finally {
            __ABS_thread.setCOG(__ABS_oldCOG);
        }
    }
    public static final <T extends UtilityImpl_c> T createNewObject() { return (T)UtilityImpl_c.__ABS_createNewObject(null); }
    public static final <T extends UtilityImpl_c> T __ABS_createNewObject(abs.backend.java.lib.runtime.ABSObject __ABS_source) {
        UtilityImpl_c __ABS_result = new UtilityImpl_c();
        __ABS_result.__ABS_init();
        return (T)__ABS_result;
    }
    // Utility.abs:0:0: 
    public final abs.backend.java.lib.runtime.ABSFut<abs.backend.java.lib.types.ABSInteger> async_stringToInteger(abs.backend.java.lib.types.ABSString s) {
        return (abs.backend.java.lib.runtime.ABSFut)abs.backend.java.lib.runtime.ABSRuntime.getCurrentRuntime().asyncCall(new abs.backend.java.lib.runtime.AbstractAsyncCallRT<ABS.Framework.Utility.UtilityImpl_c>(
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
                    return "stringToInteger";
                }
                public Object execute() {
                    return target.stringToInteger(arg0
                    );
                }
            }.init(s))
        ;
    }
    // Utility.abs:0:0: 
    public final abs.backend.java.lib.types.ABSInteger stringToInteger(abs.backend.java.lib.types.ABSString s) {
        __ABS_checkSameCOG(); 
        if (__ABS_getRuntime().debuggingEnabled()) {
            abs.backend.java.lib.runtime.Task<?> __ABS_currentTask = __ABS_getRuntime().getCurrentTask();
            __ABS_currentTask.newStackFrame(this, "stringToInteger");
            __ABS_currentTask.setLocalVariable("s",s);
        }
         {
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("C:\\Users\\ls\\Documents\\afifun\\abs-microservices-framework\\abstools\\abs-frameworks\\abs-mvc\\src\\abs\\framework\\Utility.abs",0);
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("C:\\Users\\ls\\Documents\\afifun\\abs-microservices-framework\\abstools\\abs-frameworks\\abs-mvc\\src\\abs\\framework\\Utility.abs",15);
            abs.backend.java.lib.types.ABSString inputString = s;
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().getCurrentTask().setLocalVariable("inputString",inputString);
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("C:\\Users\\ls\\Documents\\afifun\\abs-microservices-framework\\abstools\\abs-frameworks\\abs-mvc\\src\\abs\\framework\\Utility.abs",16);
            abs.backend.java.lib.types.ABSInteger length = abs.backend.java.lib.runtime.ABSBuiltInFunctions.strlen(inputString);
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().getCurrentTask().setLocalVariable("length",length);
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("C:\\Users\\ls\\Documents\\afifun\\abs-microservices-framework\\abstools\\abs-frameworks\\abs-mvc\\src\\abs\\framework\\Utility.abs",17);
            abs.backend.java.lib.types.ABSInteger output = abs.backend.java.lib.types.ABSInteger.fromString("0");
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().getCurrentTask().setLocalVariable("output",output);
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("C:\\Users\\ls\\Documents\\afifun\\abs-microservices-framework\\abstools\\abs-frameworks\\abs-mvc\\src\\abs\\framework\\Utility.abs",19);
            if (abs.backend.java.lib.expr.BinOp.lt(length,abs.backend.java.lib.types.ABSInteger.fromString("1")).toBoolean()) {
                 {
                    if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("C:\\Users\\ls\\Documents\\afifun\\abs-microservices-framework\\abstools\\abs-frameworks\\abs-mvc\\src\\abs\\framework\\Utility.abs",20);
                    if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("C:\\Users\\ls\\Documents\\afifun\\abs-microservices-framework\\abstools\\abs-frameworks\\abs-mvc\\src\\abs\\framework\\Utility.abs",21);
                    output = abs.backend.java.lib.types.ABSInteger.fromString("-1");
                    if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().getCurrentTask().setLocalVariable("output", output);}
            }
            else {
                 {
                    if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("C:\\Users\\ls\\Documents\\afifun\\abs-microservices-framework\\abstools\\abs-frameworks\\abs-mvc\\src\\abs\\framework\\Utility.abs",24);
                    if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("C:\\Users\\ls\\Documents\\afifun\\abs-microservices-framework\\abstools\\abs-frameworks\\abs-mvc\\src\\abs\\framework\\Utility.abs",25);
                    abs.backend.java.lib.types.ABSBool negative = isNegativeNumber(inputString);
                    if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().getCurrentTask().setLocalVariable("negative",negative);
                    if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("C:\\Users\\ls\\Documents\\afifun\\abs-microservices-framework\\abstools\\abs-frameworks\\abs-mvc\\src\\abs\\framework\\Utility.abs",27);
                    if (negative.toBoolean()) {
                         {
                            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("C:\\Users\\ls\\Documents\\afifun\\abs-microservices-framework\\abstools\\abs-frameworks\\abs-mvc\\src\\abs\\framework\\Utility.abs",28);
                            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("C:\\Users\\ls\\Documents\\afifun\\abs-microservices-framework\\abstools\\abs-frameworks\\abs-mvc\\src\\abs\\framework\\Utility.abs",29);
                            inputString = abs.backend.java.lib.runtime.ABSBuiltInFunctions.substr(inputString, abs.backend.java.lib.types.ABSInteger.fromString("1"), length.subtract(abs.backend.java.lib.types.ABSInteger.fromString("1")));
                            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().getCurrentTask().setLocalVariable("inputString", inputString);}
                    }
                    if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("C:\\Users\\ls\\Documents\\afifun\\abs-microservices-framework\\abstools\\abs-frameworks\\abs-mvc\\src\\abs\\framework\\Utility.abs",32);
                    length = abs.backend.java.lib.runtime.ABSBuiltInFunctions.strlen(inputString);
                    if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().getCurrentTask().setLocalVariable("length", length);if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("C:\\Users\\ls\\Documents\\afifun\\abs-microservices-framework\\abstools\\abs-frameworks\\abs-mvc\\src\\abs\\framework\\Utility.abs",33);
                    abs.backend.java.lib.types.ABSInteger idx = abs.backend.java.lib.types.ABSInteger.fromString("0");
                    if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().getCurrentTask().setLocalVariable("idx",idx);
                    if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("C:\\Users\\ls\\Documents\\afifun\\abs-microservices-framework\\abstools\\abs-frameworks\\abs-mvc\\src\\abs\\framework\\Utility.abs",34);
                    abs.backend.java.lib.types.ABSInteger tens = power(abs.backend.java.lib.types.ABSInteger.fromString("1"), length);
                    if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().getCurrentTask().setLocalVariable("tens",tens);
                    if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("C:\\Users\\ls\\Documents\\afifun\\abs-microservices-framework\\abstools\\abs-frameworks\\abs-mvc\\src\\abs\\framework\\Utility.abs",36);
                    while (abs.backend.java.lib.expr.BinOp.lt(idx,length).toBoolean()) {
                        if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("C:\\Users\\ls\\Documents\\afifun\\abs-microservices-framework\\abstools\\abs-frameworks\\abs-mvc\\src\\abs\\framework\\Utility.abs",37);
                        if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("C:\\Users\\ls\\Documents\\afifun\\abs-microservices-framework\\abstools\\abs-frameworks\\abs-mvc\\src\\abs\\framework\\Utility.abs",39);
                        abs.backend.java.lib.types.ABSString ch = abs.backend.java.lib.runtime.ABSBuiltInFunctions.substr(inputString, abs.backend.java.lib.types.ABSInteger.fromString("0"), abs.backend.java.lib.types.ABSInteger.fromString("1"));
                        if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().getCurrentTask().setLocalVariable("ch",ch);
                        if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("C:\\Users\\ls\\Documents\\afifun\\abs-microservices-framework\\abstools\\abs-frameworks\\abs-mvc\\src\\abs\\framework\\Utility.abs",40);
                        abs.backend.java.lib.types.ABSInteger digit = parseDigit(ch);
                        if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().getCurrentTask().setLocalVariable("digit",digit);
                        if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("C:\\Users\\ls\\Documents\\afifun\\abs-microservices-framework\\abstools\\abs-frameworks\\abs-mvc\\src\\abs\\framework\\Utility.abs",43);
                        abs.backend.java.lib.types.ABSInteger temp = digit.multiply(tens);
                        if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().getCurrentTask().setLocalVariable("temp",temp);
                        if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("C:\\Users\\ls\\Documents\\afifun\\abs-microservices-framework\\abstools\\abs-frameworks\\abs-mvc\\src\\abs\\framework\\Utility.abs",46);
                        output = output.add(temp);
                        if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().getCurrentTask().setLocalVariable("output", output);if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("C:\\Users\\ls\\Documents\\afifun\\abs-microservices-framework\\abstools\\abs-frameworks\\abs-mvc\\src\\abs\\framework\\Utility.abs",49);
                        idx = idx.add(abs.backend.java.lib.types.ABSInteger.fromString("1"));
                        if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().getCurrentTask().setLocalVariable("idx", idx);if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("C:\\Users\\ls\\Documents\\afifun\\abs-microservices-framework\\abstools\\abs-frameworks\\abs-mvc\\src\\abs\\framework\\Utility.abs",50);
                        tens = tens.divide(abs.backend.java.lib.types.ABSInteger.fromString("10")).truncate();
                        if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().getCurrentTask().setLocalVariable("tens", tens);if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("C:\\Users\\ls\\Documents\\afifun\\abs-microservices-framework\\abstools\\abs-frameworks\\abs-mvc\\src\\abs\\framework\\Utility.abs",53);
                        abs.backend.java.lib.types.ABSInteger remainingLength = abs.backend.java.lib.runtime.ABSBuiltInFunctions.strlen(inputString);
                        if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().getCurrentTask().setLocalVariable("remainingLength",remainingLength);
                        if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("C:\\Users\\ls\\Documents\\afifun\\abs-microservices-framework\\abstools\\abs-frameworks\\abs-mvc\\src\\abs\\framework\\Utility.abs",54);
                        inputString = abs.backend.java.lib.runtime.ABSBuiltInFunctions.substr(inputString, abs.backend.java.lib.types.ABSInteger.fromString("1"), remainingLength.subtract(abs.backend.java.lib.types.ABSInteger.fromString("1")));
                        if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().getCurrentTask().setLocalVariable("inputString", inputString);}
                    if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("C:\\Users\\ls\\Documents\\afifun\\abs-microservices-framework\\abstools\\abs-frameworks\\abs-mvc\\src\\abs\\framework\\Utility.abs",57);
                    if (negative.toBoolean()) {
                         {
                            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("C:\\Users\\ls\\Documents\\afifun\\abs-microservices-framework\\abstools\\abs-frameworks\\abs-mvc\\src\\abs\\framework\\Utility.abs",58);
                            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("C:\\Users\\ls\\Documents\\afifun\\abs-microservices-framework\\abstools\\abs-frameworks\\abs-mvc\\src\\abs\\framework\\Utility.abs",59);
                            output = output.multiply(abs.backend.java.lib.types.ABSInteger.fromString("-1"));
                            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().getCurrentTask().setLocalVariable("output", output);}
                    }
                }
            }
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("C:\\Users\\ls\\Documents\\afifun\\abs-microservices-framework\\abstools\\abs-frameworks\\abs-mvc\\src\\abs\\framework\\Utility.abs",63);
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().getCurrentTask().popStackFrame();
            return output;
        }
    }
    // Utility.abs:0:0: 
    public final abs.backend.java.lib.runtime.ABSFut<abs.backend.java.lib.types.ABSRational> async_stringToRational(abs.backend.java.lib.types.ABSString s) {
        return (abs.backend.java.lib.runtime.ABSFut)abs.backend.java.lib.runtime.ABSRuntime.getCurrentRuntime().asyncCall(new abs.backend.java.lib.runtime.AbstractAsyncCallRT<ABS.Framework.Utility.UtilityImpl_c>(
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
                    return "stringToRational";
                }
                public Object execute() {
                    return target.stringToRational(arg0
                    );
                }
            }.init(s))
        ;
    }
    // Utility.abs:0:0: 
    public final abs.backend.java.lib.types.ABSRational stringToRational(abs.backend.java.lib.types.ABSString s) {
        __ABS_checkSameCOG(); 
        if (__ABS_getRuntime().debuggingEnabled()) {
            abs.backend.java.lib.runtime.Task<?> __ABS_currentTask = __ABS_getRuntime().getCurrentTask();
            __ABS_currentTask.newStackFrame(this, "stringToRational");
            __ABS_currentTask.setLocalVariable("s",s);
        }
         {
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("C:\\Users\\ls\\Documents\\afifun\\abs-microservices-framework\\abstools\\abs-frameworks\\abs-mvc\\src\\abs\\framework\\Utility.abs",0);
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("C:\\Users\\ls\\Documents\\afifun\\abs-microservices-framework\\abstools\\abs-frameworks\\abs-mvc\\src\\abs\\framework\\Utility.abs",68);
            abs.backend.java.lib.types.ABSString inputString = s;
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().getCurrentTask().setLocalVariable("inputString",inputString);
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("C:\\Users\\ls\\Documents\\afifun\\abs-microservices-framework\\abstools\\abs-frameworks\\abs-mvc\\src\\abs\\framework\\Utility.abs",69);
            abs.backend.java.lib.types.ABSInteger length = abs.backend.java.lib.runtime.ABSBuiltInFunctions.strlen(inputString);
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().getCurrentTask().setLocalVariable("length",length);
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("C:\\Users\\ls\\Documents\\afifun\\abs-microservices-framework\\abstools\\abs-frameworks\\abs-mvc\\src\\abs\\framework\\Utility.abs",70);
            abs.backend.java.lib.types.ABSRational output = abs.backend.java.lib.types.ABSInteger.fromString("0");
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().getCurrentTask().setLocalVariable("output",output);
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("C:\\Users\\ls\\Documents\\afifun\\abs-microservices-framework\\abstools\\abs-frameworks\\abs-mvc\\src\\abs\\framework\\Utility.abs",72);
            if (abs.backend.java.lib.expr.BinOp.lt(length,abs.backend.java.lib.types.ABSInteger.fromString("1")).toBoolean()) {
                 {
                    if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("C:\\Users\\ls\\Documents\\afifun\\abs-microservices-framework\\abstools\\abs-frameworks\\abs-mvc\\src\\abs\\framework\\Utility.abs",73);
                    if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("C:\\Users\\ls\\Documents\\afifun\\abs-microservices-framework\\abstools\\abs-frameworks\\abs-mvc\\src\\abs\\framework\\Utility.abs",74);
                    output = abs.backend.java.lib.types.ABSInteger.fromString("-1");
                    if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().getCurrentTask().setLocalVariable("output", output);}
            }
            else {
                 {
                    if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("C:\\Users\\ls\\Documents\\afifun\\abs-microservices-framework\\abstools\\abs-frameworks\\abs-mvc\\src\\abs\\framework\\Utility.abs",77);
                    if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("C:\\Users\\ls\\Documents\\afifun\\abs-microservices-framework\\abstools\\abs-frameworks\\abs-mvc\\src\\abs\\framework\\Utility.abs",78);
                    abs.backend.java.lib.types.ABSBool negative = isNegativeNumber(inputString);
                    if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().getCurrentTask().setLocalVariable("negative",negative);
                    if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("C:\\Users\\ls\\Documents\\afifun\\abs-microservices-framework\\abstools\\abs-frameworks\\abs-mvc\\src\\abs\\framework\\Utility.abs",80);
                    if (negative.toBoolean()) {
                         {
                            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("C:\\Users\\ls\\Documents\\afifun\\abs-microservices-framework\\abstools\\abs-frameworks\\abs-mvc\\src\\abs\\framework\\Utility.abs",81);
                            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("C:\\Users\\ls\\Documents\\afifun\\abs-microservices-framework\\abstools\\abs-frameworks\\abs-mvc\\src\\abs\\framework\\Utility.abs",82);
                            inputString = abs.backend.java.lib.runtime.ABSBuiltInFunctions.substr(inputString, abs.backend.java.lib.types.ABSInteger.fromString("1"), length.subtract(abs.backend.java.lib.types.ABSInteger.fromString("1")));
                            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().getCurrentTask().setLocalVariable("inputString", inputString);}
                    }
                    if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("C:\\Users\\ls\\Documents\\afifun\\abs-microservices-framework\\abstools\\abs-frameworks\\abs-mvc\\src\\abs\\framework\\Utility.abs",85);
                    length = abs.backend.java.lib.runtime.ABSBuiltInFunctions.strlen(inputString);
                    if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().getCurrentTask().setLocalVariable("length", length);if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("C:\\Users\\ls\\Documents\\afifun\\abs-microservices-framework\\abstools\\abs-frameworks\\abs-mvc\\src\\abs\\framework\\Utility.abs",86);
                    abs.backend.java.lib.types.ABSInteger idx = abs.backend.java.lib.types.ABSInteger.fromString("0");
                    if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().getCurrentTask().setLocalVariable("idx",idx);
                    if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("C:\\Users\\ls\\Documents\\afifun\\abs-microservices-framework\\abstools\\abs-frameworks\\abs-mvc\\src\\abs\\framework\\Utility.abs",87);
                    abs.backend.java.lib.types.ABSInteger tens = power(abs.backend.java.lib.types.ABSInteger.fromString("1"), length);
                    if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().getCurrentTask().setLocalVariable("tens",tens);
                    if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("C:\\Users\\ls\\Documents\\afifun\\abs-microservices-framework\\abstools\\abs-frameworks\\abs-mvc\\src\\abs\\framework\\Utility.abs",89);
                    while (abs.backend.java.lib.expr.BinOp.lt(idx,length).toBoolean()) {
                        if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("C:\\Users\\ls\\Documents\\afifun\\abs-microservices-framework\\abstools\\abs-frameworks\\abs-mvc\\src\\abs\\framework\\Utility.abs",90);
                        if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("C:\\Users\\ls\\Documents\\afifun\\abs-microservices-framework\\abstools\\abs-frameworks\\abs-mvc\\src\\abs\\framework\\Utility.abs",92);
                        abs.backend.java.lib.types.ABSString ch = abs.backend.java.lib.runtime.ABSBuiltInFunctions.substr(inputString, abs.backend.java.lib.types.ABSInteger.fromString("0"), abs.backend.java.lib.types.ABSInteger.fromString("1"));
                        if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().getCurrentTask().setLocalVariable("ch",ch);
                        if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("C:\\Users\\ls\\Documents\\afifun\\abs-microservices-framework\\abstools\\abs-frameworks\\abs-mvc\\src\\abs\\framework\\Utility.abs",93);
                        abs.backend.java.lib.types.ABSInteger digit = parseDigit(ch);
                        if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().getCurrentTask().setLocalVariable("digit",digit);
                        if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("C:\\Users\\ls\\Documents\\afifun\\abs-microservices-framework\\abstools\\abs-frameworks\\abs-mvc\\src\\abs\\framework\\Utility.abs",96);
                        abs.backend.java.lib.types.ABSInteger temp = digit.multiply(tens);
                        if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().getCurrentTask().setLocalVariable("temp",temp);
                        if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("C:\\Users\\ls\\Documents\\afifun\\abs-microservices-framework\\abstools\\abs-frameworks\\abs-mvc\\src\\abs\\framework\\Utility.abs",99);
                        output = output.add(temp);
                        if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().getCurrentTask().setLocalVariable("output", output);if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("C:\\Users\\ls\\Documents\\afifun\\abs-microservices-framework\\abstools\\abs-frameworks\\abs-mvc\\src\\abs\\framework\\Utility.abs",102);
                        idx = idx.add(abs.backend.java.lib.types.ABSInteger.fromString("1"));
                        if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().getCurrentTask().setLocalVariable("idx", idx);if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("C:\\Users\\ls\\Documents\\afifun\\abs-microservices-framework\\abstools\\abs-frameworks\\abs-mvc\\src\\abs\\framework\\Utility.abs",103);
                        tens = tens.divide(abs.backend.java.lib.types.ABSInteger.fromString("10")).truncate();
                        if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().getCurrentTask().setLocalVariable("tens", tens);if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("C:\\Users\\ls\\Documents\\afifun\\abs-microservices-framework\\abstools\\abs-frameworks\\abs-mvc\\src\\abs\\framework\\Utility.abs",106);
                        abs.backend.java.lib.types.ABSInteger remainingLength = abs.backend.java.lib.runtime.ABSBuiltInFunctions.strlen(inputString);
                        if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().getCurrentTask().setLocalVariable("remainingLength",remainingLength);
                        if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("C:\\Users\\ls\\Documents\\afifun\\abs-microservices-framework\\abstools\\abs-frameworks\\abs-mvc\\src\\abs\\framework\\Utility.abs",107);
                        inputString = abs.backend.java.lib.runtime.ABSBuiltInFunctions.substr(inputString, abs.backend.java.lib.types.ABSInteger.fromString("1"), remainingLength.subtract(abs.backend.java.lib.types.ABSInteger.fromString("1")));
                        if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().getCurrentTask().setLocalVariable("inputString", inputString);}
                    if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("C:\\Users\\ls\\Documents\\afifun\\abs-microservices-framework\\abstools\\abs-frameworks\\abs-mvc\\src\\abs\\framework\\Utility.abs",110);
                    if (negative.toBoolean()) {
                         {
                            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("C:\\Users\\ls\\Documents\\afifun\\abs-microservices-framework\\abstools\\abs-frameworks\\abs-mvc\\src\\abs\\framework\\Utility.abs",111);
                            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("C:\\Users\\ls\\Documents\\afifun\\abs-microservices-framework\\abstools\\abs-frameworks\\abs-mvc\\src\\abs\\framework\\Utility.abs",112);
                            output = output.multiply(abs.backend.java.lib.types.ABSInteger.fromString("-1"));
                            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().getCurrentTask().setLocalVariable("output", output);}
                    }
                }
            }
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("C:\\Users\\ls\\Documents\\afifun\\abs-microservices-framework\\abstools\\abs-frameworks\\abs-mvc\\src\\abs\\framework\\Utility.abs",116);
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().getCurrentTask().popStackFrame();
            return output;
        }
    }
    // Utility.abs:0:0: 
    public final abs.backend.java.lib.runtime.ABSFut<abs.backend.java.lib.types.ABSInteger> async_power(abs.backend.java.lib.types.ABSInteger a, abs.backend.java.lib.types.ABSInteger b) {
        return (abs.backend.java.lib.runtime.ABSFut)abs.backend.java.lib.runtime.ABSRuntime.getCurrentRuntime().asyncCall(new abs.backend.java.lib.runtime.AbstractAsyncCallRT<ABS.Framework.Utility.UtilityImpl_c>(
            this,
            abs.backend.java.lib.runtime.ABSRuntime.checkForNull(this),
            new ABS.StdLib.Duration_InfDuration(),
            new ABS.StdLib.Duration_InfDuration(),
            abs.backend.java.lib.types.ABSBool.FALSE) {
                abs.backend.java.lib.types.ABSInteger arg0;
                abs.backend.java.lib.types.ABSInteger arg1;
                public java.util.List<abs.backend.java.lib.types.ABSValue> getArgs() {
                    return java.util.Arrays.asList(new abs.backend.java.lib.types.ABSValue[] {
                        arg0,arg1});
                }
                public abs.backend.java.lib.runtime.AsyncCall<?> init(abs.backend.java.lib.types.ABSInteger _arg0,abs.backend.java.lib.types.ABSInteger _arg1) {
                    arg0 = _arg0;
                    arg1 = _arg1;
                    return this;
                }
                public java.lang.String methodName() {
                    return "power";
                }
                public Object execute() {
                    return target.power(arg0
                    .truncate(),arg1
                    .truncate());
                }
            }.init(a, b))
        ;
    }
    // Utility.abs:0:0: 
    public final abs.backend.java.lib.types.ABSInteger power(abs.backend.java.lib.types.ABSInteger a, abs.backend.java.lib.types.ABSInteger b) {
        __ABS_checkSameCOG(); 
        if (__ABS_getRuntime().debuggingEnabled()) {
            abs.backend.java.lib.runtime.Task<?> __ABS_currentTask = __ABS_getRuntime().getCurrentTask();
            __ABS_currentTask.newStackFrame(this, "power");
            __ABS_currentTask.setLocalVariable("a",a);
            __ABS_currentTask.setLocalVariable("b",b);
        }
         {
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("C:\\Users\\ls\\Documents\\afifun\\abs-microservices-framework\\abstools\\abs-frameworks\\abs-mvc\\src\\abs\\framework\\Utility.abs",0);
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("C:\\Users\\ls\\Documents\\afifun\\abs-microservices-framework\\abstools\\abs-frameworks\\abs-mvc\\src\\abs\\framework\\Utility.abs",121);
            abs.backend.java.lib.types.ABSInteger result = a;
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().getCurrentTask().setLocalVariable("result",result);
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("C:\\Users\\ls\\Documents\\afifun\\abs-microservices-framework\\abstools\\abs-frameworks\\abs-mvc\\src\\abs\\framework\\Utility.abs",122);
            abs.backend.java.lib.types.ABSInteger n = b;
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().getCurrentTask().setLocalVariable("n",n);
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("C:\\Users\\ls\\Documents\\afifun\\abs-microservices-framework\\abstools\\abs-frameworks\\abs-mvc\\src\\abs\\framework\\Utility.abs",124);
            while (abs.backend.java.lib.expr.BinOp.gt(n,abs.backend.java.lib.types.ABSInteger.fromString("1")).toBoolean()) {
                if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("C:\\Users\\ls\\Documents\\afifun\\abs-microservices-framework\\abstools\\abs-frameworks\\abs-mvc\\src\\abs\\framework\\Utility.abs",125);
                if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("C:\\Users\\ls\\Documents\\afifun\\abs-microservices-framework\\abstools\\abs-frameworks\\abs-mvc\\src\\abs\\framework\\Utility.abs",126);
                result = result.multiply(abs.backend.java.lib.types.ABSInteger.fromString("10"));
                if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().getCurrentTask().setLocalVariable("result", result);if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("C:\\Users\\ls\\Documents\\afifun\\abs-microservices-framework\\abstools\\abs-frameworks\\abs-mvc\\src\\abs\\framework\\Utility.abs",127);
                n = n.subtract(abs.backend.java.lib.types.ABSInteger.fromString("1"));
                if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().getCurrentTask().setLocalVariable("n", n);}
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("C:\\Users\\ls\\Documents\\afifun\\abs-microservices-framework\\abstools\\abs-frameworks\\abs-mvc\\src\\abs\\framework\\Utility.abs",130);
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().getCurrentTask().popStackFrame();
            return result;
        }
    }
    // Utility.abs:0:0: 
    public final abs.backend.java.lib.runtime.ABSFut<abs.backend.java.lib.types.ABSInteger> async_parseDigit(abs.backend.java.lib.types.ABSString ch) {
        return (abs.backend.java.lib.runtime.ABSFut)abs.backend.java.lib.runtime.ABSRuntime.getCurrentRuntime().asyncCall(new abs.backend.java.lib.runtime.AbstractAsyncCallRT<ABS.Framework.Utility.UtilityImpl_c>(
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
                    return "parseDigit";
                }
                public Object execute() {
                    return target.parseDigit(arg0
                    );
                }
            }.init(ch))
        ;
    }
    // Utility.abs:0:0: 
    public final abs.backend.java.lib.types.ABSInteger parseDigit(abs.backend.java.lib.types.ABSString ch) {
        __ABS_checkSameCOG(); 
        if (__ABS_getRuntime().debuggingEnabled()) {
            abs.backend.java.lib.runtime.Task<?> __ABS_currentTask = __ABS_getRuntime().getCurrentTask();
            __ABS_currentTask.newStackFrame(this, "parseDigit");
            __ABS_currentTask.setLocalVariable("ch",ch);
        }
         {
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("C:\\Users\\ls\\Documents\\afifun\\abs-microservices-framework\\abstools\\abs-frameworks\\abs-mvc\\src\\abs\\framework\\Utility.abs",0);
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("C:\\Users\\ls\\Documents\\afifun\\abs-microservices-framework\\abstools\\abs-frameworks\\abs-mvc\\src\\abs\\framework\\Utility.abs",135);
            abs.backend.java.lib.types.ABSInteger output = abs.backend.java.lib.types.ABSInteger.fromString("-1");
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().getCurrentTask().setLocalVariable("output",output);
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("C:\\Users\\ls\\Documents\\afifun\\abs-microservices-framework\\abstools\\abs-frameworks\\abs-mvc\\src\\abs\\framework\\Utility.abs",137);
            if (abs.backend.java.lib.expr.BinOp.eq(ch,abs.backend.java.lib.types.ABSString.fromString("0")).toBoolean()) {
                 {
                    if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("C:\\Users\\ls\\Documents\\afifun\\abs-microservices-framework\\abstools\\abs-frameworks\\abs-mvc\\src\\abs\\framework\\Utility.abs",138);
                    if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("C:\\Users\\ls\\Documents\\afifun\\abs-microservices-framework\\abstools\\abs-frameworks\\abs-mvc\\src\\abs\\framework\\Utility.abs",139);
                    output = abs.backend.java.lib.types.ABSInteger.fromString("0");
                    if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().getCurrentTask().setLocalVariable("output", output);}
            }
            else {
                if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("C:\\Users\\ls\\Documents\\afifun\\abs-microservices-framework\\abstools\\abs-frameworks\\abs-mvc\\src\\abs\\framework\\Utility.abs",141);
                if (abs.backend.java.lib.expr.BinOp.eq(ch,abs.backend.java.lib.types.ABSString.fromString("1")).toBoolean()) {
                     {
                        if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("C:\\Users\\ls\\Documents\\afifun\\abs-microservices-framework\\abstools\\abs-frameworks\\abs-mvc\\src\\abs\\framework\\Utility.abs",142);
                        if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("C:\\Users\\ls\\Documents\\afifun\\abs-microservices-framework\\abstools\\abs-frameworks\\abs-mvc\\src\\abs\\framework\\Utility.abs",143);
                        output = abs.backend.java.lib.types.ABSInteger.fromString("1");
                        if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().getCurrentTask().setLocalVariable("output", output);}
                }
                else {
                    if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("C:\\Users\\ls\\Documents\\afifun\\abs-microservices-framework\\abstools\\abs-frameworks\\abs-mvc\\src\\abs\\framework\\Utility.abs",145);
                    if (abs.backend.java.lib.expr.BinOp.eq(ch,abs.backend.java.lib.types.ABSString.fromString("2")).toBoolean()) {
                         {
                            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("C:\\Users\\ls\\Documents\\afifun\\abs-microservices-framework\\abstools\\abs-frameworks\\abs-mvc\\src\\abs\\framework\\Utility.abs",146);
                            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("C:\\Users\\ls\\Documents\\afifun\\abs-microservices-framework\\abstools\\abs-frameworks\\abs-mvc\\src\\abs\\framework\\Utility.abs",147);
                            output = abs.backend.java.lib.types.ABSInteger.fromString("2");
                            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().getCurrentTask().setLocalVariable("output", output);}
                    }
                    else {
                        if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("C:\\Users\\ls\\Documents\\afifun\\abs-microservices-framework\\abstools\\abs-frameworks\\abs-mvc\\src\\abs\\framework\\Utility.abs",149);
                        if (abs.backend.java.lib.expr.BinOp.eq(ch,abs.backend.java.lib.types.ABSString.fromString("3")).toBoolean()) {
                             {
                                if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("C:\\Users\\ls\\Documents\\afifun\\abs-microservices-framework\\abstools\\abs-frameworks\\abs-mvc\\src\\abs\\framework\\Utility.abs",150);
                                if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("C:\\Users\\ls\\Documents\\afifun\\abs-microservices-framework\\abstools\\abs-frameworks\\abs-mvc\\src\\abs\\framework\\Utility.abs",151);
                                output = abs.backend.java.lib.types.ABSInteger.fromString("3");
                                if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().getCurrentTask().setLocalVariable("output", output);}
                        }
                        else {
                            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("C:\\Users\\ls\\Documents\\afifun\\abs-microservices-framework\\abstools\\abs-frameworks\\abs-mvc\\src\\abs\\framework\\Utility.abs",153);
                            if (abs.backend.java.lib.expr.BinOp.eq(ch,abs.backend.java.lib.types.ABSString.fromString("4")).toBoolean()) {
                                 {
                                    if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("C:\\Users\\ls\\Documents\\afifun\\abs-microservices-framework\\abstools\\abs-frameworks\\abs-mvc\\src\\abs\\framework\\Utility.abs",154);
                                    if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("C:\\Users\\ls\\Documents\\afifun\\abs-microservices-framework\\abstools\\abs-frameworks\\abs-mvc\\src\\abs\\framework\\Utility.abs",155);
                                    output = abs.backend.java.lib.types.ABSInteger.fromString("4");
                                    if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().getCurrentTask().setLocalVariable("output", output);}
                            }
                            else {
                                if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("C:\\Users\\ls\\Documents\\afifun\\abs-microservices-framework\\abstools\\abs-frameworks\\abs-mvc\\src\\abs\\framework\\Utility.abs",157);
                                if (abs.backend.java.lib.expr.BinOp.eq(ch,abs.backend.java.lib.types.ABSString.fromString("5")).toBoolean()) {
                                     {
                                        if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("C:\\Users\\ls\\Documents\\afifun\\abs-microservices-framework\\abstools\\abs-frameworks\\abs-mvc\\src\\abs\\framework\\Utility.abs",158);
                                        if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("C:\\Users\\ls\\Documents\\afifun\\abs-microservices-framework\\abstools\\abs-frameworks\\abs-mvc\\src\\abs\\framework\\Utility.abs",159);
                                        output = abs.backend.java.lib.types.ABSInteger.fromString("5");
                                        if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().getCurrentTask().setLocalVariable("output", output);}
                                }
                                else {
                                    if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("C:\\Users\\ls\\Documents\\afifun\\abs-microservices-framework\\abstools\\abs-frameworks\\abs-mvc\\src\\abs\\framework\\Utility.abs",161);
                                    if (abs.backend.java.lib.expr.BinOp.eq(ch,abs.backend.java.lib.types.ABSString.fromString("6")).toBoolean()) {
                                         {
                                            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("C:\\Users\\ls\\Documents\\afifun\\abs-microservices-framework\\abstools\\abs-frameworks\\abs-mvc\\src\\abs\\framework\\Utility.abs",162);
                                            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("C:\\Users\\ls\\Documents\\afifun\\abs-microservices-framework\\abstools\\abs-frameworks\\abs-mvc\\src\\abs\\framework\\Utility.abs",163);
                                            output = abs.backend.java.lib.types.ABSInteger.fromString("6");
                                            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().getCurrentTask().setLocalVariable("output", output);}
                                    }
                                    else {
                                        if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("C:\\Users\\ls\\Documents\\afifun\\abs-microservices-framework\\abstools\\abs-frameworks\\abs-mvc\\src\\abs\\framework\\Utility.abs",165);
                                        if (abs.backend.java.lib.expr.BinOp.eq(ch,abs.backend.java.lib.types.ABSString.fromString("7")).toBoolean()) {
                                             {
                                                if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("C:\\Users\\ls\\Documents\\afifun\\abs-microservices-framework\\abstools\\abs-frameworks\\abs-mvc\\src\\abs\\framework\\Utility.abs",166);
                                                if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("C:\\Users\\ls\\Documents\\afifun\\abs-microservices-framework\\abstools\\abs-frameworks\\abs-mvc\\src\\abs\\framework\\Utility.abs",167);
                                                output = abs.backend.java.lib.types.ABSInteger.fromString("7");
                                                if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().getCurrentTask().setLocalVariable("output", output);}
                                        }
                                        else {
                                            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("C:\\Users\\ls\\Documents\\afifun\\abs-microservices-framework\\abstools\\abs-frameworks\\abs-mvc\\src\\abs\\framework\\Utility.abs",169);
                                            if (abs.backend.java.lib.expr.BinOp.eq(ch,abs.backend.java.lib.types.ABSString.fromString("8")).toBoolean()) {
                                                 {
                                                    if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("C:\\Users\\ls\\Documents\\afifun\\abs-microservices-framework\\abstools\\abs-frameworks\\abs-mvc\\src\\abs\\framework\\Utility.abs",170);
                                                    if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("C:\\Users\\ls\\Documents\\afifun\\abs-microservices-framework\\abstools\\abs-frameworks\\abs-mvc\\src\\abs\\framework\\Utility.abs",171);
                                                    output = abs.backend.java.lib.types.ABSInteger.fromString("8");
                                                    if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().getCurrentTask().setLocalVariable("output", output);}
                                            }
                                            else {
                                                if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("C:\\Users\\ls\\Documents\\afifun\\abs-microservices-framework\\abstools\\abs-frameworks\\abs-mvc\\src\\abs\\framework\\Utility.abs",173);
                                                if (abs.backend.java.lib.expr.BinOp.eq(ch,abs.backend.java.lib.types.ABSString.fromString("9")).toBoolean()) {
                                                     {
                                                        if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("C:\\Users\\ls\\Documents\\afifun\\abs-microservices-framework\\abstools\\abs-frameworks\\abs-mvc\\src\\abs\\framework\\Utility.abs",174);
                                                        if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("C:\\Users\\ls\\Documents\\afifun\\abs-microservices-framework\\abstools\\abs-frameworks\\abs-mvc\\src\\abs\\framework\\Utility.abs",175);
                                                        output = abs.backend.java.lib.types.ABSInteger.fromString("9");
                                                        if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().getCurrentTask().setLocalVariable("output", output);}
                                                }
                                                else {
                                                     {
                                                        if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("C:\\Users\\ls\\Documents\\afifun\\abs-microservices-framework\\abstools\\abs-frameworks\\abs-mvc\\src\\abs\\framework\\Utility.abs",178);
                                                        if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("C:\\Users\\ls\\Documents\\afifun\\abs-microservices-framework\\abstools\\abs-frameworks\\abs-mvc\\src\\abs\\framework\\Utility.abs",179);
                                                        output = abs.backend.java.lib.types.ABSInteger.fromString("-1");
                                                        if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().getCurrentTask().setLocalVariable("output", output);}
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("C:\\Users\\ls\\Documents\\afifun\\abs-microservices-framework\\abstools\\abs-frameworks\\abs-mvc\\src\\abs\\framework\\Utility.abs",182);
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().getCurrentTask().popStackFrame();
            return output;
        }
    }
    // Utility.abs:0:0: 
    public final abs.backend.java.lib.runtime.ABSFut<abs.backend.java.lib.types.ABSBool> async_isNegativeNumber(abs.backend.java.lib.types.ABSString s) {
        return (abs.backend.java.lib.runtime.ABSFut)abs.backend.java.lib.runtime.ABSRuntime.getCurrentRuntime().asyncCall(new abs.backend.java.lib.runtime.AbstractAsyncCallRT<ABS.Framework.Utility.UtilityImpl_c>(
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
                    return "isNegativeNumber";
                }
                public Object execute() {
                    return target.isNegativeNumber(arg0
                    );
                }
            }.init(s))
        ;
    }
    // Utility.abs:0:0: 
    public final abs.backend.java.lib.types.ABSBool isNegativeNumber(abs.backend.java.lib.types.ABSString s) {
        __ABS_checkSameCOG(); 
        if (__ABS_getRuntime().debuggingEnabled()) {
            abs.backend.java.lib.runtime.Task<?> __ABS_currentTask = __ABS_getRuntime().getCurrentTask();
            __ABS_currentTask.newStackFrame(this, "isNegativeNumber");
            __ABS_currentTask.setLocalVariable("s",s);
        }
         {
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("C:\\Users\\ls\\Documents\\afifun\\abs-microservices-framework\\abstools\\abs-frameworks\\abs-mvc\\src\\abs\\framework\\Utility.abs",0);
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("C:\\Users\\ls\\Documents\\afifun\\abs-microservices-framework\\abstools\\abs-frameworks\\abs-mvc\\src\\abs\\framework\\Utility.abs",187);
            abs.backend.java.lib.types.ABSInteger length = abs.backend.java.lib.runtime.ABSBuiltInFunctions.strlen(s);
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().getCurrentTask().setLocalVariable("length",length);
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("C:\\Users\\ls\\Documents\\afifun\\abs-microservices-framework\\abstools\\abs-frameworks\\abs-mvc\\src\\abs\\framework\\Utility.abs",188);
            abs.backend.java.lib.types.ABSString firstDigit = abs.backend.java.lib.runtime.ABSBuiltInFunctions.substr(s, abs.backend.java.lib.types.ABSInteger.fromString("0"), abs.backend.java.lib.types.ABSInteger.fromString("1"));
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().getCurrentTask().setLocalVariable("firstDigit",firstDigit);
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("C:\\Users\\ls\\Documents\\afifun\\abs-microservices-framework\\abstools\\abs-frameworks\\abs-mvc\\src\\abs\\framework\\Utility.abs",189);
            abs.backend.java.lib.types.ABSBool result = abs.backend.java.lib.types.ABSBool.FALSE;
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().getCurrentTask().setLocalVariable("result",result);
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("C:\\Users\\ls\\Documents\\afifun\\abs-microservices-framework\\abstools\\abs-frameworks\\abs-mvc\\src\\abs\\framework\\Utility.abs",191);
            if (abs.backend.java.lib.expr.BinOp.eq(firstDigit,abs.backend.java.lib.types.ABSString.fromString("-")).toBoolean()) {
                 {
                    if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("C:\\Users\\ls\\Documents\\afifun\\abs-microservices-framework\\abstools\\abs-frameworks\\abs-mvc\\src\\abs\\framework\\Utility.abs",192);
                    if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("C:\\Users\\ls\\Documents\\afifun\\abs-microservices-framework\\abstools\\abs-frameworks\\abs-mvc\\src\\abs\\framework\\Utility.abs",193);
                    result = abs.backend.java.lib.types.ABSBool.TRUE;
                    if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().getCurrentTask().setLocalVariable("result", result);}
            }
            else {
                 {
                    if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("C:\\Users\\ls\\Documents\\afifun\\abs-microservices-framework\\abstools\\abs-frameworks\\abs-mvc\\src\\abs\\framework\\Utility.abs",196);
                    if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("C:\\Users\\ls\\Documents\\afifun\\abs-microservices-framework\\abstools\\abs-frameworks\\abs-mvc\\src\\abs\\framework\\Utility.abs",197);
                    result = abs.backend.java.lib.types.ABSBool.FALSE;
                    if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().getCurrentTask().setLocalVariable("result", result);}
            }
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep("C:\\Users\\ls\\Documents\\afifun\\abs-microservices-framework\\abstools\\abs-frameworks\\abs-mvc\\src\\abs\\framework\\Utility.abs",200);
            if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().getCurrentTask().popStackFrame();
            return result;
        }
    }
}
