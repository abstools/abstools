/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.backend.tests;

import java.io.PrintStream;
import java.io.PrintWriter;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Map;
import java.util.Set;

import abs.backend.prettyprint.DefaultABSFormatter;
import abs.frontend.ast.Access;
import abs.frontend.ast.Annotation;
import abs.frontend.ast.AssignStmt;
import abs.frontend.ast.AsyncCall;
import abs.frontend.ast.Block;
import abs.frontend.ast.ClassDecl;
import abs.frontend.ast.Cog;
import abs.frontend.ast.DataConstructor;
import abs.frontend.ast.DataConstructorExp;
import abs.frontend.ast.DataTypeUse;
import abs.frontend.ast.Decl;
import abs.frontend.ast.Exp;
import abs.frontend.ast.ExpressionStmt;
import abs.frontend.ast.FnApp;
import abs.frontend.ast.FromImport;
import abs.frontend.ast.GetExp;
import abs.frontend.ast.Import;
import abs.frontend.ast.InterfaceDecl;
import abs.frontend.ast.InterfaceTypeUse;
import abs.frontend.ast.List;
import abs.frontend.ast.MainBlock;
import abs.frontend.ast.MethodImpl;
import abs.frontend.ast.MethodSig;
import abs.frontend.ast.Model;
import abs.frontend.ast.ModuleDecl;
import abs.frontend.ast.Name;
import abs.frontend.ast.NamedImport;
import abs.frontend.ast.NewExp;
import abs.frontend.ast.Opt;
import abs.frontend.ast.ParamDecl;
import abs.frontend.ast.ParametricDataTypeDecl;
import abs.frontend.ast.ParametricDataTypeUse;
import abs.frontend.ast.PureExp;
import abs.frontend.ast.StarImport;
import abs.frontend.ast.SyncCall;
import abs.frontend.ast.TypeUse;
import abs.frontend.ast.VarDecl;
import abs.frontend.ast.VarDeclStmt;
import abs.frontend.ast.VarUse;
import abs.frontend.ast.WhileStmt;
import abs.frontend.tests.ABSFormatter;
import abs.frontend.typechecker.Type;

/**
 * The ABSUnit test runner generator
 * 
 * @author pwong
 * 
 */
public class ABSTestRunnerGenerator {
    
    public static final String RUNNER_MAIN = "AbsUnit.TestRunner";
    
    private static final String ignore = "AbsUnit.Ignored";
    private static final String test = "AbsUnit.Test";
    private static final String dataPoint = "AbsUnit.DataPoint";
        
    private static final String suite = "AbsUnit.Suite";
    private static final String fixture = "AbsUnit.Fixture";
    private static final String absStdSet = "ABS.StdLib.Set";
    private static final String absStdLib = "ABS.StdLib";

    private static final String dataValue = "d";
    private static final String futs = "futs";
    private static final String fut = "fut";

    private DataConstructor ignoreType;
    private DataConstructor testType;
    private DataConstructor dataPointType;
    
    private DataConstructor suiteType;
    private DataConstructor fixtureType;

    private Map<InterfaceDecl, Set<ClassDecl>> tests = new HashMap<InterfaceDecl, Set<ClassDecl>>();

    private final Model model;
    
    private boolean isEmpty = true;

    /**
     * The constructor takes a type checked {@link Model} of the ABS model
     * 
     * @param model
     * @throws IllegalArgumentException if model is null
     */
    public ABSTestRunnerGenerator(Model model) {
        if (model == null)
            throw new IllegalArgumentException("Model cannot be null!");
        
        this.model = model;

        gatherABSUnitAnnotations();

        /*
         * Do not search for test class definitions if this model does not
         * contain the necessary ABSUnit annotations
         */
        if (ignoreType == null || testType == null || dataPointType == null || 
            suiteType == null || fixtureType == null) {
            return;
        }

        gatherTestClasses();

        /*
         * An ABSUnit ABS model must have defined at least one interface with
         * TestClass annotation.
         */
        if (tests.isEmpty())
            return;

        /*
         * An ABSUnit ABS model must have defined at least one class with
         * TestClassImpl annotation.
         */
        for (InterfaceDecl inf : tests.keySet()) {
            isEmpty &= tests.get(inf).isEmpty();
        }
    }

    /**
     * Checks if this generator contains a {@link Model} that defines ABSUnit
     * tests.
     * 
     * @return
     */
    public boolean hasUnitTest() {
        return !isEmpty;
    }

    /**
     * Outputs an ABS module that only contains a main block. For each test
     * method defined in the model, this main blo annotated test classes
     * 
     * @param stream
     */
    public void generateTestRunner(PrintStream stream) {
        TestRunnerScriptBuilder imports = generateImports();
        TestRunnerScriptBuilder main = generateMainBlock(imports);
        stream.println("module "+RUNNER_MAIN+";");
        stream.println(imports.toString());
        stream.print("{");
        stream.print(TestRunnerScriptBuilder.NEWLINE);
        stream.print(TestRunnerScriptBuilder.INDENT);
        stream.print(main);
        stream.print(TestRunnerScriptBuilder.NEWLINE);
        stream.print("}");
        stream.print(TestRunnerScriptBuilder.NEWLINE);
    }
    
    public void generateTestRunnerAST(PrintStream stream) {
        ModuleDecl module = new ModuleDecl();
        module.setName(RUNNER_MAIN);
        module.setImportList(generateImportsAST());
        module.setBlock(generateMainBlockAST(module.getImportList()));
        
        ABSFormatter formatter = new DefaultABSFormatter();
        PrintWriter writer = new PrintWriter(stream, true);
        formatter.setPrintWriter(writer);
        module.doPrettyPrint(writer, formatter);
    }

    private String uncap(String word) {
        return new StringBuilder().append(Character.toLowerCase(word.charAt(0))).append(word.substring(1)).toString();
    }

    private void gatherTestClasses() {
        for (Decl decl : this.model.getDecls()) {
            if (decl instanceof ClassDecl) {
                ClassDecl clazz = (ClassDecl) decl;
                if (isTestClassImpl(clazz)) {
                    InterfaceDecl inf = getTestClass(clazz);
                    if (inf != null) {
                        addTest(inf, clazz);
                    }
                    continue;
                }
            }
        }
    }

    private void addTest(InterfaceDecl inf, ClassDecl clazz) {
        if (!tests.containsKey(inf)) {
            tests.put(inf, new HashSet<ClassDecl>());
        }
        tests.get(inf).add(clazz);
    }

    private boolean hasTestAnnotation(List<Annotation> annotations, DataConstructor... constructors) {
        java.util.List<DataConstructor> cs = Arrays.asList(constructors);
        for (Annotation ta : annotations) {
            PureExp exp = ta.getValue();
            if (exp instanceof DataConstructorExp
                    && cs.contains(((DataConstructorExp) exp).getDataConstructor())) {
                return true;
            }
        }
        return false;
    }
    
    private InterfaceDecl getTestClass(ClassDecl clazz) {
        for (InterfaceTypeUse inf : clazz.getImplementedInterfaceUseList()) {
            if (inf.getDecl() instanceof InterfaceDecl) {
                InterfaceDecl decl = (InterfaceDecl) inf.getDecl();
                if (hasTestAnnotation(decl.getAnnotations(), fixtureType) &&
                    ! hasTestAnnotation(decl.getAnnotations(), ignoreType)) {
                    return decl;
                }
            }
        }
        return null;
    }

    private boolean isTestClassImpl(ClassDecl clazz) {
        return hasTestAnnotation(clazz.getAnnotations(), suiteType);
    }

    private void gatherABSUnitAnnotations() {
        for (Decl decl : this.model.getDecls()) {
            if (decl instanceof ParametricDataTypeDecl) {
                String name = decl.getType().getQualifiedName(); 
                if (test.equals(name)) {
                    testType = ((ParametricDataTypeDecl) decl).getDataConstructor(0);
                } else if (fixture.equals(name)) {
                    fixtureType = ((ParametricDataTypeDecl) decl).getDataConstructor(0);
                } else if (suite.equals(name)) {
                    suiteType = ((ParametricDataTypeDecl) decl).getDataConstructor(0);
                } else if (dataPoint.equals(name)) {
                    dataPointType = ((ParametricDataTypeDecl) decl).getDataConstructor(0);
                } else if (ignore.equals(name)) {
                    ignoreType = ((ParametricDataTypeDecl) decl).getDataConstructor(0);
                }
            }
        }
    }

    /**
     * For each test interface and classes, append the corresponding
     * import declaration to a {@link StringBuilder} and return that
     * string builder
     * 
     * @return a reference to that string builder
     */
    private TestRunnerScriptBuilder generateImports() {
        TestRunnerScriptBuilder builder = new TestRunnerScriptBuilder();
        for (InterfaceDecl key : tests.keySet()) {
            builder = generateImport(builder, key);
            for (ClassDecl clazz : tests.get(key)) {
                builder = generateImport(builder, clazz);
            }
        }
        return builder;
    }
    
    private void getImportsFrom(Set<String> mn, Set<String> qn, ModuleDecl m) {
        mn.add(m.getName());
        for (Import i : m.getImportList()) {
            if (i instanceof NamedImport) {
                for (Name n : ((NamedImport) i).getNameList()) {
                    qn.add(n.getName());
                }
            } else if (i instanceof FromImport) {
                mn.add(((FromImport) i).getModuleName());
            } else if (i instanceof StarImport) {
                mn.add(((StarImport) i).getModuleName());
            }
        }
    }
    
    private List<Import> generateImportsAST() {
        List<Import> imports = new List<Import>();
        Set<String> mn = new HashSet<String>();
        Set<String> qn = new HashSet<String>();
        for (InterfaceDecl key : tests.keySet()) {
            getImportsFrom(mn, qn, key.getModule());
//            if (! absStdLib.equals(key.getModule().getName())) {
//                imports.add(generateImportAST(key));
//            }
            for (ClassDecl clazz : tests.get(key)) {
                getImportsFrom(mn, qn, clazz.getModule());
//                if (! absStdLib.equals(clazz.getModule().getName())) {
//                    imports.add(generateImportAST(clazz));
//                }
            }
        }
        
        for (String m : mn) {
            imports.add(new StarImport(m));
        }
        
        if (!qn.isEmpty()) {
            List<Name> names = new List<Name>(); 
            for (String q : qn) {
                names.add(new Name(q));
            }
            imports.add(new NamedImport(names));
        }
        
        return imports;
    }

    /**
     * Append a line of the form "import n from m;" to builder where n is the
     * name of the specified decl and m is the name of the {@link ModuleDecl} of
     * that decl.
     * 
     * @param builder
     * @param decl
     * @return a reference to {@code builder}
     */
    private TestRunnerScriptBuilder generateImport(TestRunnerScriptBuilder builder, Decl decl) {
        return generateImport(builder, decl.getName(), decl.getModuleDecl().getName());
    }
    
    private Import generateImportAST(Decl decl) {
        return generateImportAST(decl.getName(), decl.getModuleDecl().getName());
    }

    private TestRunnerScriptBuilder generateImports(TestRunnerScriptBuilder builder, Set<Type> types) {
        for (Type type : types) {
            generateImport(builder, type.getSimpleName(), type.getModuleName());   
        }
        return builder;
    }
    
    private Set<Import> generateImportsAST(Set<TypeUse> typeUses) {
        Set<Import> imports = new HashSet<Import>();
        for (TypeUse type : typeUses) {
            if (type instanceof DataTypeUse) {
                imports.addAll(generateImportAST((DataTypeUse) type));
            } else {
                imports.add(generateImportAST(type.getName(), type.getModuleDecl().getName()));
            }
        }
        return imports;
    }
    
    private Set<Import> generateImportAST(DataTypeUse t) {
        Set<Import> imports = new HashSet<Import>();
        imports.add(generateImportAST(t.getName(), t.getModuleDecl().getName()));
        if (t instanceof ParametricDataTypeUse) {
            for (DataTypeUse st : ((ParametricDataTypeUse) t).getParams()) {
                imports.addAll(generateImportAST(st));
            }
        }
        return imports;
    }

    private TestRunnerScriptBuilder generateImport(TestRunnerScriptBuilder builder, String name, String module) {
        if (module.equals(absStdLib)) {
            return builder;
        }
        return builder.append("import ").append(name).append(" from ").append(module).append(";").newLine();
    }
    
    private Import generateImportAST(String name, String module) {
        return new FromImport(new List<Name>().add(new Name(name)), module);
    }

    private TestRunnerScriptBuilder generateMainBlock(TestRunnerScriptBuilder imports) {
        TestRunnerScriptBuilder builder = new TestRunnerScriptBuilder();
        builder.increaseIndent();
        Set<Type> paramNames = new HashSet<Type>();
        builder.append("Set<Fut<Unit>> ").append(futs).append(" = EmptySet;").newLine();
        builder.append("Fut<Unit> ").append(fut).append(";").newLine();
        for (InterfaceDecl key : tests.keySet()) {
            builder.append("//Test cases for ").append(key.getType().getQualifiedName()).newLine();
            for (ClassDecl clazz : tests.get(key)) {
                builder.append("//Test cases for implementation ").append(clazz.getName()).newLine();
                paramNames.addAll(generateTestClassImpl(key, clazz, builder));
            }
        }
        generateWaitSync(builder);
        generateImports(imports, paramNames);
        return builder;
    }
    
    private DataTypeUse getType(String n, DataTypeUse... types) {
        if (types.length > 0) {
            ParametricDataTypeUse set = new ParametricDataTypeUse(); 
            set.setName(n);
            for (DataTypeUse d : types) {
                set.addParam(d);
            }
            return set;
        } else {
            DataTypeUse set = new DataTypeUse();
            set.setName(n);
            return set;
        }
        
    }
    private DataTypeUse getFutUnitType() {
        return getType("Fut", getType("Unit"));
    }
    
    private VarDeclStmt getVarDecl(String name, Access a, Exp exp) {
        Opt<Exp> opt = new Opt<Exp>(); 
        if (exp != null) {
            opt.setChild(exp, 0);
        }
        return new VarDeclStmt(new List<Annotation>(),new VarDecl(name,a,opt));
    }
    
    private MainBlock generateMainBlockAST(List<Import> list) {
        final MainBlock block = new MainBlock();
        
        DataConstructorExp empty = new DataConstructorExp("EmptySet",new List<PureExp>());
        VarDeclStmt futsStatement = getVarDecl(futs, getType("Set", getFutUnitType()), empty);
        block.addStmt(futsStatement);
        
        VarDeclStmt futStatement = getVarDecl(fut, getFutUnitType(), null);
        block.addStmt(futStatement);
        
        Set<TypeUse> use = new HashSet<TypeUse>();
        for (InterfaceDecl key : tests.keySet()) {
            for (ClassDecl clazz : tests.get(key)) {
                use.addAll(generateTestClassImplAST(key, clazz, block));
            }
        }
        
        block.addStmt(generateWaitSyncAST());
        //for (Import i : generateImportsAST(use)) {
        //    list.add(i);
        //}
        
        return block;
    }
    
    private TestRunnerScriptBuilder generateWaitSync(TestRunnerScriptBuilder builder) {
        return 
          builder
            .append("//waits for methods return...").newLine()
            .append("while (hasNext(").append(futs).append(")) {").newLine().increaseIndent() // begin while
            .append("Pair<Set<Fut<Unit>>,Fut<Unit>> nt = next(").append(futs).append(");").newLine()
            .append(fut).append(" = snd(nt);").newLine()
            .append(futs).append(" = fst(nt);").newLine()
            .append(fut).append(".get;").newLine()
            .decreaseIndent().append("}").newLine(); // end while
    }
    
    private FnApp getFnApp(String fn, PureExp...exps) {
        List<PureExp> ps = new List<PureExp>();
        for (PureExp p : exps) {
            ps.add(p);
        }
        return new FnApp(fn, ps);
    }
    
    private AssignStmt getVAssign(String v, Exp exp) {
        AssignStmt s = new AssignStmt();
        s.setVar(new VarUse(v));
        s.setValue(exp);
        return s;
    }
    
    private ExpressionStmt getExpStmt(Exp exp) {
        ExpressionStmt ex = new ExpressionStmt();
        ex.setExp(exp);
        return ex;
    }
    
    private WhileStmt generateWaitSyncAST() {
        WhileStmt ws = new WhileStmt();
        ws.setCondition(getFnApp("hasNext",new VarUse(futs)));
        Block body = new Block();
        ws.setBody(body);
        DataTypeUse u = getType("Pair", getType("Set", 
                getType("Fut", getType("Unit"))),getType("Fut", getType("Unit")));
        body.addStmt(getVarDecl("nt", u, getFnApp("next",new VarUse(futs))));
        body.addStmt(getVAssign(fut, getFnApp("snd",new VarUse("nt"))));
        body.addStmt(getVAssign(futs, getFnApp("fst",new VarUse("nt"))));
        body.addStmt(getExpStmt(new GetExp(new VarUse("fut"))));
        return ws;
    }
    
    private Set<Type> generateTestClassImpl(InterfaceDecl inf, ClassDecl clazz, TestRunnerScriptBuilder main) {
        Set<Type> paramNames = new HashSet<Type>();
        Type dataType = generateDataPoints(inf, clazz, paramNames, main);
        String namePrefix = clazz.getName();
        int instance = 0;
        for (MethodSig method : getTestMethods(inf)) {
            boolean needdata = method.getParamList().iterator().hasNext();
            if (needdata) {
                if (dataType == null) {
                    throw new IllegalStateException("Test method requires arguments but test class defines no data point");
                } 
                /*
                 * a while loop over all data points
                 */
                String dataPointSet = dataPointSetName(clazz);
                main.append("while (hasNext(").append(dataPointSet).append(")) {").newLine().increaseIndent(); // begin while
                main.append("Pair<Set<").append(dataType).append(">,").append(dataType)
                    .append("> nt = next(").append(dataPointSet).append(");").newLine();
                main.append(dataType).append(" ").append(dataValue).append(" = snd(nt);").newLine();
                main.append(dataPointSet).append(" = fst(nt);").newLine();
            }
            
            /*
             * Add those methods that are not ignored
             */
            if (! isIgnored(clazz,method)) {
                main.append("//Test cases for method ").append(method.getName()).newLine();
                String objectRef = uncap(namePrefix) + instance;
                main = newCog(main, inf, clazz, objectRef);
                generateAsyncTestCall(main, objectRef, method);
            }
            
            if (needdata) {
                main.decreaseIndent().append("}").newLine(); // end while
            }
            instance++;
        }

        return paramNames;
    }
    
    private Set<TypeUse> generateTestClassImplAST(
            InterfaceDecl inf, ClassDecl clazz, MainBlock block) {
        Set<TypeUse> accesses = new HashSet<TypeUse>();
        DataTypeUse dataType = generateDataPointsAST(inf, clazz, accesses, block);
        
        String namePrefix = clazz.getName();
        int instance = 0;
        for (MethodSig method : getTestMethods(inf)) {
            Block thisBlock = block;
            WhileStmt ws = null;
            
            if (method.getNumParam() > 0) {
                if (dataType == null) {
                    throw new IllegalStateException("Test method requires arguments but test class defines no data point");
                } 
                /*
                 * a while loop over all data points
                 */
                String dataPointSet = dataPointSetName(clazz);
                ws = new WhileStmt();
                ws.setCondition(getFnApp("hasNext",new VarUse(dataPointSet)));
                Block body = new Block();
                ws.setBody(body);
                thisBlock = body;
                DataTypeUse u = getType("Pair", getType("Set", dataType.copy()), dataType.copy()); 
                thisBlock.addStmt(getVarDecl("nt", u, getFnApp("next",new VarUse(dataPointSet))));
                thisBlock.addStmt(getVarDecl(dataValue, dataType.copy(), getFnApp("snd",new VarUse("nt"))));
                thisBlock.addStmt(getVAssign(dataPointSet, getFnApp("fst",new VarUse("nt"))));
            }
            
            /*
             * Add those methods that are not ignored
             */
            if (! isIgnored(clazz,method)) {
                String objectRef = uncap(namePrefix) + instance;
                thisBlock.addStmt(newObj(inf, clazz, objectRef, true));
                generateAsyncTestCallAST(thisBlock, objectRef, method);
            }
            
            if (ws != null) {
               block.addStmt(ws);
            }
            instance++;
        }

        return accesses;
    }
    
    private boolean isIgnored(ClassDecl clazz, MethodSig method) {
        for (MethodImpl m : clazz.getMethodList()) {
            if (m.getMethodSig().getName().equals(method.getName())) {
                return hasTestAnnotation(m.getMethodSig().getAnnotationList(), ignoreType); 
            }
        }
        return false;
    }

    /**
     * Generates data points for test class {@code clazz}
     * 
     * @param inf
     * @param clazz
     * @param paramNames
     * @param main
     * @return The data type the set return type is parametric on, null if
     *         {@code inf} does not define a data point method or {@code clazz}
     *         does not implement such method.
     */
    private Type generateDataPoints(InterfaceDecl inf, ClassDecl clazz, Set<Type> paramNames, TestRunnerScriptBuilder main) {

        MethodSig dataPoint = findDataPoints(inf);
        if (dataPoint == null) {
            return null;
        }

        /*
         * It must be a Set of data
         */
        Type data = ((ParametricDataTypeUse) dataPoint.getReturnType()).getParams().iterator().next().getType();

        /*
         * make sure the return type can be resolved TODO this needs to be
         * resolved recursively
         */
        paramNames.add(data);

        /*
         * create an object in the same cog as main for retrieving the data
         * points
         */
        String objName = uncap(clazz.getName()) + "dataPoint";
        String dataSet = dataPointSetName(clazz);
        newObj(main, inf, clazz, objName, false);

        main.append(dataPoint.getReturnType()).append(" ").append(dataSet).append(" = ").append(objName).append(".")
                .append(dataPoint.getName()).append("();").newLine();

        return data;
    }
    
    private DataTypeUse generateDataPointsAST(InterfaceDecl key, ClassDecl clazz, 
            Set<TypeUse> use, MainBlock block) {
        MethodSig dataPoint = findDataPoints(key);
        if (dataPoint == null) {
            return null;
        }
        
        Access rt = dataPoint.getReturnType();
        if (!(rt instanceof ParametricDataTypeUse)) {
            return null;
        }
        
        ParametricDataTypeUse prt = (ParametricDataTypeUse) rt;
        if (! prt.getName().equals("Set")) {
            return null;
        }
        
        //Set has only one type parameter
        DataTypeUse u = prt.getParam(0).copy();
        use.add(u);
        
        String objName = uncap(clazz.getName()) + "dataPoint";
        String dataSet = dataPointSetName(clazz);
        block.addStmt(newObj(key, clazz, objName, false));
        block.addStmt(getVarDecl(dataSet, prt.copy(), 
             new SyncCall(new VarUse(objName), dataPoint.getName(), new List<PureExp>())));
        
        return u;
    }

    


    private String dataPointSetName(ClassDecl clazz) {
        return uncap(clazz.getName()) + "dataPointSet";
    }

    /**
     * Find a method defined in {@code inf} that is annotated with [DataPoint],
     * takes no argument and returns a Set of data values.
     * 
     * @param inf
     * @return the method defined in {@code inf} that is annotated with
     *         [DataPoint], or null if such a method does not exist.
     */
    private MethodSig findDataPoints(InterfaceDecl inf) {
        for (MethodSig meth : inf.getAllMethodSigs()) {
            if (hasTestAnnotation(meth.getAnnotations(), dataPointType)) {
                Decl d = ((ParametricDataTypeUse) meth.getReturnType()).getDecl();
                if (d.getType().getQualifiedName().equals(absStdSet)) {
                    return meth;
                }
                return null;
            }
        }
        return null;
    }

    /**
     * Write the line of the form {@code objectRef!method(d);}, where d is
     * {@link #dataValue} to {@code builder}, if {@code method} takes an
     * argument, otherwise it write the line of the form
     * {@code objectRef!method();} to {@code builder}.
     * 
     * @param main
     * @param objectRef
     * @param method
     */
    private void generateAsyncTestCall(TestRunnerScriptBuilder main, String objectRef, MethodSig method) {
        main.append(fut).append(" = ").append(objectRef).append("!").append(method.getName());
        Iterator<ParamDecl> paramIts = method.getParamList().iterator();
        if (paramIts.hasNext()) {
            main.append("(").append(dataValue).append(");").newLine(); // add
                                                                  // parameter
                                                                  // values
        } else {
            main.append("();").newLine(); // no parameter
        }
        main.append(futs).append("= Insert(").append(fut).append(",").append(futs).append(");").newLine();
    }
    
    private void generateAsyncTestCallAST(Block block, String objectRef, MethodSig method) {
        List<PureExp> args = new List<PureExp>();
        if (method.getNumParam() > 0) {
            args.add(new VarUse(dataValue));
        }
        block.addStmt(getVAssign(fut, new AsyncCall(new VarUse(objectRef), method.getName(), args)));
        block.addStmt(getVAssign(futs, getFnApp("Insert", new VarUse(fut), new VarUse(futs))));
    }

    /**
     * Write the line of the form {@code inf name = new cog clazz();} to
     * {@code builder}
     * 
     * @param main
     * @param inf
     * @param clazz
     * @param name
     * @return a reference to builder.
     */
    private TestRunnerScriptBuilder newCog(TestRunnerScriptBuilder main, InterfaceDecl inf, ClassDecl clazz, String name) {
        return newObj(main, inf, clazz, name, true);
    }

    /**
     * If {@code cog} is true, this method writes the line of the form
     * {@code inf name = new cog clazz();} to {@code builder}, otherwise it
     * writes the line of the form {@code inf name = new clazz();} to
     * {@code builder}.
     * 
     * @param main
     * @param inf
     * @param clazz
     * @param name
     * @param cog
     * @return a reference to builder.
     */
    private TestRunnerScriptBuilder newObj(TestRunnerScriptBuilder main, InterfaceDecl inf, ClassDecl clazz, String name, boolean cog) {
        return main.append(inf.getName()).append(" ").append(name).append((cog) ? " = new cog " : " = new ")
                .append(clazz.getName()).append("();").newLine();
    }
    
    private VarDeclStmt newObj(InterfaceDecl inf, ClassDecl clazz, String name, boolean cog) {
        NewExp ne = new NewExp();
        ne.setClassName(clazz.getName());
        if (cog) {
            ne.setCog(new Cog());
        }
        return getVarDecl(name, new InterfaceTypeUse(inf.getName()), ne);
    }

    private Set<MethodSig> getTestMethods(InterfaceDecl inf) {
        Set<MethodSig> testmethods = new HashSet<MethodSig>();
        for (MethodSig meth : inf.getAllMethodSigs()) {
            /*
             * Add those methods that are tests but are not ignored
             */
            if (hasTestAnnotation(meth.getAnnotations(), testType) && 
                ! hasTestAnnotation(meth.getAnnotations(), ignoreType)) {
                testmethods.add(meth);
            }
        }
        return testmethods;
    }

}
