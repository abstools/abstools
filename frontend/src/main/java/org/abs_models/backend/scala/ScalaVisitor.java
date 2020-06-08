/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved.
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.abs_models.backend.scala;

import org.abs_models.backend.tests.AbsASTBuilderUtil;
import org.abs_models.common.CompilerUtils;
import org.abs_models.common.ListUtils;
import org.abs_models.frontend.analyser.AnnotationHelper;
import org.abs_models.frontend.ast.Access;
import org.abs_models.frontend.ast.AddAddExp;
import org.abs_models.frontend.ast.AndBoolExp;
import org.abs_models.frontend.ast.AndGuard;
import org.abs_models.frontend.ast.AsExp;
import org.abs_models.frontend.ast.AssertStmt;
import org.abs_models.frontend.ast.AssignStmt;
import org.abs_models.frontend.ast.AsyncCall;
import org.abs_models.frontend.ast.AwaitAsyncCall;
import org.abs_models.frontend.ast.AwaitStmt;
import org.abs_models.frontend.ast.Block;
import org.abs_models.frontend.ast.BuiltinFunctionDef;
import org.abs_models.frontend.ast.CaseBranch;
import org.abs_models.frontend.ast.CaseBranchStmt;
import org.abs_models.frontend.ast.CaseExp;
import org.abs_models.frontend.ast.CaseStmt;
import org.abs_models.frontend.ast.ClaimGuard;
import org.abs_models.frontend.ast.ClassDecl;
import org.abs_models.frontend.ast.ConstructorArg;
import org.abs_models.frontend.ast.ConstructorPattern;
import org.abs_models.frontend.ast.DataConstructor;
import org.abs_models.frontend.ast.DataConstructorExp;
import org.abs_models.frontend.ast.DataTypeDecl;
import org.abs_models.frontend.ast.DataTypeUse;
import org.abs_models.frontend.ast.Decl;
import org.abs_models.frontend.ast.DieStmt;
import org.abs_models.frontend.ast.DivMultExp;
import org.abs_models.frontend.ast.DurationGuard;
import org.abs_models.frontend.ast.DurationStmt;
import org.abs_models.frontend.ast.EffExp;
import org.abs_models.frontend.ast.EqExp;
import org.abs_models.frontend.ast.ExceptionDecl;
import org.abs_models.frontend.ast.Exp;
import org.abs_models.frontend.ast.ExpFunctionDef;
import org.abs_models.frontend.ast.ExpGuard;
import org.abs_models.frontend.ast.ExpressionStmt;
import org.abs_models.frontend.ast.FieldDecl;
import org.abs_models.frontend.ast.FieldUse;
import org.abs_models.frontend.ast.FloatLiteral;
import org.abs_models.frontend.ast.FnApp;
import org.abs_models.frontend.ast.ForeachStmt;
import org.abs_models.frontend.ast.FromImport;
import org.abs_models.frontend.ast.FunctionDecl;
import org.abs_models.frontend.ast.FunctionDef;
import org.abs_models.frontend.ast.GTEQExp;
import org.abs_models.frontend.ast.GTExp;
import org.abs_models.frontend.ast.GetExp;
import org.abs_models.frontend.ast.IfExp;
import org.abs_models.frontend.ast.IfStmt;
import org.abs_models.frontend.ast.Import;
import org.abs_models.frontend.ast.ImplementsExp;
import org.abs_models.frontend.ast.InitBlock;
import org.abs_models.frontend.ast.IntLiteral;
import org.abs_models.frontend.ast.InterfaceDecl;
import org.abs_models.frontend.ast.InterfaceTypeUse;
import org.abs_models.frontend.ast.LTEQExp;
import org.abs_models.frontend.ast.LTExp;
import org.abs_models.frontend.ast.LetExp;
import org.abs_models.frontend.ast.ListLiteral;
import org.abs_models.frontend.ast.LiteralPattern;
import org.abs_models.frontend.ast.MethodImpl;
import org.abs_models.frontend.ast.MethodSig;
import org.abs_models.frontend.ast.MinusExp;
import org.abs_models.frontend.ast.ModMultExp;
import org.abs_models.frontend.ast.Model;
import org.abs_models.frontend.ast.ModuleDecl;
import org.abs_models.frontend.ast.MoveCogToStmt;
import org.abs_models.frontend.ast.MultMultExp;
import org.abs_models.frontend.ast.Name;
import org.abs_models.frontend.ast.NamedImport;
import org.abs_models.frontend.ast.NegExp;
import org.abs_models.frontend.ast.NewExp;
import org.abs_models.frontend.ast.NotEqExp;
import org.abs_models.frontend.ast.NullExp;
import org.abs_models.frontend.ast.OrBoolExp;
import org.abs_models.frontend.ast.OriginalCall;
import org.abs_models.frontend.ast.ParFnApp;
import org.abs_models.frontend.ast.ParamDecl;
import org.abs_models.frontend.ast.ParametricDataTypeDecl;
import org.abs_models.frontend.ast.ParametricDataTypeUse;
import org.abs_models.frontend.ast.ParametricFunctionDecl;
import org.abs_models.frontend.ast.PartialFunctionDecl;
import org.abs_models.frontend.ast.Pattern;
import org.abs_models.frontend.ast.PatternVar;
import org.abs_models.frontend.ast.PatternVarUse;
import org.abs_models.frontend.ast.PureExp;
import org.abs_models.frontend.ast.ReturnStmt;
import org.abs_models.frontend.ast.SkipStmt;
import org.abs_models.frontend.ast.StarImport;
import org.abs_models.frontend.ast.Stmt;
import org.abs_models.frontend.ast.StringLiteral;
import org.abs_models.frontend.ast.SubAddExp;
import org.abs_models.frontend.ast.SuspendStmt;
import org.abs_models.frontend.ast.SyncCall;
import org.abs_models.frontend.ast.ThisExp;
import org.abs_models.frontend.ast.ThrowStmt;
import org.abs_models.frontend.ast.TraitDecl;
import org.abs_models.frontend.ast.TryCatchFinallyStmt;
import org.abs_models.frontend.ast.TypeParameterDecl;
import org.abs_models.frontend.ast.TypeParameterUse;
import org.abs_models.frontend.ast.TypeSynDecl;
import org.abs_models.frontend.ast.TypeUse;
import org.abs_models.frontend.ast.UnderscorePattern;
import org.abs_models.frontend.ast.UnknownDecl;
import org.abs_models.frontend.ast.UnresolvedTypeUse;
import org.abs_models.frontend.ast.VarDecl;
import org.abs_models.frontend.ast.VarDeclStmt;
import org.abs_models.frontend.ast.VarUse;
import org.abs_models.frontend.ast.WhileStmt;
import org.abs_models.frontend.typechecker.BoundedType;
import org.abs_models.frontend.typechecker.DataTypeType;
import org.abs_models.frontend.typechecker.InterfaceType;
import org.abs_models.frontend.typechecker.Type;
import com.google.common.base.StandardSystemProperty;
import com.google.common.base.Supplier;
import com.google.common.collect.Multimap;
import com.google.common.collect.Multimaps;
import com.google.common.collect.Sets;

import javax.lang.model.element.ElementKind;
import javax.lang.model.element.Modifier;
import java.io.IOException;
import java.io.StringWriter;
import java.nio.file.Path;
import java.util.*;
import java.util.concurrent.Callable;
import java.util.concurrent.atomic.AtomicLong;
import java.util.concurrent.locks.Lock;
import java.util.function.Function;
import java.util.logging.Logger;

//import ScalaVisitor.AbsElementType;

public class ScalaVisitor {
    public static final String CONSTRUCTOR = "constructor";
    private static final String GET_SPAWN = "getSpawn";
    private static final String CONS_FUT = "constructorFuture";
    public static final String GET_CONSTRUCTOR_FUTURE = "getConstructorFuture";
    public static final String INSTANCE_OF = "asInstanceOf";
    public static final String ADD_DC = "addDC";
    public static final String TAS = "TimedActorSystem";
    public static final String ACQUIRE_RESOURCE = "acquireResource";
    public static final String THIS_DC = "thisDC";
    public static Boolean withDC = false;
    public static final String DEPLOYMENT_COMPONENT = "DeploymentComponent";
    public static final String MOVE_TO_COG = "moveToCOG";
    public static final String SET_DC = "setDC";
    public static final String CLASS_DC = "ClassDeploymentComponent";
    private boolean fromConstructor = false;
    private int firstDCPass = 0;


    private boolean hasRun = false;


    private static MethodImpl ACQUIRE = null;
    private static FunctionDecl BUILTIN_THIS_DC = null;

    static enum AbsElementType {
        /**
         * Equivalent to Java's {@link ElementKind#INTERFACE}
         */
        INTERFACE,

        /**
         * Same as Java's {@link ElementKind#CLASS}
         */
        CLASS,

        /**
         * An abstract data type declaration.
         */
        DATA,

        /**
         * Equivalent of a set of Java's <code>static</code> functions.
         */
        FUNCTION,

        /**
         * An abstract data type declaration.
         */
        TYPE,

        /**
         * An exception declaration.
         */
        EXCEPTION,;
    }

    // Constants

    private static final String DATA_DECL_INSTANCE_NAME = "INSTANCE";
    private static final char CHAR_UNDERSCORE = '_';
    private static final char CHAR_DOT = '.';
    private static final String VOID_WRAPPER_CLASS_NAME = "Void";
    private static final String VOID_PRIMITIVE_NAME = "void";
    private static final String LITERAL_THIS = "this";
    private static final String LITERAL_NULL = "null";
    private static final String FUNCTIONS_CLASS_NAME = "Functions";
    private static final String MAIN_CLASS_NAME = "Main";
    private static final String COMMA_SPACE = ", ";
    private static final String METHOD_GET = "geT";
    private static final String SYNC_GET = "getCompleted()"; // this is a shortcut
    private static final String GET_DC = "getDc()";
    // to optimize those
    // calls that are
    // synchronous and
    // complete (these
    // calls DO NOT
    // contain an await or
    // a chain of calls
    // that have an
    // await).
    private static final String EXTERN = "JavaExternClass";
    private static final String STATIC = "JavaStaticClass";
    private static final String ACTOR_SERVER_MEMBER = "me";
    private static final String OF = "Of";
    private static final String ALL_IMPORTS = "._";

    private static final String NEW_LINE = StandardSystemProperty.LINE_SEPARATOR.value();
    private static final String ABS_API_ACTOR_CLASS = "LocalActor";
    private static final String ABS_API_INTERFACE_CLASS = "Actor";
    private static final String ABS_API = "abs.api.cwi";

    private static final String ABS_STDLIB = "ABS.StdLib";

    static final String ABSFUTURE_CLASS = "ABSFuture";

    private static final String ORDERED_INTERFACE_CLASS = "Ordered";

    // private static final String CASE = "_case";

    // private static final String ABS_API_ACTOR_SERVER_CLASS =
    // ActorServer.class.getName();
    private static final Set<Modifier> DEFAULT_MODIFIERS = new HashSet<>();
    private static final String[] DEFAULT_IMPORTS = new String[]{ABS_API + ALL_IMPORTS,
            ABS_API + "." + FUNCTIONS_CLASS_NAME + ALL_IMPORTS,
            Function.class.getPackage().getName() + ALL_IMPORTS, Callable.class.getPackage().getName() + ALL_IMPORTS,
            AtomicLong.class.getPackage().getName() + ALL_IMPORTS, Lock.class.getPackage().getName() + ALL_IMPORTS,
            "abs.api.realtime" + ALL_IMPORTS,
            "abs.api" + ALL_IMPORTS,
            "ABS.DC" + ALL_IMPORTS,
            // LocalActor.class.getPackage().getName() + ALL_IMPORTS,
            // "absstdlib.Functions" + ALL_IMPORTS, "absstdlib" + ALL_IMPORTS,
            // "scala.collection.mutable.Set",
            // "scala.collection.mutable.TreeSet",
            // "scala.collection.mutable.Map",
            // "scala.collection.mutable.HashMap",
            Objects.class.getName(),
            // List.class.getName(),
            // LinkedList.class.getName(),
            // CloudProvider.class.getPackage().getName() + ".*",
            // DeploymentComponent.class.getPackage().getName() + ".*",
    };
    // private static String LIBRARY_IMPORT = "";
    // private static final String[] DEFAULT_IMPORTS_PATTERNS = new String[] {
    // "com.leacox.motif.function.*", "com.leacox.motif.matching.*",
    // "com.leacox.motif.cases.*", "com.leacox.motif.caseclass.*" };
    private static final String[] DEFAULT_STATIC_IMPORTS = new String[]{
            // Functional.class.getPackage().getName() + "." +
            // Functional.class.getSimpleName() + ALL_IMPORTS,
            // CloudProvider.class.getPackage().getName() + "." +
            // CloudProvider.class.getSimpleName()
            // + ".*",
            // DeploymentComponent.class.getPackage().getName() + "."
            // + DeploymentComponent.class.getSimpleName() + ".*"
    };

    /*
     * private static final String[] DEFAULT_STATIC_IMPORTS_PATTERNS = new
     * String[] { "com.leacox.motif.Motif.*",
     * "com.leacox.motif.cases.ListConsCases.*",
     * "com.leacox.motif.cases.Case1Cases.*",
     * "com.leacox.motif.cases.Case2Cases.*",
     * "com.leacox.motif.cases.Case3Cases.*", "com.leacox.motif.MatchesAny.*",
     * "com.leacox.motif.hamcrest.CaseThatCases.*",
     * "com.leacox.motif.MatchesExact.eq", "org.hamcrest.CoreMatchers.*" };
     */
    // Internal Fields

    private static final Logger LOGGER = Logger.getLogger(ScalaVisitor.class.getName());
    private static final Random RANDOM = new Random(System.currentTimeMillis());

    private final Set<String> moduleNames;
    private final Model prog;
    private final JavaWriterSupplier javaWriterSupplier;
    private final String packageName;
    private final ScalaTypeTranslator javaTypeTranslator;
    private final Path outputDirectory;
    private final String[] ignoreCodeGen = new String[]{"Rat", "Bool", "Int", "Fut", "True", "False", "Unit", "Float",
            "String", "Exception", "numerator", "denominator", "truncate", "random", "substr", "log", "exp", "sqrt", "float", "rat", "floor", "ceil", "strlen", "toString",
            "print", "println", "readln", "currentms", "watch", "watchEx", "lowlevelDeadline"};// ,
    // "currentms"
    // };
    private final Set<String> builtinSet = new HashSet<>(Arrays.asList(ignoreCodeGen));
    private static final Map<String, String> dataCollisions = new HashMap<>();
    private static final Map<String, String> interfaceCollisions = new HashMap<>();
    private static final Map<String, String> classCollisions = new HashMap<>();


    // Internal state
    private final Multimap<String, MethodDefinition> methods = Multimaps.newSetMultimap(new HashMap<>(),
            new Supplier<Set<MethodDefinition>>() {
                @Override
                public Set<MethodDefinition> get() {
                    return new HashSet<>();
                }
            });
    private final Multimap<String, VarDefinition> variables = Multimaps.newSetMultimap(new HashMap<>(),
            new Supplier<Set<VarDefinition>>() {
                @Override
                public Set<VarDefinition> get() {
                    return new HashSet<>();
                }
            });

    private LinkedList<TreeSet<VarDefinition>> variablesInScope = new LinkedList<>();
    private final HashSet<VarDefinition> variablesBeforeBlock = new HashSet<>();

    public static final LinkedHashMap<String, MethodDefinition> programMethods = new LinkedHashMap<>();

    // private final HashMap<String, List<String>> paramConstructs = new
    // HashMap<>();
    // private final HashMap<String, List<String>> caseTypeConstructs = new
    // HashMap<>();
    // private final HashMap<String, List<String>> polyTypeConstructs = new
    // HashMap<>();

    // private String patternParamType = "";
    // private String caseKey = "";
    // private boolean fromCase = false;
    private boolean fromSupplier = false;
    private boolean fromEquals = false;
    private boolean fromInit = false;

    // private boolean avoidDeadCode = false;
    private String initType = "";

    private final Stack<ModuleDecl> modules = new Stack<>();
    private final Stack<String> classes = new Stack<>();
    private final EnumMap<AbsElementType, List<Decl>> elements = new EnumMap<>(AbsElementType.class);
    private final Map<String, String> classNames = new HashMap<>();
    private final Set<String> packageLevelImports = new HashSet<>();
    private final Map<String, String> dataDeclarations = new HashMap<>();

    private final Set<String> exceptionDeclaraions = new HashSet<>();
    private final Set<String> staticImports = new HashSet<>();
    private final Set<String> adtInstances = new HashSet<>();

    private final Map<String, LinkedList<StringWriter>> labelMap = new HashMap<>();

    private final List<StringWriter> currentMethodLabels = new LinkedList<>();

    private final Map<String, String> caseMap = new HashMap<>();

    private MethodDefinition currentMethod = null;

    public boolean awaitsDetected = false;

    private int awaitCounter = 0;
    private int syncPCounter = 0;
    private int asyncPCounter = 0;
    private ScalaWriter functionsWriter;

    /**
     * Ctor.
     *
     * @param packageName        the package spec of the program
     * @param prog               the parsed {@link Model} AST node
     * @param javaWriterSupplier the {@link JavaWriterSupplier} for each top-level element
     * @param javaTypeTranslator The ABS to Java type translator
     * @param outputDirectory
     */
    public ScalaVisitor(String packageName, Model prog, JavaWriterSupplier javaWriterSupplier,
                        ScalaTypeTranslator javaTypeTranslator, Path outputDirectory) {
        this.packageName = packageName;
        this.prog = prog;
        this.javaWriterSupplier = javaWriterSupplier;
        this.javaTypeTranslator = javaTypeTranslator;
        this.outputDirectory = outputDirectory;
        this.moduleNames = new HashSet<>();

        // List<String> justT = new ArrayList<>();
        // justT.add("A");
        // paramConstructs.put("Just", justT);
        // List<String> consT = new ArrayList<>(justT);
        // consT.add("List[A]");
        // paramConstructs.put("Cons", consT);
        // List<String> insT = new ArrayList<>(justT);
        // insT.add("Set[A]");
        // paramConstructs.put("Insert", insT);
        //
        // List<String> leftT = new ArrayList<>(justT);
        // leftT.add("B");
        // paramConstructs.put("Left", leftT);
        // List<String> rightT = new ArrayList<>(leftT);
        // paramConstructs.put("Right", rightT);
        // List<String> pairT = new ArrayList<>(leftT);
        // paramConstructs.put("APair", pairT);
        // List<String> iaT = new ArrayList<>();
        // iaT.add("Pair[A,B]");
        // iaT.add("Map[A,B]");
        // paramConstructs.put("InsertAsscoc", iaT);
        //
        // List<String> tripleT = new ArrayList<>(leftT);
        // tripleT.add("C");
        // paramConstructs.put("Triple", tripleT);
        //
        // List<String> maybe = new ArrayList<>();
        // maybe.add("Just");
        // caseTypeConstructs.put("Maybe", maybe);
        //
        // List<String> cons = new ArrayList<>();
        // cons.add("Cons");
        // caseTypeConstructs.put("List", cons);
        // caseTypeConstructs.put("LinkedList", cons);
        //
        // List<String> ts = new ArrayList<>();
        // ts.add("Insert");
        // caseTypeConstructs.put("TreeSet", ts);
        // caseTypeConstructs.put("Set", ts);
        //
        // List<String> ia = new ArrayList<>();
        // ia.add("InsertAssoc");
        // caseTypeConstructs.put("HashMap", ia);
        // caseTypeConstructs.put("Map", ia);
        //
        // List<String> pair = new ArrayList<>();
        // pair.add("APair");
        // caseTypeConstructs.put("Pair", pair);
        //
        // List<String> triple = new ArrayList<>();
        // triple.add("ATriple");
        // caseTypeConstructs.put("Triple", triple);
        //
        // List<String> either = new ArrayList<>();
        // either.add("Left");
        // either.add("Right");
        // caseTypeConstructs.put("Either", either);
        //
        // System.out.println(caseTypeConstructs);
        // System.out.println(paramConstructs);

    }

    public void visit(Model p, ScalaWriter w) {
        // WARNING: `w` should NOT be used in this method;
        // otherwise, I/O issues occur during generation.
        Model program = (Model) p;
        do {
            awaitsDetected = false;
            ScalaWriter notNeeded = new ScalaWriter(true, new StringWriter(), true);
            System.out.println("Checking awaits ");
            for (ModuleDecl module : program.getModuleDecls()) {
                moduleNames.add(module.getName());
                modules.push(module);
                visit(module, notNeeded);
                modules.pop();
            }
            System.out.println("DONE Checking awaits ");

            // System.out.println(programMethods);
        } while (awaitsDetected);
        for (ModuleDecl module : program.getModuleDecls()) {
            modules.push(module);
            visit(module, w);
            modules.pop();
        }


    }

    public void visit(ModuleDecl m, ScalaWriter w) {
        buildProgramDeclarationTypes(m);

        //preVisit(m);
        //System.out.println(programMethods);
        // System.out.println(elements);

        moduleNames.add(m.getName());
        modules.push(m);
        try {

            functionsWriter = (ScalaWriter) javaWriterSupplier.apply(FUNCTIONS_CLASS_NAME);
            functionsWriter.emitPackage(packageName);
            // jw.emitStaticImports(DEFAULT_STATIC_IMPORTS_PATTERNS);
            functionsWriter.emitEmptyLine();
            // functionsWriter.emitImports(DEFAULT_IMPORTS);
            visitImports(m.getImports(), functionsWriter);
            // jw.emitImports(DEFAULT_IMPORTS_PATTERNS);
            emitPackageLevelImport(functionsWriter);
            functionsWriter.emitEmptyLine();

            beginElementKind(functionsWriter, ElementKind.CONSTRUCTOR, FUNCTIONS_CLASS_NAME, DEFAULT_MODIFIERS, null,
                    null, null, false);
            functionsWriter.emitEmptyLine();

            // Type
            for (Decl ad : elements.get(AbsElementType.TYPE)) {
                Decl decl = (Decl) ad;
                ad.accept(this, w);
            }

            // Data
            for (Decl ad : elements.get(AbsElementType.DATA)) {
                Decl decl = (Decl) ad;
                String name = decl.getName();
                ScalaWriter declWriter = (ScalaWriter) javaWriterSupplier.apply(name);
                declWriter.emitPackage(packageName);
                visitImports(m.getImports(), declWriter);
                emitPackageLevelImport(declWriter);
                decl.accept(this, declWriter);
                close(declWriter, w);

                // JavaWriter declWriter =
                // javaWriterSupplier.apply(name);
                // declWriter.emitPackage(packageName);
                // visitImports(m.listimport_, declWriter);
                // decl.accept(this, declWriter);
                // close(declWriter, w);
            }

            // Exception
            for (Decl ad : elements.get(AbsElementType.EXCEPTION)) {
                Decl decl = (Decl) ad;
                String name = decl.getName();
                ScalaWriter declWriter = (ScalaWriter) javaWriterSupplier.apply(name);
                declWriter.emitPackage(packageName);
                visitImports(m.getImports(), declWriter);
                decl.accept(this, declWriter);
                close(declWriter, w);
            }

            // Interfaces
            for (Decl ad : elements.get(AbsElementType.INTERFACE)) {
                if (w.checkAwaits) {
                    Decl decl = (Decl) ad;
                    decl.accept(this, w);

                } else {
                    Decl decl = (Decl) ad;
                    String name = decl.getName();
                    ScalaWriter declWriter = (ScalaWriter) javaWriterSupplier.apply(name);
                    declWriter.emitPackage(packageName);
                    visitImports(m.getImports(), declWriter);
                    emitPackageLevelImport(declWriter);
                    decl.accept(this, declWriter);
                    close(declWriter, w);
                }
            }

            // Classes
            for (Decl ad : elements.get(AbsElementType.CLASS)) {

                // System.out.println(caseTypeConstructs);
                // System.out.println(paramConstructs);
                Decl decl = (Decl) ad;
                String name = dataCollisions.containsKey(decl.getName()) ? dataCollisions.get(decl.getName())
                        : decl.getName();
                ScalaWriter declWriter = (ScalaWriter) javaWriterSupplier.apply(name);
                declWriter.emitPackage(packageName);
                visitImports(m.getImports(), declWriter);
                emitPackageLevelImport(declWriter);
                decl.accept(this, declWriter);
                close(declWriter, w);

            }

            visitFunctions(m, w);

            close(functionsWriter, w);
            if (m.hasBlock())
                visitMain(m, w);

            //System.out.println("Data Collisions " + dataCollisions);

        } catch (IOException e) {
            throw new RuntimeException(e);
        }
        modules.pop();
    }

    public void preVisit(ModuleDecl m) {
        buildProgramDeclarationTypes(m);
        firstDCPass = 0;
        do {
            awaitsDetected = false;
            ScalaWriter notNeeded = new ScalaWriter(true, new StringWriter(), true);
            System.out.println("Checking awaits ");
            moduleNames.add(m.getName());
            modules.push(m);
            for (Decl ad : elements.get(AbsElementType.CLASS)) {
                ad.accept(this, notNeeded);
            }

            for (Decl ad : elements.get(AbsElementType.TYPE)) {
                Decl decl = (Decl) ad;
                ad.accept(this, notNeeded);
            }

            modules.pop();
            System.out.println("DONE Checking awaits ");
            firstDCPass++;

        } while (awaitsDetected);

    }

    public void visit(NamedImport p, ScalaWriter w) {
        Collection<String> types = new HashSet<>();
        for (Name qa : p.getNames()) {
            types.add(qa.getName());
        }
        try {
            w.emitImports(types);
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }

    public void visit(StarImport sfi, ScalaWriter w) {

        String type = sfi.getModuleName();
        try {
            w.emitImports(type + ALL_IMPORTS);
            w.emitImports(type + "." + FUNCTIONS_CLASS_NAME + ALL_IMPORTS);
        } catch (IOException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }
        // this.staticImports.add(this.packageName + "." + type + ".*");

    }

    public void visit(FromImport sfi, ScalaWriter w) {

        String type = sfi.getModuleName();
        Collection<String> types = new HashSet<>();
        types.add(type);
        for (Name qa : sfi.getNames()) {
            types.add(qa.getName());
        }
        try {
            w.emitImports(type + ALL_IMPORTS);
        } catch (IOException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }
        // this.staticImports.add(this.packageName + "." + type + ".*");

    }

    public void visit(InterfaceDecl ed, ScalaWriter w) {
        try {
            final String identifier = ed.getName();

            beginElementKind(w, ElementKind.INTERFACE, identifier, DEFAULT_MODIFIERS, null,
                    ed.getExtendedInterfaceUses());
            this.classes.push(identifier);
            w.emitEmptyLine();
            ed.getAllMethodSigs().forEach(sig -> sig.accept(this, w));
            w.endType();
            this.classes.pop();

        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }

    public void visit(MethodSig ms, ScalaWriter w) {
        try {
            StringWriter auxsw = new StringWriter();
            ScalaWriter auxw = new ScalaWriter(auxsw);
            auxw.continuationLevel = w.continuationLevel;
            auxw.duplicateReplacements = w.duplicateReplacements;

            ms.getReturnType().accept(this, auxw);

            String returnType = String.format("%s[%s]", ABSFUTURE_CLASS, auxsw.toString());
            String name = ms.getName().equals(METHOD_GET) ? "get" : ms.getName();
            List<String> parameters = new ArrayList<>();
            List<String> parameterTypes = new ArrayList<>();
            for (ParamDecl param : ms.getParams()) {
                StringWriter typesw = new StringWriter();
                ScalaWriter tw = new ScalaWriter(typesw);
                param.getAccess().accept(this, tw);

                String pType = typesw.toString();
                parameters.add(pType);
                parameters.add(javaTypeTranslator.translateKeyword(param.getName()));
                parameterTypes.add(pType);
                VarDefinition vd = createVarDefinition(javaTypeTranslator.translateKeyword(param.getName()), pType);
                if (!variablesInScope.isEmpty()) {
                    variablesInScope.peek().add(vd);
                }
            }
            createMethodDefinition(returnType, name, parameterTypes);
            w.beginMethod(returnType, name, DEFAULT_MODIFIERS, parameters, Collections.emptyList());
            w.endMethod();
            w.emitEmptyLine();
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }

    public void visit(MethodImpl mcb, ScalaWriter w) {
        try {
            MethodSig ms = mcb.getMethodSig();
            if (w.checkAwaits) {

                StringWriter auxsw = new StringWriter();
                ScalaWriter auxw = new ScalaWriter(auxsw);
                auxw.continuationLevel = w.continuationLevel;
                auxw.duplicateReplacements = w.duplicateReplacements;

                ms.getReturnType().accept(this, auxw);

                String returnType = String.format("%s[%s]", ABSFUTURE_CLASS, auxsw.toString());
                String name = ms.getName().equals(METHOD_GET) ? "get" : ms.getName();

                if (name.equals(ACQUIRE_RESOURCE))
                    if (this.classes.peek().equals(CLASS_DC))
                        ACQUIRE = mcb;

                if (name.equals("run")) {
                    System.out.println("Found run in" + currentClass());
                    hasRun = true;
                }
                List<String> parameterTypes = new ArrayList<>();
                for (ParamDecl param : ms.getParams()) {
                    StringWriter typesw = new StringWriter();
                    ScalaWriter tw = new ScalaWriter(typesw);
                    param.getAccess().accept(this, tw);

                    String pType = typesw.toString();
                    parameterTypes.add(pType);
                    // createVarDefinition(p.l_, paramType);
                }
                createMethodDefinition(returnType, name, parameterTypes);
                visitMethodBody(mcb.getBlock(), w);

            } else {

                variablesInScope.clear();
                TreeSet<VarDefinition> methodScope = new TreeSet<>();
                variablesInScope.push(methodScope);
                StringWriter auxsw = new StringWriter();
                ScalaWriter auxw = new ScalaWriter(auxsw);
                auxw.continuationLevel = w.continuationLevel;
                auxw.duplicateReplacements = w.duplicateReplacements;

                ms.getReturnType().accept(this, auxw);
                String returnType = String.format("%s[%s]", ABSFUTURE_CLASS, auxsw.toString());
                String name = ms.getName().equals(METHOD_GET) ? "get" : ms.getName();

                if (name.equals("run")) {
                    System.out.println("Found run in" + currentClass());
                    hasRun = true;
                }

                //if (name.equals("run"))
                //    System.out.println(programMethods);

                List<String> parameters = new ArrayList<>();
                List<String> parameterTypes = new ArrayList<>();
                for (ParamDecl param : ms.getParams()) {
                    StringWriter typesw = new StringWriter();
                    ScalaWriter tw = new ScalaWriter(typesw);
                    param.getAccess().accept(this, tw);

                    String pType = typesw.toString();
                    parameters.add(pType);
                    parameters.add(javaTypeTranslator.translateKeyword(param.getName()));
                    parameterTypes.add(pType);
                    VarDefinition vd = createVarDefinition(javaTypeTranslator.translateKeyword(param.getName()), pType);
                    if (!variablesInScope.isEmpty()) {
                        variablesInScope.peek().add(vd);
                    }
                }
                // System.out.println(variablesInScope);

                w.beginMethod(returnType, name, DEFAULT_MODIFIERS, parameters, Collections.emptyList());
                createMethodDefinition(returnType, name, parameterTypes);

                org.abs_models.frontend.ast.List<Stmt> copyOfMcb = mcb.getBlock().getStmts();

                TreeSet<VarDefinition> blockScope = new TreeSet<>();
                variablesInScope.push(blockScope);

                awaitCounter = 0;
                asyncPCounter = 0;
                syncPCounter = 0;

                w.avoiddc = false;

                //System.out.println("Building method for " + name);
                for (Stmt annStm : mcb.getBlock().getStmts()) {

                    if (!w.avoiddc)
                        annStm.accept(this, w);

                }
                w.avoiddc = false;
                if (auxsw.toString().equals("Void")) {
                    w.emitStatement("return %s.done()", ABSFUTURE_CLASS);
                }

                //System.out.println("Done method for " + name);

                variablesInScope.pop();

                awaitCounter = 0;
                asyncPCounter = 0;
                syncPCounter = 0;

                //System.out.println("Building continuations for " + name);
                continuation(copyOfMcb, new LinkedList<>(), true);
                // visitStatementsBlock(mcb.listannstm_, w);
                variablesInScope.pop();
                //System.out.println("Done continuations for " + name);
                variablesInScope.clear();

                w.endMethod();

                for (StringWriter stringWriter : currentMethodLabels) {
                    labelMap.get(this.classes.peek()).add(stringWriter);

                }
                currentMethodLabels.clear();
                currentMethod = null;

                w.emitEmptyLine();
            }

        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }

    public void visit(ClassDecl cpi, ScalaWriter w) {
        //TODO looke at new first then do cpi.getAnnotationList() look ABS.Scheduler.Scheduler. If it exists, cast to FnApp, and then apply it.
        try {
            hasRun = false;
            final String className = dataCollisions.containsKey(cpi.getName()) ? dataCollisions.get(cpi.getName())
                    : cpi.getName();
            LinkedList<String> parameters = new LinkedList<>();
            parameters.add(ABS_API_ACTOR_CLASS);
            parameters.add("destCOG");

            if (withDC) {
                parameters.add(ABS_API_INTERFACE_CLASS);
                parameters.add("destDC");
            }

            if (cpi.hasParam()) {
                for (ParamDecl param : cpi.getParams()) {
                    StringWriter typesw = new StringWriter();
                    ScalaWriter tw = new ScalaWriter(typesw);
                    param.getAccess().accept(this, tw);


                    String fieldType = typesw.toString();
                    parameters.add(fieldType);
                    parameters.add(javaTypeTranslator.translateKeyword(param.getName()));
                    createVarDefinition(javaTypeTranslator.translateKeyword(param.getName()), fieldType);
                }
            }

            beginElementKind(w, ElementKind.CLASS, className, DEFAULT_MODIFIERS, ABS_API_ACTOR_CLASS,
                    cpi.getImplementedInterfaceUses(), parameters, true);
            this.classes.push(className);
            labelMap.put(className, new LinkedList<>());

            w.emitEmptyLine();

            emitImplicitConversions(w);

            for (FieldDecl cb : cpi.getFieldList()) {
                cb.accept(this, w);
            }


            for (MethodImpl cb : cpi.getMethods()) {

                cb.accept(this, w);
            }

            w.beginConstructor();
            w.emitStatement("%s(%s)", MOVE_TO_COG, parameters.get(1));

            if (withDC) {
                w.emitStatement("%s(%s)", SET_DC, parameters.get(3));

                if (className.equals(CLASS_DC)) {
                    w.emitStatement("%s.%s(%s)", TAS, ADD_DC, LITERAL_THIS);

                }
            }
            if (cpi.hasInitBlock()) {
                InitBlock ib = cpi.getInitBlock();
                ib.accept(this, w);
            }
            if (hasRun)
                w.emitStatement("%s.%s(()=>%s.run())", LITERAL_THIS, "send", LITERAL_THIS);
            hasRun = false;
            w.endConstructor();

            if (!w.checkAwaits) {
                for (InterfaceTypeUse itu :
                        cpi.getImplementedInterfaceUses()) {
                    InterfaceType it = (InterfaceType) itu.getType();
                    for (MethodSig sig :
                            it.getAllMethodSigs()) {
                        String qname = cpi.getModuleDecl().getName() + "." + cpi.getName();
                        if (checkAwait(sig.getName(), qname)) {
                            String key = cpi.getModuleDecl().getName() + "." + it.getSimpleName() + "." + sig.getName();
                            programMethods.get(key).setContainsAwait(true);
                        }
                    }
                }

            }
            w.emitEmptyLine();
            // emitToStringMethod(w);
            for (StringWriter continuation : labelMap.get(className)) {
                w.emit(continuation.toString());
            }


            w.emitEmptyLine();
            w.endType();
            this.classes.pop();
            labelMap.remove(className);
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }

    public void visit(FieldDecl p, ScalaWriter w) {
        try {

            StringWriter mtsw = new StringWriter();
            ScalaWriter mtw = new ScalaWriter(mtsw);
            p.getAccess().accept(this, mtw);

            String fieldType = mtsw.toString();
            /*if(p.getName().equals("exclusion")) {
                System.out.println(p.getAccess() + " exclusion");
                System.out.println(javaTypeTranslator.abstractTypes);
            }*/

            String fieldName = javaTypeTranslator.translateKeyword(p.getName());

            if (p.hasInitExp()) {
                fromInit = true;
                StringWriter auxsw = new StringWriter();
                ScalaWriter auxw = new ScalaWriter(auxsw);
                auxw.continuationLevel = w.continuationLevel;
                auxw.duplicateReplacements = w.duplicateReplacements;
                PureExp exp = p.getInitExp();
                if (exp instanceof DataConstructorExp)
                    initType = fieldType;
                exp.accept(this, auxw);
                initType = "";
                fromInit = false;
                emitField(w, fieldType, fieldName, auxsw.toString(), false);
            } else {
                emitField(w, fieldType, fieldName, "null", false);
            }
            createVarDefinition(fieldName, fieldType);

            w.emitEmptyLine();

        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }

    public void visit(DataTypeDecl dd, ScalaWriter w) {
        try {
            int counter = 0;
            int rank = 0;
            w.emitEmptyLine();
            Set<Modifier> mods = new HashSet<>(DEFAULT_MODIFIERS);
            mods.add(Modifier.ABSTRACT);
            String parentOrder = String.format("%s[%s]", ORDERED_INTERFACE_CLASS, dd.getName());
            beginElementKind(w, ElementKind.CLASS, dd.getName(), mods, parentOrder, null);
            emitField(w, "Int", "rank", null, false);
            w.endType();

            String parentDataInterface = dd.getName();
            // Define parent 'data' holder interface
            this.dataDeclarations.put(dd.getName(), parentDataInterface);
            // Each data declaration as an implementing class
            org.abs_models.frontend.ast.List<DataConstructor> lci = dd.getDataConstructors();
            for (DataConstructor constrIdent : lci) {
                if (!constrIdent.hasConstructorArg()) {
                    if (constrIdent.getName().equals(parentDataInterface)) {
                        dataCollisions.put(parentDataInterface, parentDataInterface + OF);

                        beginElementKind(w, ElementKind.OTHER, constrIdent.getName() + OF, DEFAULT_MODIFIERS,
                                parentDataInterface, null, new ArrayList<>(), false);
                    } else

                        beginElementKind(w, ElementKind.OTHER, constrIdent.getName(), DEFAULT_MODIFIERS,
                                parentDataInterface, null, new ArrayList<>(), false);

                    emitField(w, "Int", "rank", String.valueOf(rank++), true);
                    overrideCompare(dd.getName(), w);
                    w.endType();
                } else {
                    org.abs_models.frontend.ast.List<ConstructorArg> pci = constrIdent.getConstructorArgs();
                    List<String> parameters = new ArrayList<>();
                    List<String> types = new ArrayList<>();
                    int currentSpot = 0;
                    for (ConstructorArg ct : pci) {

                        StringWriter typesw = new StringWriter();
                        ScalaWriter tw = new ScalaWriter(typesw);
                        ct.getTypeUse().accept(this, tw);

                        String ctType = typesw.toString();
                        parameters.add(ctType);
                        types.add(ctType);

                        if (ct.hasSelectorName()) {

                            String ctName = javaTypeTranslator.translateKeyword(ct.getSelectorName().getName());
                            parameters.add(ctName);
                            /*
                             * functionsWriter.beginMethod(ctType, ctName,
                             * DEFAULT_MODIFIERS, dd.getName(),
                             * dd.getName().toLowerCase() + "par");
                             * functionsWriter.beginControlFlow("%s match",
                             * dd.getName().toLowerCase() + "par");
                             * StringBuilder matchList = new StringBuilder();
                             * for (int i = 0; i < pci.size(); i++) { if (i ==
                             * currentSpot) { matchList.append(ctName + "p"); }
                             * else matchList.append('_'); if (i < pci.size() -
                             * 1) matchList.append(','); }
                             * functionsWriter.emitStatement("case %s(%s) => %s"
                             * , constrIdent.getName(), matchList, ctName +
                             * "p"); functionsWriter.endControlFlow();
                             * functionsWriter.endMethod();
                             */
                        } else {
                            if (ctType.contains("["))
                                parameters.add(ctType.substring(0, ctType.indexOf('[')).toLowerCase() + counter++);
                            else
                                parameters.add(ctType.toLowerCase() + counter++);
                        }
                        currentSpot++;

                    }
                    // paramConstructs.put(pci.u_, types);
                    // caseTypeConstructs.get(dd.u_).add(pci.u_);
                    if (constrIdent.getName().equals(parentDataInterface)) {

                        dataCollisions.put(parentDataInterface, parentDataInterface + OF);
                        beginElementKind(w, ElementKind.OTHER, constrIdent.getName() + OF, DEFAULT_MODIFIERS,
                                parentDataInterface, null, parameters, false);
                    } else
                        beginElementKind(w, ElementKind.OTHER, constrIdent.getName(), DEFAULT_MODIFIERS,
                                parentDataInterface, null, parameters, false);

                    emitField(w, "Int", "rank", String.valueOf(rank++), true);
                    overrideCompare(dd.getName(), w);
                    w.endType();

                }
            }
        } catch (IOException e) {
            throw new RuntimeException(e);
        }

    }

    public void visit(ParametricDataTypeDecl dpd, ScalaWriter w) {
        try {
            int counter = 0;
            int rank = 0;
            ;
            Set<Modifier> mods = new HashSet<>(DEFAULT_MODIFIERS);
            mods.add(Modifier.ABSTRACT);
            List<String> listExtended = new ArrayList<>();
            List<String> listGens = new ArrayList<>();
            for (TypeParameterDecl generic : dpd.getTypeParameters()) {
                // if(dpd.u_.equals("Set"))
                listExtended
                        .add(generic.getName() + "<%" + ORDERED_INTERFACE_CLASS + "[_ >: " + generic.getName() + "]");
                listGens.add(generic.getName());

                // else
                // listExtended.add(generic);
            }

            String parentOrder = String.format("%s[%s]", ORDERED_INTERFACE_CLASS,
                    dpd.getName() + (listGens.isEmpty() ? "" : listGens));

            beginElementKind(w, ElementKind.CLASS,
                    String.format("%s%s", dpd.getName(), listExtended.isEmpty() ? "" : listExtended), mods, parentOrder,
                    null);
            emitField(w, "Int", "rank", null, false);
            w.endType();

            // caseTypeConstructs.put(dpd.getName(), new ArrayList<>());

            String parentDataInterface = dpd.getName();
            // Define parent 'data' holder interface
            this.dataDeclarations.put(dpd.getName(), parentDataInterface);
            // Each data declaration as an implementing class
            org.abs_models.frontend.ast.List<DataConstructor> lci = dpd.getDataConstructors();
            for (DataConstructor constrIdent : lci) {
                if (!constrIdent.hasConstructorArg()) {
                    if (constrIdent.getName().equals(parentDataInterface)) {
                        dataCollisions.put(parentDataInterface, parentDataInterface + OF);
                        beginElementKind(w, ElementKind.OTHER,
                                String.format("%s%s", constrIdent.getName() + OF,
                                        listExtended.isEmpty() ? "" : listExtended),
                                DEFAULT_MODIFIERS,
                                String.format("%s%s", parentDataInterface, (listGens.isEmpty() ? "" : listGens)), null,
                                new ArrayList<>(), false);
                    } else
                        beginElementKind(w, ElementKind.OTHER,
                                String.format("%s%s", constrIdent.getName(),
                                        listExtended.isEmpty() ? "" : listExtended),
                                DEFAULT_MODIFIERS,
                                String.format("%s%s", parentDataInterface, (listGens.isEmpty() ? "" : listGens)), null,
                                new ArrayList<>(), false);

                    emitField(w, "Int", "rank", String.valueOf(rank++), true);
                    overrideCompare(dpd.getName() + (listGens.isEmpty() ? "" : listGens), w);
                    w.endType();
                } else {
                    org.abs_models.frontend.ast.List<ConstructorArg> pci = constrIdent.getConstructorArgs();
                    List<String> parameters = new ArrayList<>();
                    List<String> types = new ArrayList<>();
                    int currentSpot = 0;
                    for (ConstructorArg ct : pci) {

                        StringWriter typesw = new StringWriter();
                        ScalaWriter tw = new ScalaWriter(typesw);
                        ct.getTypeUse().accept(this, tw);

                        String ctType = typesw.toString();
                        parameters.add(ctType);
                        types.add(ctType);

                        if (ct.hasSelectorName()) {

                            String ctName = javaTypeTranslator.translateKeyword(ct.getSelectorName().getName());
                            parameters.add(ctName);
                            // String parentPar =
                            // parentDataInterface.toLowerCase() + "par";
                            // functionsWriter.beginMethod(ctType, ctName +
                            // listExtended, DEFAULT_MODIFIERS,
                            // parentDataInterface + dpd.getTypeParameters(),
                            // parentPar);
                            // functionsWriter.beginControlFlow("%s match",
                            // parentPar);
                            // StringBuilder matchList = new StringBuilder();
                            // for (int i = 0; i < pci.size(); i++) {
                            // if (i == currentSpot) {
                            // matchList.append(ctName + "p");
                            // } else
                            // matchList.append('_');
                            // if (i < pci.size() - 1)
                            // matchList.append(',');
                            // }
                            // functionsWriter.emitStatement("case %s(%s) =>
                            // %s", constrIdent.getName(), matchList,
                            // ctName + "p");
                            // functionsWriter.endControlFlow();
                            // functionsWriter.endMethod();

                        } else {
                            if (ctType.contains("["))
                                parameters.add(ctType.substring(0, ctType.indexOf('[')).toLowerCase() + counter++);
                            else
                                parameters.add(ctType.toLowerCase() + counter++);
                        }
                        currentSpot++;

                    }
                    // paramConstructs.put(pci.u_, types);
                    // caseTypeConstructs.get(dd.u_).add(pci.u_);
                    if (constrIdent.getName().equals(parentDataInterface)) {
                        dataCollisions.put(parentDataInterface, parentDataInterface + OF);
                        beginElementKind(w, ElementKind.OTHER,
                                String.format("%s%s", constrIdent.getName() + OF,
                                        listExtended.isEmpty() ? "" : listExtended),
                                DEFAULT_MODIFIERS,
                                String.format("%s%s", parentDataInterface, (listGens.isEmpty() ? "" : listGens)), null,
                                parameters, false);
                    } else
                        beginElementKind(w, ElementKind.OTHER,
                                String.format("%s%s", constrIdent.getName(),
                                        listExtended.isEmpty() ? "" : listExtended),
                                DEFAULT_MODIFIERS,
                                String.format("%s%s", parentDataInterface, (listGens.isEmpty() ? "" : listGens)), null,
                                parameters, false);

                    emitField(w, "Int", "rank", String.valueOf(rank++), true);
                    overrideCompare(dpd.getName() + (listGens.isEmpty() ? "" : listGens), w);
                    w.endType();

                }
            }
        } catch (IOException e) {
            throw new RuntimeException(e);
        }

    }

    public void visit(FunctionDecl functionDecl, ScalaWriter w) {
        try {
            this.classes.push("Functions");
            String methodName = functionDecl.getName();

            StringWriter mtsw = new StringWriter();
            ScalaWriter mtw = new ScalaWriter(mtsw);
            functionDecl.getTypeUse().accept(this, mtw);

            String methodType = mtsw.toString();
            List<String> parameters = new ArrayList<>();

            variablesInScope.clear();
            TreeSet<VarDefinition> methodScope = new TreeSet<>();
            variablesInScope.push(methodScope);
            for (ParamDecl param : functionDecl.getParams()) {
                StringWriter typesw = new StringWriter();
                ScalaWriter tw = new ScalaWriter(typesw);
                param.getAccess().accept(this, tw);

                String paramType = typesw.toString();
                parameters.add(paramType);
                parameters.add(javaTypeTranslator.translateKeyword(param.getName()));

                VarDefinition vd = createVarDefinition(javaTypeTranslator.translateKeyword(param.getName()), paramType);
                if (!variablesInScope.isEmpty()) {
                    variablesInScope.peek().add(vd);
                }

            }
            FunctionDef fbody = functionDecl.getFunctionDef();
            if (fbody instanceof BuiltinFunctionDef) {
                if (methodName.equals(THIS_DC)) {
                    BUILTIN_THIS_DC = functionDecl;
                    parameters.add(ABS_API_ACTOR_CLASS);
                    parameters.add(ABS_API_ACTOR_CLASS.toLowerCase());

                }
            }
            Set<Modifier> modifiers = Sets.newHashSet(Modifier.PUBLIC, Modifier.STATIC);

            w.beginMethod(methodType.equals("Void") ? null : methodType, methodName, modifiers, parameters.toArray(new String[0]));

            createMethodDefinition(methodType, methodName, parameters);

            if (fbody instanceof BuiltinFunctionDef) {
                if (methodName.equals("thisDC")) {
                    if (!withDC)
                        w.emitStatement("return %s", LITERAL_NULL);
                    else
                        w.emitStatement("return %s.%s.%s[%s]", ABS_API_ACTOR_CLASS.toLowerCase(), GET_DC, INSTANCE_OF, DEPLOYMENT_COMPONENT);
                }

            } else if (fbody instanceof ExpFunctionDef) {
                ExpFunctionDef nfb = (ExpFunctionDef) fbody;
                org.abs_models.frontend.ast.PureExp pe = nfb.getRhs();

                StringWriter sw = new StringWriter();
                ScalaWriter auxjw = new ScalaWriter(sw);

                auxjw.continuationLevel = w.continuationLevel;
                auxjw.duplicateReplacements = w.duplicateReplacements;

                pe.accept(this, auxjw);
                String stm = sw.toString();
                if (methodType.equals("Void"))
                    w.emitStatement("%s", stm);
                else
                    w.emitStatement("return %s", stm);

            }
            variablesInScope.pop();
            variablesInScope.clear();
            w.endMethod();
            w.emitEmptyLine();

            this.classes.push("Functions");
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }

    public void visit(ParametricFunctionDecl functionDecl, ScalaWriter w) {
        try {
            this.classes.push("Functions");
            String methodName = functionDecl.getName();
            StringWriter mtsw = new StringWriter();
            ScalaWriter mtw = new ScalaWriter(mtsw);
            functionDecl.getTypeUse().accept(this, mtw);

            String methodType = mtsw.toString();
            List<String> parameters = new ArrayList<>();

            variablesInScope.clear();
            TreeSet<VarDefinition> methodScope = new TreeSet<>();
            variablesInScope.push(methodScope);
            for (ParamDecl param : functionDecl.getParams()) {
                StringWriter typesw = new StringWriter();
                ScalaWriter tw = new ScalaWriter(typesw);
                param.getAccess().accept(this, tw);

                String paramType = typesw.toString();

                parameters.add(paramType);
                parameters.add(javaTypeTranslator.translateKeyword(param.getName()));
                VarDefinition vd = createVarDefinition(javaTypeTranslator.translateKeyword(param.getName()), paramType);
                if (!variablesInScope.isEmpty()) {
                    variablesInScope.peek().add(vd);
                }

            }

            FunctionDef fbody = functionDecl.getFunctionDef();
            if (fbody instanceof BuiltinFunctionDef) {
                if (methodName.equals("thisDC")) {
                    parameters.add(ABS_API_ACTOR_CLASS);
                    parameters.add(ABS_API_ACTOR_CLASS.toLowerCase());

                }
            }
            Set<Modifier> modifiers = Sets.newHashSet(Modifier.PUBLIC, Modifier.STATIC);
            List<String> generics = new LinkedList<>();
            List<String> genericsNames = new LinkedList<>();

            for (TypeParameterDecl modifier : functionDecl.getTypeParameters()) {
                generics.add(modifier.getName() + "<%" + ORDERED_INTERFACE_CLASS + "[ _ >:" + modifier.getName() + "]");
                genericsNames.add(modifier.getName());
            }
            w.beginMethod(methodType, methodName + (generics.isEmpty() ? "" : generics), modifiers,
                    parameters.toArray(new String[0]));
            if (fbody instanceof BuiltinFunctionDef) {
                if (methodName.equals("thisDC")) {
                    if (!withDC)
                        w.emitStatement("return %s", LITERAL_NULL);
                    else
                        w.emitStatement("return %s.%s.%s[%s]", ABS_API_ACTOR_CLASS.toLowerCase(), GET_DC, INSTANCE_OF, DEPLOYMENT_COMPONENT);
                }

            } else if (fbody instanceof ExpFunctionDef) {
                ExpFunctionDef nfb = (ExpFunctionDef) fbody;
                org.abs_models.frontend.ast.PureExp pe = nfb.getRhs();

                StringWriter sw = new StringWriter();
                ScalaWriter auxjw = new ScalaWriter(sw);

                auxjw.continuationLevel = w.continuationLevel;
                auxjw.duplicateReplacements = w.duplicateReplacements;

                pe.accept(this, auxjw);
                String stm = sw.toString();
                w.emitStatement("return %s", stm);

            }
            variablesInScope.pop();
            variablesInScope.clear();
            w.endMethod();
            w.emitEmptyLine();
            this.classes.pop();
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }

    public void visit(TypeSynDecl typeSynDecl, ScalaWriter w) {
        String adtName = typeSynDecl.getName();
        StringWriter sw = new StringWriter();
        ScalaWriter tw = new ScalaWriter(sw);
        typeSynDecl.getValue().accept(this, tw);
        String typeName = sw.toString();
        this.javaTypeTranslator.registerAbstractType(adtName, typeName);

    }

    public void visit(AssignStmt ss, ScalaWriter w) {
        try {
            org.abs_models.frontend.ast.Exp exp = ss.getValue();
            StringWriter vsw = new StringWriter();
            ScalaWriter vw = new ScalaWriter(vsw);
            vw.duplicateReplacements = w.duplicateReplacements;
            vw.continuationLevel = w.continuationLevel;
            ss.getVar().accept(this, vw);

            String varName = vsw.toString();

            /*
             * if (w.continuationLevel >= -1 || w.avoidDuplicates) {
             * if(w.methodParameters.containsKey(varName)){
             * duplicateReplacements.peek().put(varName, "w_" + awaitCounter +
             * "$" + varName); type=w.methodParameters.get(varName); } }
             */
            StringWriter sw = new StringWriter();
            ScalaWriter tw = new ScalaWriter(sw);
            ss.getVar().getType().toUse().accept(this, tw);
            String varType = sw.toString();
            if (exp instanceof EffExp == false) {
                // initType = varType;
                visitStatementAssignmentExp(exp, varName, null, w);
            } else {
                EffExp expe = (EffExp) exp;
                if (expe instanceof AsyncCall) {
                    AsyncCall amc = (AsyncCall) expe;
                    visitAsyncMethodCall(amc, varType, varName, true, w);
                } else if (expe instanceof SyncCall) {
                    SyncCall smc = (SyncCall) expe;
                    visitSyncMethodCall_Sync(smc, varType, varName, true, w);
                } else if (expe instanceof GetExp) {

                    GetExp g = (GetExp) expe;
                    varName = getDuplicate(varName, w);
                    visitGet(g, varType, varName, true, w);
                } else {
                    visitStatementAssignmentExp(exp, varName, null, w);
                }
            }
        } catch (IOException e) {
            throw new RuntimeException(e);
        }

    }

    public void visit(AwaitStmt awaitStmt, ScalaWriter w) {
        // System.out.println(w.checkAwaits + " AWAIT " +
        // currentMethod.getName());

        if (w.checkAwaits && !currentMethod.containsAwait()) {
            currentMethod.setContainsAwait(true);
            awaitsDetected = true;
        }
        StringWriter auxsw = new StringWriter();
        ScalaWriter auxw = new ScalaWriter(auxsw);
        auxw.continuationLevel = w.continuationLevel;
        auxw.duplicateReplacements = w.duplicateReplacements;
        awaitStmt.getGuard().accept(this, auxw);
        StringBuilder label = new StringBuilder(classes.peek());
        label.append(currentMethod.getName());

        if (!w.isScope && !w.checkAwaits) {
            label.append("Await" + (awaitCounter));
        }

        try {
            if (auxsw.toString().contains("new Supplier"))
                w.beginControlFlow("if(%s.get())", auxsw.toString());
            else if (auxsw.toString().contains("Array"))
                w.beginControlFlow("if(%s(0)<=0)", auxsw.toString());
            else
                w.beginControlFlow("if(%s.isDone())", auxsw.toString());
            w.emitStatement("return %s", generateContinuationMethodInvocation("this", label.toString(), w, 'w', awaitCounter, false));
            w.endControlFlow();
            w.beginControlFlow("else");
            w.emitStatement("return spawn(Guard.convert(%s),%s)", auxsw.toString(), "()=>" + generateContinuationMethodInvocation("this", label.toString(), w, 'w', awaitCounter, true)
            );
            if (!w.isScope && !w.checkAwaits) {
                awaitCounter++;
            }
            w.avoiddc = true;
            w.endControlFlow();

        } catch (IOException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }

    }

    public void visit(ReturnStmt returnStmt, ScalaWriter w) {
        try {
            StringWriter auxsw = new StringWriter();
            ScalaWriter auxw = new ScalaWriter(auxsw);
            auxw.continuationLevel = w.continuationLevel;
            auxw.duplicateReplacements = w.duplicateReplacements;
            returnStmt.getRetExp().accept(this, auxw);
            w.emitStatement("return ABSFuture.done({" + auxsw.toString() + "})");
        } catch (IOException e) {
            throw new RuntimeException(e);
        }

    }

    public void visit(ExpressionStmt e, ScalaWriter w) {
        try {
            Exp exp = e.getExp();
            /*
             * StringWriter auxsw = new StringWriter(); ScalaWriter auxjw = new
             * ScalaWriter(auxsw); auxjw.duplicateReplacements =
             * w.duplicateReplacements; auxjw.continuationLevel =
             * w.continuationLevel;
             */
            exp.accept(this, w);
            // w.emit(auxsw.toString(), true);
            if (exp instanceof EffExp) {
                EffExp expE = (EffExp) exp;
                if (expE instanceof GetExp || expE instanceof NewExp) {
                    // XXX Ideally fix the indentation
                    w.emitStatementEnd();
                }
            } else if (exp instanceof PureExp) {
                if ((PureExp) exp instanceof FnApp) {
                    w.emitStatementEnd();
                }
            }
        } catch (IOException ex) {
            throw new RuntimeException(ex);
        }

    }

    public void visit(AssertStmt assertStmt, ScalaWriter w) {
        try {
            StringWriter sw = new StringWriter();
            ScalaWriter auxw = new ScalaWriter(sw);
            auxw.continuationLevel = w.continuationLevel;
            auxw.duplicateReplacements = w.duplicateReplacements;
            assertStmt.getCondition().accept(this, auxw);
            w.emitStatement("assert(%s)", sw.toString());

        } catch (IOException e) {
            throw new RuntimeException(e);
        }

    }

    public void visit(VarDeclStmt varDeclStmt, ScalaWriter w) {
        try {

            StringWriter auxsw2 = new StringWriter();
            ScalaWriter auxw2 = new ScalaWriter(auxsw2);
            auxw2.continuationLevel = w.continuationLevel;
            auxw2.duplicateReplacements = w.duplicateReplacements;
            varDeclStmt.getVarDecl().getAccess().accept(this, auxw2);

            String varType = auxsw2.toString();
            String varName = javaTypeTranslator.translateKeyword(varDeclStmt.getVarDecl().getName());
            VarDefinition vd = createVarDefinition(varName, varType);


            if ((w.continuationLevel > -1 || w.avoidDuplicates)) {
                // System.out.println(varName + " " + "w_" + awaitCounter + "$"
                // + varName);
                w.duplicateReplacements.peek().put(varName, "w_" + awaitCounter + "$" + varName);
            }
            if (varDeclStmt.getVarDecl().hasInitExp()) {

                Exp exp = varDeclStmt.getVarDecl().getInitExp();

                if (!variablesInScope.isEmpty()) {
                    if (exp instanceof EffExp) {
                        EffExp expe = (EffExp) exp;
                        if (!(expe instanceof GetExp)) {
                            if (expe instanceof SyncCall) {
                                SyncCall smc = (SyncCall) expe;
                                String methodName = smc.getMethod().equals(METHOD_GET) ? "get" : smc.getMethod();
                                String calleeid = getCalleeId(smc, new ScalaWriter(new StringWriter()));

                                if (!callHasAwait(smc, methodName) && calleeid.equals(LITERAL_THIS)) {
                                    variablesInScope.peek().add(vd);
                                }

                            } else
                                variablesInScope.peek().add(vd);
                        }
                    } else
                        variablesInScope.peek().add(vd);

                }


                if (exp instanceof EffExp == false) {
                    fromInit = true;
                    visitStatementAssignmentExp(exp, varName, varType, w);
                    fromInit = false;
                } else {
                    EffExp expe = (EffExp) exp;
                    if (expe instanceof AsyncCall) {
                        AsyncCall amc = (AsyncCall) expe;
                        visitAsyncMethodCall(amc, varType, varName, true, w);
                    } else if (expe instanceof SyncCall) {
                        SyncCall smc = (SyncCall) expe;
                        visitSyncMethodCall_Sync(smc, varType, varName, false, w);
                        String methodName = smc.getMethod().equals(METHOD_GET) ? "get" : smc.getMethod();
                        if (callHasAwait(smc, methodName) || !getCalleeId(smc, new ScalaWriter(new StringWriter())).equals(LITERAL_THIS)) {
                            if (!variablesInScope.isEmpty()) {
                                variablesInScope.peek().add(vd);
                            }
                        }

                    } else if (expe instanceof GetExp) {
                        GetExp g = (GetExp) expe;
                        varName = getDuplicate(varName, w);
                        visitGet(g, varType, varName, false, w);
                        if (!variablesInScope.isEmpty()) {
                            variablesInScope.peek().add(vd);
                        }

                    } else {
                        visitStatementAssignmentExp(exp, varName, varType, w);
                    }
                }
            } else {
                emitField(w, varType, varName, null, false);

                if (!variablesInScope.isEmpty()) {
                    variablesInScope.peek().add(vd);
                }
                verifyJavaStatic(varType, varName);
                w.emitEmptyLine();
            }
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }

    public void visit(IfStmt se, ScalaWriter w) {
        try {
            StringWriter auxsw = new StringWriter();
            ScalaWriter auxw = new ScalaWriter(auxsw);
            auxw.continuationLevel = w.continuationLevel;
            auxw.duplicateReplacements = w.duplicateReplacements;
            se.getCondition().accept(this, auxw);
            w.beginControlFlow("if (" + auxsw.toString() + ")");

            se.getThen().accept(this, w);
            w.endControlFlow();

            if (se.hasElse()) {
                w.beginControlFlow("else");

                se.getElse().accept(this, w);
                w.endControlFlow();
            }
        } catch (IOException e) {
            throw new RuntimeException(e);
        }

    }

    public void visit(WhileStmt sw, ScalaWriter w) {
        try {
            StringWriter auxsw = new StringWriter();
            ScalaWriter auxw = new ScalaWriter(auxsw);
            auxw.continuationLevel = w.continuationLevel;
            auxw.duplicateReplacements = w.duplicateReplacements;

            sw.getCondition().accept(this, auxw);
            w.beginControlFlow("while (" + auxsw.toString() + ")");
            sw.getBody().accept(this, w);
            w.endControlFlow();

        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }

    public void visit(CaseStmt p, ScalaWriter w) {
        try {
            StringWriter auxsw = new StringWriter();
            ScalaWriter auxw = new ScalaWriter(auxsw);
            auxw.continuationLevel = w.continuationLevel;
            auxw.duplicateReplacements = w.duplicateReplacements;

            if (!w.checkAwaits)
                p.getExpr().accept(this, auxw);
            w.beginControlFlow("%s match ", auxsw);

            // if (!w.checkAwaits) {
            // caseKey = findVariableTypeInScope(auxsw.toString());
            // System.out.println("#############" + caseKey + " " +
            // auxsw.toString() + "#################");
            // }
            // System.out.println(caseKey);
            for (CaseBranchStmt scb : p.getBranchs()) {
                scb.accept(this, w);
            }
            // caseKey = "";
            w.endControlFlow();

        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }

    public void visit(CaseBranchStmt cb, ScalaWriter w) {
        try {

            StringWriter patsw = new StringWriter();
            ScalaWriter patw = new ScalaWriter(patsw);
            patw.continuationLevel = w.continuationLevel;
            patw.duplicateReplacements = w.duplicateReplacements;
            cb.getLeft().accept(this, patw);

            StringWriter stmsw = new StringWriter();
            ScalaWriter stmw = new ScalaWriter(stmsw);
            stmw.continuationLevel = w.continuationLevel;
            stmw.duplicateReplacements = w.duplicateReplacements;
            stmw.checkAwaits = w.checkAwaits;

            cb.getRight().accept(this, stmw);
            // TODO1: Add declarations inside block
            // caseKey = oldKey;

            w.emitStatement("case %s => %s", patsw.toString(), stmsw.toString());
            // variablesBeforeBlock.clear();
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }

    public void visit(InitBlock b, ScalaWriter w) {
        try {
            if (w.checkAwaits) {


                String returnType = String.format("%s[%s]", ABSFUTURE_CLASS, "Void");
                String name = CONSTRUCTOR;
                List<String> parameterTypes = new ArrayList<>();
                createMethodDefinition(returnType, name, parameterTypes);
                visitMethodBody(b, w);
            } else {

                variablesInScope.clear();
                TreeSet<VarDefinition> methodScope = new TreeSet<>();
                variablesInScope.push(methodScope);
                String returnType = String.format("%s[%s]", ABSFUTURE_CLASS, "Void");
                String name = CONSTRUCTOR;
                List<String> parameterTypes = new ArrayList<>();
                createMethodDefinition(returnType, name, parameterTypes);

                org.abs_models.frontend.ast.List<Stmt> copyOfMcb = b.getStmts();

                TreeSet<VarDefinition> blockScope = new TreeSet<>();
                variablesInScope.push(blockScope);

                awaitCounter = 0;
                asyncPCounter = 0;
                syncPCounter = 0;

                w.avoiddc = false;

                //System.out.println("Building method for " + name);
                fromConstructor = true;
                for (Stmt annStm : b.getStmts()) {
                    if (!w.avoiddc)
                        annStm.accept(this, w);

                }
                fromConstructor = false;
                if (!w.avoiddc)
                    w.emitStatement("%s =  %s.done()", CONS_FUT, ABSFUTURE_CLASS);
                w.avoiddc = false;

                //System.out.println("Done method for " + name);


                variablesInScope.pop();

                awaitCounter = 0;
                asyncPCounter = 0;
                syncPCounter = 0;

                System.out.println("Building continuations for " + name);
                continuation(copyOfMcb, new LinkedList<>(), true);
                // visitStatementsBlock(mcb.listannstm_, w);
                variablesInScope.pop();
                System.out.println("Done continuations for " + name);
                variablesInScope.clear();


                for (StringWriter stringWriter : currentMethodLabels) {
                    labelMap.get(this.classes.peek()).add(stringWriter);

                }
                currentMethodLabels.clear();
                currentMethod = null;

                w.emitEmptyLine();
            }

        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }

    public void visit(Block b, ScalaWriter w) {


        TreeSet<VarDefinition> blockScope = new TreeSet<>();
        variablesInScope.push(blockScope);
        if (w.continuationLevel != -5)
            w.continuationLevel++;

        HashMap<String, String> duplicateScope = new HashMap<>();
        w.duplicateReplacements.push(duplicateScope);

        if (!variablesBeforeBlock.isEmpty()) {
            for (VarDefinition vd : variablesBeforeBlock) {
                if ((w.continuationLevel > -1 || w.avoidDuplicates)) {
                    w.duplicateReplacements.peek().put(vd.getName(), "w_" + awaitCounter + "$" + vd.getName());
                }

                if (!variablesInScope.isEmpty()) {
                    variablesInScope.peek().add(vd);
                }
            }
            variablesBeforeBlock.clear();
        }

        // System.out.println("new block " + w.duplicateReplacements);
        StringWriter scopesw = new StringWriter();
        ScalaWriter scopew = new ScalaWriter(scopesw);
        TreeMap<Integer, Stmt> annHandlers = new TreeMap<>(new Comparator<Integer>() {
            @Override
            public int compare(Integer integer, Integer t1) {
                return t1 - integer;
            }
        });
        for (Stmt stm : b.getStmts()) {
            //TODO: stm.getAnnotationList(), check for "ABS.DC.COST"
            //TODO: if cost is not null, decrement available cost, if > 0 continue, else decrement all, and tell the DC that I need more.
            //TODO: thisDC has to be generated spearately by the Builtin visitor node.
            //TODO:
            if (withDC && firstDCPass == 0) {
                PureExp cost = AnnotationHelper.getAnnotationValueFromName(stm.getAnnotationList(), "ABS.DC.Cost");
                if (cost != null) {
                    if (!currentMethod.containsAwait()) {
                        currentMethod.setContainsAwait(true);
                        awaitsDetected = true;
                    }

                    List<PureExp> pars = new LinkedList<>();
                    DataConstructorExp rtype = new DataConstructorExp("Speed", new org.abs_models.frontend.ast.List<>());
                    pars.add(cost.treeCopyNoTransform());
                    pars.add(rtype);

                    //Call acquire = AbsASTBuilderUtil.getCall(AbsASTBuilderUtil.getFnApp("thisDC"), "acquireResource", false, cost.treeCopyNoTransform(),rtype);
                    AwaitAsyncCall aws = new AwaitAsyncCall(AbsASTBuilderUtil.getFnApp("thisDC"), "acquireResource", ListUtils.toASTList(pars));
                    Stmt s = AbsASTBuilderUtil.getExpStmt(aws);
                    annHandlers.put(b.getStmts().getIndexOfChild(stm), s);
                }
            }

            if (!w.avoiddc)
                stm.accept(this, w);
            else
                stm.accept(this, scopew);
        }
        if (withDC && firstDCPass == 0) {
            for (Integer k : annHandlers.keySet()
                    ) {
                b.getStmts().insertChild(annHandlers.get(k), k);
            }
        }
        w.avoiddc = false;
        // System.out.println(variablesInScope);
        w.duplicateReplacements.pop();
        variablesInScope.pop();
        if (w.continuationLevel > -5)
            w.continuationLevel--;

    }

    public void visit(ExpGuard p, ScalaWriter w) {
        try {
            StringWriter suppliersw = new StringWriter();
            ScalaWriter supplierw = new ScalaWriter(suppliersw);
            supplierw.continuationLevel = w.continuationLevel;
            supplierw.duplicateReplacements = w.duplicateReplacements;
            w.beginControlFlow("new Supplier[Boolean]");
            fromSupplier = true;
            p.getPureExp().accept(this, supplierw);
            w.beginMethod("Boolean", "get", DEFAULT_MODIFIERS);
            w.emitStatement("return %s", suppliersw.toString());
            w.endMethod();
            w.endControlFlow();
            fromSupplier = false;
        } catch (IOException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }
    }

    public void visit(DurationGuard e, ScalaWriter w) {
        try {
            StringWriter minsw = new StringWriter();
            ScalaWriter minw = new ScalaWriter(minsw);
            minw.continuationLevel = w.continuationLevel;
            minw.duplicateReplacements = w.duplicateReplacements;
            e.getMin().accept(this, minw);

            StringWriter maxsw = new StringWriter();
            ScalaWriter maxw = new ScalaWriter(maxsw);
            maxw.continuationLevel = w.continuationLevel;
            maxw.duplicateReplacements = w.duplicateReplacements;
            e.getMax().accept(this, maxw);

            w.emit(String.format("Array(truncate(%s),truncate(%s))", minsw, maxsw));

        } catch (IOException ex) {
            // TODO Auto-generated catch block
            ex.printStackTrace();
        }
    }

    public void visit(ClaimGuard p, ScalaWriter w) {
        try {

            StringWriter futsw = new StringWriter();
            ScalaWriter futw = new ScalaWriter(futsw);
            futw.continuationLevel = w.continuationLevel;
            futw.duplicateReplacements = w.duplicateReplacements;
            p.getVar().accept(this, futw);
            w.emit(getDuplicate(futsw.toString(), w));

        } catch (IOException x) {
            throw new RuntimeException(x);
        }
    }

    public void visit(AndGuard p, ScalaWriter w) {
        try {
            StringWriter suppliersw = new StringWriter();
            ScalaWriter supplierw = new ScalaWriter(suppliersw);

            supplierw.continuationLevel = w.continuationLevel;
            supplierw.duplicateReplacements = w.duplicateReplacements;

            StringWriter suppliersw2 = new StringWriter();
            ScalaWriter supplierw2 = new ScalaWriter(suppliersw);
            supplierw2.continuationLevel = w.continuationLevel;
            supplierw2.duplicateReplacements = w.duplicateReplacements;

            StringBuilder supplierName = new StringBuilder();
            supplierName.append("supplier_" + Math.abs(p.hashCode()));
            p.getLeft().accept(this, supplierw);
            p.getRight().accept(this, supplierw2);
            w.emit("new ConjunctionGuard(Guard.convert(" + suppliersw.toString() + "),Guard.convert("
                    + suppliersw2.toString() + "))");
        } catch (IOException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }
    }

    public void visit(AsyncCall amc, ScalaWriter w) {

        StringWriter auxsw = new StringWriter();
        ScalaWriter auxw = new ScalaWriter(auxsw);
        auxw.continuationLevel = w.continuationLevel;
        auxw.duplicateReplacements = w.duplicateReplacements;

        amc.getMethodSig().getReturnType().accept(this, auxw);
        String retType = String.format("[%s]", auxsw.toString());
        try {
            visitAsyncMethodCall(amc, retType, null, false, w);

        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }

    public void visit(AwaitAsyncCall amc, ScalaWriter w) {

    }

    public void visit(SyncCall smc, ScalaWriter w) {
        StringWriter auxsw = new StringWriter();
        ScalaWriter auxw = new ScalaWriter(auxsw);
        auxw.continuationLevel = w.continuationLevel;
        auxw.duplicateReplacements = w.duplicateReplacements;

        System.out.println(smc.getMethod() + " " + smc.getCallee() + " " + smc.getMethodSig());
        MethodSig sig = smc.getMethodSig();
        Access r = sig.getReturnType();
        r.accept(this, auxw);

        try {
            visitSyncMethodCall_Sync(smc, auxsw.toString(), null, false, w);
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }

    public void visit(NewExp n, ScalaWriter w) {
        try {
            //TODO: CompilerUtils.findstatementfor expression (GenerateErlang.jadd l330)
            //TODO: stmt.getAnnotations (ABS.DC.DC)
            //TODO: if there's no dc annotation, use the current one
            //TODO: if a DC is specified, parse a pureExp, assign a parsed DC to the new COG.
            //TODO: n.getType().isDeploymentComponentType()


            Stmt stmt = CompilerUtils.findStmtForExpression(n);
            PureExp dc = AnnotationHelper.getAnnotationValueFromName(stmt.getAnnotations(), "ABS.DC.DC");
            boolean isNewDC = n.getType().isDeploymentComponentType();


            List<String> parameters = new ArrayList<>();
            String name = dataCollisions.containsKey(n.getClassName()) ? dataCollisions.get(n.getClassName())
                    : n.getClassName();
            for (PureExp par : n.getParams()) {
                StringWriter parSW = new StringWriter();
                ScalaWriter parameterWriter = new ScalaWriter(parSW);
                parameterWriter.continuationLevel = w.continuationLevel;
                parameterWriter.duplicateReplacements = w.duplicateReplacements;
                par.accept(this, parameterWriter);
                parameters.add(parSW.toString());
            }
            String parametersString = String.join(COMMA_SPACE, parameters);
            if (n.hasLocal()) {
                if (!withDC)
                    w.emit("new " + name + "(this " + (parametersString.length() == 0 ? "" : ", ") + parametersString
                            + ")");
                else
                    w.emit("new " + name + "(this, getDc(), " + parametersString + ")");
            } else {
                if (isNewDC) {
                    if (withDC)
                        w.emit("new " + name + "(null, null, " + parametersString + ")");
                    else
                        w.emit("new " + name + "(null " + (parametersString.length() == 0 ? "" : ", ") + parametersString
                                + ")");

                } else {
                    if (!withDC)
                        w.emit("new " + name + "(null " + (parametersString.length() == 0 ? "" : ", ") + parametersString
                                + ")");
                    else {
                        if (dc == null)
                            w.emit("new " + name + "(null, getDc(), " + parametersString + ")");

                        else {
                            StringWriter dcsw = new StringWriter();
                            ScalaWriter dcw = new ScalaWriter(dcsw);
                            dcw.continuationLevel = w.continuationLevel;
                            dcw.duplicateReplacements = w.duplicateReplacements;
                            dc.accept(this, dcw);


                            w.emit("new " + name + "(null, " + dcsw.toString() + ", " + parametersString + ")");
                        }
                    }
                }
            }

            /*
            String qname = n.getType().getQualifiedName();
            if(checkAwait(CONSTRUCTOR,qname)) {

                StringBuilder label = new StringBuilder(classes.peek());
                label.append(currentMethod.getName());

                if (!w.isScope && !w.checkAwaits) {
                    label.append("Await" + (awaitCounter));
                }

                StringBuilder valueName = new StringBuilder("unusedValue");

                if (!w.isScope && !w.checkAwaits) {
                    label.append("Await" + (awaitCounter));
                }

                String methodCall = generateContinuationMethodInvocation("this", label.toString(), w, 'w', awaitCounter, true);
                try {
                    //w.emitStatement("var %s: %s=>%s = (%s)=>%s", label + "m", type, currentMethod.type(), valueName,
                    //       methodCall);
                    w.emitStatement("getSpawn(%s.%s, (%s)=>%s, %s.HIGH_PRIORITY, false)", varName, GET_CONSTRUCTOR_FUTURE, valueName, methodCall
                            ABS_API_INTERFACE_CLASS);
                    w.avoiddc = true;
                    if (!w.isScope && !w.checkAwaits) {
                        awaitCounter++;
                    }
                } catch (IOException e) {
                    // TODO Auto-generated catch block
                    e.printStackTrace();
                }
            }*/

        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }

    public void visit(GetExp g, ScalaWriter w) {


        if (w.checkAwaits && !currentMethod.containsAwait()) {
            currentMethod.setContainsAwait(true);
            awaitsDetected = true;
        }
        StringWriter auxsw = new StringWriter();
        ScalaWriter auxw = new ScalaWriter(auxsw);
        auxw.continuationLevel = w.continuationLevel;
        auxw.duplicateReplacements = w.duplicateReplacements;
        g.getPureExp().accept(this, auxw);

        StringWriter tsw = new StringWriter();
        ScalaWriter tw = new ScalaWriter(tsw);
        g.getPureExp().getType().toUse().accept(this, tw);

        String type = tsw.toString();

        if (!auxsw.toString().contains("tmp")) {

            StringBuilder valueName = new StringBuilder("unusedValue: Void");

            StringBuilder label = new StringBuilder(classes.peek());
            label.append(currentMethod.getName());

            if (!w.isScope && !w.checkAwaits) {
                label.append("Await" + (awaitCounter));
            }

            List<String> parameters = new ArrayList<>();
            for (TreeSet<VarDefinition> defs : variablesInScope) {
                for (VarDefinition varDefinition : defs) {
                    parameters.add(varDefinition.getName());
                }
            }
            String methodCall = generateContinuationMethodInvocation("this", label.toString(), w, 'w', awaitCounter, true);

            try {
                String ret = currentMethod.type();
                String cgrt = ret.substring(ret.indexOf("[") + 1, ret.lastIndexOf("]"));
                String atype = type.substring(type.indexOf("[") + 1, type.lastIndexOf("]"));
                w.emitStatement("var %s: CallableGet[%s,%s] = (%s)=>%s", label + "m", cgrt, atype, valueName,
                        methodCall);
                w.emitStatement("return getSpawn(%s, %s, %s.HIGH_PRIORITY, true)", auxsw.toString(), label + "m",
                        ABS_API_INTERFACE_CLASS);
                w.avoiddc = true;
                if (!w.isScope && !w.checkAwaits) {
                    awaitCounter++;
                }
            } catch (IOException e) {
                // TODO Auto-generated catch block
                e.printStackTrace();
            }
        }
    }

    public void visit(NullExp p, ScalaWriter w) {
        try {
            w.emit(LITERAL_NULL);

        } catch (IOException x) {
            throw new RuntimeException(x);
        }
    }

    public void visit(ThisExp p, ScalaWriter w) {
        try {
            w.emit(LITERAL_THIS);

        } catch (IOException x) {
            throw new RuntimeException(x);
        }
    }

    public void visit(AsExp p, ScalaWriter w) {

    }

    public void visit(ImplementsExp p, ScalaWriter w) {

    }

    public void visit(DataConstructorExp cons, ScalaWriter w) {
        try {
            String functionName = cons.getConstructor();

            functionName = translateDataColl(functionName);

            if (cons.hasParam()) {
                boolean tmp = fromInit;
                fromInit = false;
                List<String> parameters = new ArrayList<>();
                for (PureExp param : cons.getParams()) {
                    StringWriter psw = new StringWriter();
                    ScalaWriter pw = new ScalaWriter(psw);
                    pw.continuationLevel = w.continuationLevel;
                    pw.duplicateReplacements = w.duplicateReplacements;

                    param.accept(this, pw);
                    parameters.add(psw.toString());
                }


                String result = String.format("%s(%s)", functionName, String.join(COMMA_SPACE, parameters));
                fromInit = tmp;
                w.emit(result);
            } else {

                String resolvedType = javaTypeTranslator.translateFunctionalType(functionName);
                resolvedType = translateDataColl(resolvedType);

                String terminator = resolvedType.equals("false") || resolvedType.equals("true")
                        || resolvedType.equals("None") ? "" : "()";
                // List<String> polyC = polyTypeConstructs.get(refinedType);
                if (fromEquals || fromInit) {

                    StringWriter psw = new StringWriter();
                    ScalaWriter tw = new ScalaWriter(psw);
                    List<Type> argList = null;
                    Type t = cons.getType();
                    if (t.isDataType()) {
                        DataTypeType dt = (DataTypeType) t;
                        argList = dt.getTypeArgs();
                    }

                    if (argList != null && !argList.isEmpty()) {
                        StringBuilder genType = new StringBuilder("[");
                        for (Type arg : argList
                                ) {
                            StringWriter argsw = new StringWriter();
                            if (arg.toUse() != null) {
                                arg.toUse().accept(this, new ScalaWriter(argsw));
                                genType.append(argsw.toString() + ",");
                            } else if (arg.isBoundedType()) {
                                Type bt = ((BoundedType) arg).getBoundType();
                                if (bt != null && bt.toUse() != null) {
                                    bt.toUse().accept(this, new ScalaWriter(argsw));
                                    genType.append(argsw.toString() + ",");
                                }
                            } else {
                                genType.append(arg.getSimpleName() + ",");
                            }
                        }

                        int x = genType.lastIndexOf(",");
                        if (x > -1)
                            genType.replace(x, x + 1, "]");
                        if (genType.length() == 1) {
                            if (initType != null && initType.length() > 0) {
                                int index = initType.indexOf('[');
                                if (index > -1)
                                    genType.append(initType.substring(index + 1));
                            }
                        }
                        if (genType.length() > 1)
                            w.emit(resolvedType + genType + terminator);
                        else
                            w.emit(resolvedType + terminator);
                    } else
                        w.emit(resolvedType + terminator);
                } else
                    w.emit(resolvedType + terminator);
            }
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }

    public void visit(ParFnApp cons, ScalaWriter w) {

    }

    public void visit(FnApp cons, ScalaWriter w) {
        try {

            String pack = cons.getDecl().getModuleDecl().getName();
            StringBuilder functionN = new StringBuilder(cons.getName());

            if (!pack.equals("ABS.StdLib"))
                functionN.insert(0, pack + ".Functions.");

            String functionName = functionN.toString();
            List<String> parameters = new ArrayList<>();
            for (PureExp param : cons.getParams()) {
                StringWriter psw = new StringWriter();
                ScalaWriter pw = new ScalaWriter(psw);
                pw.continuationLevel = w.continuationLevel;
                pw.duplicateReplacements = w.duplicateReplacements;
                param.accept(this, pw);
                parameters.add(psw.toString());
            }
            // if (fromCase)
            // caseKey = findMethodReturnType(name, "Functions", params);
            w.emit(functionName.equals("toString") ? "cwi.Functions.toString" : functionName);
            w.emit("(");
            if (cons.hasParam()) {
                w.emit(String.join(COMMA_SPACE, parameters));
            }
            w.emit((functionName.equals("ABS.DC.Functions.thisDC") ? "this" : "") +
                    ")" /*+ (functionName.equals("toString") ? ".toString()" : "")*/);

        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }

    public void visit(IfExp p, ScalaWriter w) {
        try {
            w.beginExpressionGroup();

            StringWriter sw = new StringWriter();
            ScalaWriter w1 = new ScalaWriter(sw);
            w1.continuationLevel = w.continuationLevel;
            w1.duplicateReplacements = w.duplicateReplacements;
            p.getCondExp().accept(this, w1);
            String condition = sw.toString();

            sw = new StringWriter();
            ScalaWriter w2 = new ScalaWriter(sw);
            w2.continuationLevel = w.continuationLevel;
            w2.duplicateReplacements = w.duplicateReplacements;
            p.getThenExp().accept(this, w2);
            String left = sw.toString();

            sw = new StringWriter();
            ScalaWriter w3 = new ScalaWriter(sw);
            w3.continuationLevel = w.continuationLevel;
            w3.duplicateReplacements = w.duplicateReplacements;
            p.getElseExp().accept(this, w3);
            String right = sw.toString();

            w.emit(String.format("if (%s)  %s else %s", condition, left, right));
            w.endExpressionGroup();
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }

    public void visit(LetExp e, ScalaWriter w) {
        try {
            w.beginExpressionGroup();
            StringWriter typesw = new StringWriter();
            ScalaWriter tw = new ScalaWriter(typesw);

            String name = javaTypeTranslator.translateKeyword(e.getVar().getName());
            e.getVar().getAccess().accept(this, tw);
            String type = typesw.toString();

            StringWriter sw = new StringWriter();
            ScalaWriter w1 = new ScalaWriter(sw);
            w1.continuationLevel = w.continuationLevel;
            w1.duplicateReplacements = w.duplicateReplacements;
            e.getVal().accept(this, w1);
            String assign = sw.toString();

            sw = new StringWriter();
            ScalaWriter w2 = new ScalaWriter(sw);
            w2.continuationLevel = w.continuationLevel;
            w2.duplicateReplacements = w.duplicateReplacements;
            e.getExp().accept(this, w2);
            String right = sw.toString();

            w.emit(String.format("{val %s:%s = %s\n  %s}", name, type, assign, right));
            w.endExpressionGroup();
        } catch (IOException ex) {
            throw new RuntimeException(ex);
        }
    }

    public void visit(CaseExp p, ScalaWriter w) {
        try {
            w.beginExpressionGroup();
            StringWriter auxsw = new StringWriter();
            ScalaWriter auxw = new ScalaWriter(auxsw);
            auxw.continuationLevel = w.continuationLevel;
            auxw.duplicateReplacements = w.duplicateReplacements;

            if (!variablesBeforeBlock.isEmpty()) {
                for (VarDefinition vd : variablesBeforeBlock) {
                    if ((w.continuationLevel > -1 || w.avoidDuplicates)) {
                        w.duplicateReplacements.peek().put(vd.getName(), "w_" + awaitCounter + "$" + vd.getName());
                    }

                    if (!variablesInScope.isEmpty()) {
                        variablesInScope.peek().add(vd);
                    }
                }
                variablesBeforeBlock.clear();
            }
            p.getExpr().accept(this, auxw);
            w.beginControlFlow("%s match ", auxsw);
            for (CaseBranch scb : p.getBranchs()) {
                StringWriter pattersw = new StringWriter();
                ScalaWriter patternw = new ScalaWriter(pattersw);
                patternw.continuationLevel = w.continuationLevel;
                patternw.duplicateReplacements = w.duplicateReplacements;

                scb.accept(this, patternw);

                w.emit(pattersw.toString());

            }

            w.endControlFlow();
            w.endExpressionGroup();
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }

    public void visit(CaseBranch p, ScalaWriter w) {
        try {
            StringWriter patsw = new StringWriter();
            ScalaWriter patw = new ScalaWriter(patsw);
            patw.continuationLevel = w.continuationLevel;
            patw.duplicateReplacements = w.duplicateReplacements;
            p.getLeft().accept(this, patw);

            StringWriter stmsw = new StringWriter();
            ScalaWriter stmw = new ScalaWriter(stmsw);
            stmw.continuationLevel = w.continuationLevel;
            stmw.duplicateReplacements = w.duplicateReplacements;

            p.getRight().accept(this, stmw);

            w.emitStatement("case %s => %s", patsw.toString(), stmsw.toString());
            variablesBeforeBlock.clear();
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }

    public void visit(PatternVarUse p, ScalaWriter w) {
        try {

            if ((w.continuationLevel >= -1 || w.avoidDuplicates))
                w.emit("`w_" + awaitCounter + "$" + p.getName() + "`");
            else
                w.emit("`" + getDuplicate(javaTypeTranslator.translateKeyword(p.getName()), w) + "`");

        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }

    public void visit(PatternVar p, ScalaWriter w) {
        try {

            StringWriter psw = new StringWriter();
            ScalaWriter tw = new ScalaWriter(psw);

            Type t = p.getVar().getType();
            t.toUse().accept(this, tw);
            String varType = psw.toString();

            String varName = javaTypeTranslator.translateKeyword(p.getVar().getName());
            VarDefinition vd = createVarDefinition(varName, varType);
            variablesBeforeBlock.add(vd);
            if ((w.continuationLevel >= -1 || w.avoidDuplicates)) {
                w.emit("w_" + awaitCounter + "$" + varName);
                w.duplicateReplacements.peek().put(varName, "w_" + awaitCounter + "$" + varName);
            } else
                w.emit(getDuplicate(varName, w));

        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }

    public void visit(ConstructorPattern p, ScalaWriter w) {
        try {
            String name = javaTypeTranslator.translateFunctionalType(p.getConstructor());
            name = translateDataColl(name);

            if (p.hasParam()) {
                List<String> parameters = new ArrayList<>();
                for (Pattern pattern : p.getParams()) {
                    StringWriter sw = new StringWriter();
                    ScalaWriter auxw = new ScalaWriter(sw);
                    auxw.continuationLevel = w.continuationLevel;
                    auxw.duplicateReplacements = w.duplicateReplacements;
                    pattern.accept(this, auxw);
                    parameters.add(sw.toString());

                }
                String params = String.join(COMMA_SPACE, parameters);
                w.emit(name + "(" + params + ")");
            } else {
                String terminator = name.equals("false") || name.equals("true") || name.equals("None") ? "" : "()";
                w.emit(name + terminator);
            }
        } catch (IOException e) {
            throw new RuntimeException(e);
        }

    }

    public void visit(LiteralPattern literalPattern, ScalaWriter w) {
        literalPattern.getLiteral().accept(this, w);
    }

    public void visit(UnderscorePattern underscorePattern, ScalaWriter w) {
        try {
            w.emit("_");
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }

    /* Literal Expressions */

    public void visit(IntLiteral p, ScalaWriter w) {
        try {
            w.emit(Long.toString(Long.valueOf(p.getContent())));
        } catch (IOException x) {
            throw new RuntimeException(x);
        }
    }

    public void visit(FloatLiteral p, ScalaWriter w) {
        try {
            w.emit(Float.toString(Float.valueOf(p.getContent())));
        } catch (IOException x) {
            throw new RuntimeException(x);
        }
    }

    public void visit(StringLiteral s, ScalaWriter w) {
        try {

            w.emit("\"" + s.getContent() + "\"");

        } catch (IOException x) {
            throw new RuntimeException(x);
        }
    }

    /* Unary and Binary */

    public void visit(MinusExp e, ScalaWriter w) {
        try {
            w.beginExpressionGroup();
            Type t = e.getType();
            Type lt = e.getOperand().getType();
            StringWriter lsw = new StringWriter();
            ScalaWriter lw = new ScalaWriter(lsw);
            lw.continuationLevel = w.continuationLevel;
            lw.duplicateReplacements = w.duplicateReplacements;
            e.getOperand().accept(this, lw);
            if (t.getQualifiedName().contains("Rat")) {
                if (lt.getQualifiedName().contains("Int")) {
                    w.emit(String.format(" - new Rational(%s)", lsw));
                } else {
                    w.emit(" - ");
                    e.getOperand().accept(this, w);
                }
            } else {
                /*if(lt.getQualifiedName().contains("Int")){
                    if(rt.getQualifiedName().contains("Int"))
                        w.emit(String.format("new Rational(%s,%s)", lsw,rsw));
                        else
                        w.emit(String.format("new Rational(%s)/%s", lsw,rsw));
                }
                else{
                    if(rt.getQualifiedName().contains("Int"))
                        w.emit(String.format("%s/new Rational(%s)", lsw,rsw));
                    else
                        w.emit(String.format("%s/%s", lsw,rsw));
                }*/
                w.emit(" - ");
                e.getOperand().accept(this, w);
            }
            w.endExpressionGroup();
        } catch (IOException x) {
            throw new RuntimeException(x);
        }

    }

    public void visit(NegExp e, ScalaWriter w) {
        try {
            w.beginExpressionGroup();
            w.emit(" ! ");
            e.getOperand().accept(this, w);
            w.endExpressionGroup();
        } catch (IOException x) {
            throw new RuntimeException(x);
        }

    }

    public void visit(MultMultExp e, ScalaWriter w) {
        try {
            w.beginExpressionGroup();
            Type t = e.getType();
            Type lt = e.getLeft().getType();
            Type rt = e.getRight().getType();
            StringWriter lsw = new StringWriter();
            ScalaWriter lw = new ScalaWriter(lsw);
            lw.continuationLevel = w.continuationLevel;
            lw.duplicateReplacements = w.duplicateReplacements;
            StringWriter rsw = new StringWriter();
            ScalaWriter rw = new ScalaWriter(rsw);
            rw.continuationLevel = w.continuationLevel;
            rw.duplicateReplacements = w.duplicateReplacements;
            e.getLeft().accept(this, lw);
            e.getRight().accept(this, rw);
            if (t.getQualifiedName().contains("Rat")) {
                if (lt.getQualifiedName().contains("Int")) {
                    w.emit(String.format("new Rational(%s)*%s", lsw, rsw));
                } else {
                    e.getLeft().accept(this, w);
                    w.emit(" * ");
                    e.getRight().accept(this, w);
                }
            } else {
                /*if(lt.getQualifiedName().contains("Int")){
                    if(rt.getQualifiedName().contains("Int"))
                        w.emit(String.format("new Rational(%s,%s)", lsw,rsw));
                        else
                        w.emit(String.format("new Rational(%s)/%s", lsw,rsw));
                }
                else{
                    if(rt.getQualifiedName().contains("Int"))
                        w.emit(String.format("%s/new Rational(%s)", lsw,rsw));
                    else
                        w.emit(String.format("%s/%s", lsw,rsw));
                }*/
                e.getLeft().accept(this, w);
                w.emit(" * ");
                e.getRight().accept(this, w);
            }
            w.endExpressionGroup();
        } catch (IOException x) {
            throw new RuntimeException(x);
        }

    }

    public void visit(DivMultExp e, ScalaWriter w) {
        try {
            w.beginExpressionGroup();
            Type t = e.getType();
            Type lt = e.getLeft().getType();
            Type rt = e.getRight().getType();
            StringWriter lsw = new StringWriter();
            ScalaWriter lw = new ScalaWriter(lsw);
            lw.continuationLevel = w.continuationLevel;
            lw.duplicateReplacements = w.duplicateReplacements;
            StringWriter rsw = new StringWriter();
            ScalaWriter rw = new ScalaWriter(rsw);
            rw.continuationLevel = w.continuationLevel;
            rw.duplicateReplacements = w.duplicateReplacements;
            e.getLeft().accept(this, lw);
            e.getRight().accept(this, rw);
            if (t.getQualifiedName().contains("Rat")) {
                if (lt.getQualifiedName().contains("Int")) {
                    w.emit(String.format("new Rational(%s)/%s", lsw, rsw));
                } else {
                    e.getLeft().accept(this, w);
                    w.emit(" / ");
                    e.getRight().accept(this, w);
                }
            } else {
                /*if(lt.getQualifiedName().contains("Int")){
                    if(rt.getQualifiedName().contains("Int"))
                        w.emit(String.format("new Rational(%s,%s)", lsw,rsw));
                        else
                        w.emit(String.format("new Rational(%s)/%s", lsw,rsw));
                }
                else{
                    if(rt.getQualifiedName().contains("Int"))
                        w.emit(String.format("%s/new Rational(%s)", lsw,rsw));
                    else
                        w.emit(String.format("%s/%s", lsw,rsw));
                }*/
                e.getLeft().accept(this, w);
                w.emit(" / ");
                e.getRight().accept(this, w);
            }
            w.endExpressionGroup();
        } catch (IOException x) {
            throw new RuntimeException(x);
        }

    }

    public void visit(ModMultExp e, ScalaWriter w) {
        try {
            w.beginExpressionGroup();
            e.getLeft().accept(this, w);
            w.emit(" % ");
            e.getRight().accept(this, w);
            w.endExpressionGroup();
        } catch (IOException x) {
            throw new RuntimeException(x);
        }

    }

    public void visit(AddAddExp e, ScalaWriter w) {
        try {
            w.beginExpressionGroup();
            Type t = e.getType();
            Type lt = e.getLeft().getType();
            Type rt = e.getRight().getType();
            StringWriter lsw = new StringWriter();
            ScalaWriter lw = new ScalaWriter(lsw);
            lw.continuationLevel = w.continuationLevel;
            lw.duplicateReplacements = w.duplicateReplacements;
            StringWriter rsw = new StringWriter();
            ScalaWriter rw = new ScalaWriter(rsw);
            rw.continuationLevel = w.continuationLevel;
            rw.duplicateReplacements = w.duplicateReplacements;
            e.getLeft().accept(this, lw);
            e.getRight().accept(this, rw);
            if (t.getQualifiedName().contains("Rat")) {
                if (lt.getQualifiedName().contains("Int")) {
                    w.emit(String.format("new Rational(%s) + %s", lsw, rsw));
                } else {
                    e.getLeft().accept(this, w);
                    w.emit(" + ");
                    e.getRight().accept(this, w);
                }
            } else {
                /*if(lt.getQualifiedName().contains("Int")){
                    if(rt.getQualifiedName().contains("Int"))
                        w.emit(String.format("new Rational(%s,%s)", lsw,rsw));
                        else
                        w.emit(String.format("new Rational(%s)/%s", lsw,rsw));
                }
                else{
                    if(rt.getQualifiedName().contains("Int"))
                        w.emit(String.format("%s/new Rational(%s)", lsw,rsw));
                    else
                        w.emit(String.format("%s/%s", lsw,rsw));
                }*/
                e.getLeft().accept(this, w);
                w.emit(" + ");
                e.getRight().accept(this, w);
            }
            w.endExpressionGroup();
        } catch (IOException x) {
            throw new RuntimeException(x);
        }

    }

    public void visit(SubAddExp e, ScalaWriter w) {
        try {
            w.beginExpressionGroup();
            Type t = e.getType();
            Type lt = e.getLeft().getType();
            Type rt = e.getRight().getType();
            StringWriter lsw = new StringWriter();
            ScalaWriter lw = new ScalaWriter(lsw);
            lw.continuationLevel = w.continuationLevel;
            lw.duplicateReplacements = w.duplicateReplacements;
            StringWriter rsw = new StringWriter();
            ScalaWriter rw = new ScalaWriter(rsw);
            rw.continuationLevel = w.continuationLevel;
            rw.duplicateReplacements = w.duplicateReplacements;
            e.getLeft().accept(this, lw);
            e.getRight().accept(this, rw);
            if (t.getQualifiedName().contains("Rat")) {
                if (lt.getQualifiedName().contains("Int")) {
                    w.emit(String.format("new Rational(%s) - %s", lsw, rsw));
                } else {
                    e.getLeft().accept(this, w);
                    w.emit(" - ");
                    e.getRight().accept(this, w);
                }
            } else {
                /*if(lt.getQualifiedName().contains("Int")){
                    if(rt.getQualifiedName().contains("Int"))
                        w.emit(String.format("new Rational(%s,%s)", lsw,rsw));
                        else
                        w.emit(String.format("new Rational(%s)/%s", lsw,rsw));
                }
                else{
                    if(rt.getQualifiedName().contains("Int"))
                        w.emit(String.format("%s/new Rational(%s)", lsw,rsw));
                    else
                        w.emit(String.format("%s/%s", lsw,rsw));
                }*/
                e.getLeft().accept(this, w);
                w.emit(" - ");
                e.getRight().accept(this, w);
            }
            w.endExpressionGroup();
        } catch (IOException x) {
            throw new RuntimeException(x);
        }

    }

    public void constructList(List<PureExp> rest, ScalaWriter w) throws IOException {
        if (rest.isEmpty())
            w.emit("Nil()");
        else {
            w.emit("Cons");
            w.beginExpressionGroup();
            PureExp element = rest.remove(0);
            element.accept(this, w);
            w.emit(", ");
            constructList(rest, w);
            w.endExpressionGroup();
        }
    }

    public void visit(ListLiteral e, ScalaWriter w) {
        try {
            List<PureExp> contents = new LinkedList<>();
            for (PureExp element : e.getPureExpList()
                    ) {
                contents.add(element);
            }
            constructList(contents, w);
        } catch (IOException x) {
            throw new RuntimeException(x);
        }
    }

    public void visit(AndBoolExp e, ScalaWriter w) {
        try {
            w.beginExpressionGroup();
            e.getLeft().accept(this, w);
            w.emit(" && ");
            e.getRight().accept(this, w);
            w.endExpressionGroup();
        } catch (IOException x) {
            throw new RuntimeException(x);
        }

    }

    public void visit(OrBoolExp e, ScalaWriter w) {
        try {
            w.beginExpressionGroup();
            e.getLeft().accept(this, w);
            w.emit(" || ");
            e.getRight().accept(this, w);
            w.endExpressionGroup();
        } catch (IOException x) {
            throw new RuntimeException(x);
        }

    }

    public void visit(LTExp e, ScalaWriter w) {
        try {
            w.beginExpressionGroup();
            Type t = e.getType();
            Type lt = e.getLeft().getType();
            Type rt = e.getRight().getType();
            //if(!t.getQualifiedName().contains("Rat")) {
            e.getLeft().accept(this, w);
            w.emit(" < ");
            e.getRight().accept(this, w);
            /*}
            else{
                StringWriter lsw = new StringWriter();
                ScalaWriter lw = new ScalaWriter(lsw);
                lw.continuationLevel = w.continuationLevel;
                lw.duplicateReplacements = w.duplicateReplacements;
                StringWriter rsw = new StringWriter();
                ScalaWriter rw = new ScalaWriter(rsw);
                rw.continuationLevel = w.continuationLevel;
                rw.duplicateReplacements = w.duplicateReplacements;
                e.getLeft().accept(this,lw);
                e.getRight().accept(this, rw);
                if(lt.getQualifiedName().contains("Int")){
                    if(rt.getQualifiedName().contains("Int"))
                        w.emit(String.format("new Rational(%s,%s)", lsw,rsw));
                    else
                        w.emit(String.format("new Rational(%s)<%s", lsw,rsw));
                }
                else{
                    if(rt.getQualifiedName().contains("Int"))
                        w.emit(String.format("%s<new Rational(%s)", lsw,rsw));
                    else
                        w.emit(String.format("%s<%s", lsw,rsw));
                }
            }*/
            w.endExpressionGroup();
        } catch (IOException x) {
            throw new RuntimeException(x);
        }

    }

    public void visit(GTExp e, ScalaWriter w) {
        try {
            w.beginExpressionGroup();
            Type t = e.getType();
            Type lt = e.getLeft().getType();
            Type rt = e.getRight().getType();
            //if(!t.getQualifiedName().contains("Rat")) {
            e.getLeft().accept(this, w);
            w.emit(" > ");
            e.getRight().accept(this, w);
            /*}
            else{
                StringWriter lsw = new StringWriter();
                ScalaWriter lw = new ScalaWriter(lsw);
                lw.continuationLevel = w.continuationLevel;
                lw.duplicateReplacements = w.duplicateReplacements;
                StringWriter rsw = new StringWriter();
                ScalaWriter rw = new ScalaWriter(rsw);
                rw.continuationLevel = w.continuationLevel;
                rw.duplicateReplacements = w.duplicateReplacements;
                e.getLeft().accept(this,lw);
                e.getRight().accept(this, rw);
                if(lt.getQualifiedName().contains("Int")){
                    if(rt.getQualifiedName().contains("Int"))
                        w.emit(String.format("new Rational(%s,%s)", lsw,rsw));
                    else
                        w.emit(String.format("new Rational(%s)>%s", lsw,rsw));
                }
                else{
                    if(rt.getQualifiedName().contains("Int"))
                        w.emit(String.format("%s>new Rational(%s)", lsw,rsw));
                    else
                        w.emit(String.format("%s>%s", lsw,rsw));
                }
            }*/
            w.endExpressionGroup();
        } catch (IOException x) {
            throw new RuntimeException(x);
        }

    }

    public void visit(LTEQExp e, ScalaWriter w) {
        try {
            w.beginExpressionGroup();
            Type t = e.getType();
            Type lt = e.getLeft().getType();
            Type rt = e.getRight().getType();
            //if(!t.getQualifiedName().contains("Rat")) {
            e.getLeft().accept(this, w);
            w.emit(" <= ");
            e.getRight().accept(this, w);
            /*}
            else{
                StringWriter lsw = new StringWriter();
                ScalaWriter lw = new ScalaWriter(lsw);
                lw.continuationLevel = w.continuationLevel;
                lw.duplicateReplacements = w.duplicateReplacements;
                StringWriter rsw = new StringWriter();
                ScalaWriter rw = new ScalaWriter(rsw);
                rw.continuationLevel = w.continuationLevel;
                rw.duplicateReplacements = w.duplicateReplacements;
                e.getLeft().accept(this,lw);
                e.getRight().accept(this, rw);
                if(lt.getQualifiedName().contains("Int")){
                    if(rt.getQualifiedName().contains("Int"))
                        w.emit(String.format("new Rational(%s,%s)", lsw,rsw));
                    else
                        w.emit(String.format("new Rational(%s)<=%s", lsw,rsw));
                }
                else{
                    if(rt.getQualifiedName().contains("Int"))
                        w.emit(String.format("%s<=new Rational(%s)", lsw,rsw));
                    else
                        w.emit(String.format("%s<=%s", lsw,rsw));
                }
            }*/
            w.endExpressionGroup();
        } catch (IOException x) {
            throw new RuntimeException(x);
        }

    }

    public void visit(GTEQExp e, ScalaWriter w) {
        try {
            w.beginExpressionGroup();
            Type t = e.getType();
            Type lt = e.getLeft().getType();
            Type rt = e.getRight().getType();
            //if(!t.getQualifiedName().contains("Rat")) {
            e.getLeft().accept(this, w);
            w.emit(" >= ");
            e.getRight().accept(this, w);
            /*}
            else{
                StringWriter lsw = new StringWriter();
                ScalaWriter lw = new ScalaWriter(lsw);
                lw.continuationLevel = w.continuationLevel;
                lw.duplicateReplacements = w.duplicateReplacements;
                StringWriter rsw = new StringWriter();
                ScalaWriter rw = new ScalaWriter(rsw);
                rw.continuationLevel = w.continuationLevel;
                rw.duplicateReplacements = w.duplicateReplacements;
                e.getLeft().accept(this,lw);
                e.getRight().accept(this, rw);
                if(lt.getQualifiedName().contains("Int")){
                    if(rt.getQualifiedName().contains("Int"))
                        w.emit(String.format("new Rational(%s,%s)", lsw,rsw));
                    else
                        w.emit(String.format("new Rational(%s)>=%s", lsw,rsw));
                }
                else{
                    if(rt.getQualifiedName().contains("Int"))
                        w.emit(String.format("%s>=new Rational(%s)", lsw,rsw));
                    else
                        w.emit(String.format("%s>=%s", lsw,rsw));
                }
            }*/
            w.endExpressionGroup();
        } catch (IOException x) {
            throw new RuntimeException(x);
        }

    }

    public void visit(EqExp e, ScalaWriter w) {
        try {
            w.beginExpressionGroup();
            StringWriter auxsw = new StringWriter();
            ScalaWriter auxw = new ScalaWriter(auxsw);
            auxw.continuationLevel = w.continuationLevel;
            auxw.duplicateReplacements = w.duplicateReplacements;

            fromEquals = true;
            e.getLeft().accept(this, auxw);
            String firstArg = auxsw.toString();
            StringWriter auxsw2 = new StringWriter();
            ScalaWriter auxw2 = new ScalaWriter(auxsw2);
            auxw2.continuationLevel = w.continuationLevel;
            auxw2.duplicateReplacements = w.duplicateReplacements;

            e.getRight().accept(this, auxw2);
            String secondArg = auxsw2.toString();
            w.emit(String.format("Objects.equals(%s, %s)", firstArg, secondArg));
            w.endExpressionGroup();
            fromEquals = false;
        } catch (IOException x) {
            throw new RuntimeException(x);
        }
    }

    public void visit(NotEqExp e, ScalaWriter w) {
        try {
            w.beginExpressionGroup();
            StringWriter auxsw = new StringWriter();
            ScalaWriter auxw = new ScalaWriter(auxsw);
            auxw.continuationLevel = w.continuationLevel;
            auxw.duplicateReplacements = w.duplicateReplacements;

            fromEquals = true;
            e.getLeft().accept(this, auxw);
            String firstArg = auxsw.toString();
            StringWriter auxsw2 = new StringWriter();
            ScalaWriter auxw2 = new ScalaWriter(auxsw2);
            auxw2.continuationLevel = w.continuationLevel;
            auxw2.duplicateReplacements = w.duplicateReplacements;

            e.getRight().accept(this, auxw2);
            String secondArg = auxsw2.toString();
            w.emit(String.format("!Objects.equals(%s, %s)", firstArg, secondArg));
            w.endExpressionGroup();
            fromEquals = false;
        } catch (IOException x) {
            throw new RuntimeException(x);
        }

    }


    public void visit(InterfaceTypeUse e, ScalaWriter w) {
        try {
            String name = javaTypeTranslator.apply(e.getName());
            w.emit(name);
        } catch (IOException e1) {
            // TODO Auto-generated catch block
            e1.printStackTrace();
        }
    }

    public void visit(DataTypeUse e, ScalaWriter w) {
        try {
            String name = javaTypeTranslator.apply(e.getName());
            w.emit(name);
        } catch (IOException e1) {
            // TODO Auto-generated catch block
            e1.printStackTrace();
        }
    }

    public void visit(ParametricDataTypeUse e, ScalaWriter w) {
        try {

            String name = javaTypeTranslator.apply(e.getName());

            List<String> p = new ArrayList<>();

            for (TypeUse string : e.getParams()) {
                StringWriter auxsw2 = new StringWriter();
                ScalaWriter auxw2 = new ScalaWriter(auxsw2);
                auxw2.continuationLevel = w.continuationLevel;
                auxw2.duplicateReplacements = w.duplicateReplacements;
                string.accept(this, auxw2);
                p.add(auxsw2.toString());

            }
            w.emit(name + (p.isEmpty() ? "" : p));
        } catch (IOException e1) {
            // TODO Auto-generated catch block
            e1.printStackTrace();
        }
    }

    public void visit(TypeParameterUse e, ScalaWriter w) {
        try {
            String name = javaTypeTranslator.apply(e.getName());
            w.emit(name);
        } catch (IOException e1) {
            // TODO Auto-generated catch block
            e1.printStackTrace();
        }
    }

    public void visit(VarUse e, ScalaWriter w) {
        try {
            String x = getDuplicate(e.getName(), w);
            w.emit(javaTypeTranslator.translateKeyword(x));
        } catch (IOException e1) {
            // TODO Auto-generated catch block
            e1.printStackTrace();
        }
    }

    public void visit(FieldUse e, ScalaWriter w) {
        try {
            String convertedkw = javaTypeTranslator.translateKeyword(e.getName());
            if (!fromSupplier)
                w.emit(LITERAL_THIS + "." + convertedkw);
            else
                w.emit(convertedkw);

        } catch (IOException e1) {
            // TODO Auto-generated catch block
            e1.printStackTrace();
        }
    }

    /* UNIMIPLEMENTED VISITS */

    public void visit(ExceptionDecl e, ScalaWriter w) {

    }

    public void visit(PartialFunctionDecl e, ScalaWriter w) {

    }


    public void visit(SkipStmt e, ScalaWriter w) {

    }

    public void visit(SuspendStmt e, ScalaWriter w) {

    }

    public void visit(DurationStmt e, ScalaWriter w) {

    }

    public void visit(ForeachStmt e, ScalaWriter w) {

    }

    public void visit(TryCatchFinallyStmt e, ScalaWriter w) {
        //TODO: Java try catch
        //TODO: look for the recovery block in class decl.
    }

    public void visit(ThrowStmt e, ScalaWriter w) {
        //TODO: Java throw

    }

    public void visit(DieStmt e, ScalaWriter w) {

    }

    public void visit(UnknownDecl e, ScalaWriter w) {

    }

    public void visit(DataConstructor e, ScalaWriter w) {

    }

    public void visit(TypeParameterDecl e, ScalaWriter w) {
        String adtName = e.getName();
        String typeName = e.getType().getQualifiedName();
        StringWriter sw = new StringWriter();
        ScalaWriter tw = new ScalaWriter(sw);
        if (e.getType().toUse() != null) {
            e.getType().toUse().accept(this, tw);
            typeName = sw.toString();
        }
        this.javaTypeTranslator.registerAbstractType(adtName, typeName);
    }

    /*  public void visit(GetLocExp e, ScalaWriter w) {

      }

      public void visit(GetFatherExp e, ScalaWriter w) {

      }

      public void visit(IncompleteAsyncAccess e, ScalaWriter w) {

      }

      public void visit(IncompleteNewExp e, ScalaWriter w) {

      }

      public void visit(IncompleteStmt e, ScalaWriter w) {

      }

      public void visit(IncompleteSyncAccess e, ScalaWriter w) {

      }

  */
    public void visit(OriginalCall e, ScalaWriter w) {

    }

    public void visit(BuiltinFunctionDef e, ScalaWriter w) {
        //TODO: Scheduler builtins (manual 14.2)

    }

    public void visit(MoveCogToStmt e, ScalaWriter w) {

    }

    /*    public void visit(MoveStmt e, ScalaWriter w) {

        }

        public void visit(NewLocExp e, ScalaWriter w) {

        }

        public void visit(ObjectGuard e, ScalaWriter w) {

        }

        public void visit(RebindStmt e, ScalaWriter w) {

        }
    */
    public void visit(TraitDecl e, ScalaWriter w) {

    }

    public void visit(UnresolvedTypeUse e, ScalaWriter w) {

    }

    /******************/
    public List<ScalaWriter> continuation(org.abs_models.frontend.ast.List<Stmt> list, List<ScalaWriter> currentMethodWriters,
                                          boolean isInMethod) {

        // System.out.println("Entering continuation creation with awaitCounter
        // " + awaitCounter + "for method "
        // + currentMethod.getName());

        TreeSet<VarDefinition> methodScope = new TreeSet<>();
        variablesInScope.push(methodScope);

        if (!variablesBeforeBlock.isEmpty()) {
            for (VarDefinition vd : variablesBeforeBlock) {
                if (!variablesInScope.isEmpty()) {
                    variablesInScope.peek().add(vd);
                }
            }
            variablesBeforeBlock.clear();
        }

        StringWriter scopesw = new StringWriter();
        ScalaWriter scopew = new ScalaWriter(scopesw);
        scopew.isScope = true;

        for (Stmt stm : list) {

            Stmt as = stm;

            int tmp = awaitCounter;
            int tmpS = syncPCounter;
            int tmpA = asyncPCounter;


            for (ScalaWriter javaWriter : currentMethodWriters) {
                LinkedList<TreeSet<VarDefinition>> oldVarsinScope = new LinkedList<>();
                for (TreeSet<VarDefinition> node :
                        variablesInScope) {
                    oldVarsinScope.add(new TreeSet<>(node));
                }

                if (!javaWriter.avoiddc)
                    as.accept(this, javaWriter);

                variablesInScope = oldVarsinScope;
                awaitCounter = tmp;
                syncPCounter = tmpS;
                asyncPCounter = tmpA;
            }

            tmp = awaitCounter;
            tmpS = syncPCounter;
            tmpA = asyncPCounter;

            as.accept(this, scopew);

            awaitCounter = tmp;
            //syncPCounter = tmpS;
            //asyncPCounter = tmpA;

            if (as instanceof WhileStmt) {
                WhileStmt sw = (WhileStmt) as;
                Block whileBlock = sw.getBody();

//                System.out.println("while in"+currentMethod.getName());
//                System.out.println(sw.getCondition().toString());

                List<ScalaWriter> innerAwaits = continuation(whileBlock.getStmts(), new LinkedList<>(), false);

                int tmp2 = awaitCounter;
                int tmpS2 = syncPCounter;
                int tmpA2 = asyncPCounter;

                awaitCounter = tmp;
                syncPCounter = tmpS;
                asyncPCounter = tmpA;

                for (ScalaWriter javaWriter : innerAwaits) {

                    tmp = awaitCounter;
                    tmpS = syncPCounter;
                    tmpA = asyncPCounter;
                    as.accept(this, javaWriter);
                    awaitCounter = tmp;
                    syncPCounter = tmpS;
                    asyncPCounter = tmpA;

                }

                currentMethodWriters.addAll(innerAwaits);

                awaitCounter = tmp2;
                syncPCounter = tmpS2;
                asyncPCounter = tmpA2;

                /*
                 * for (JavaWriter javaWriter : currentMethodWriters) {
                 * visitLabelWhile(sw, javaWriter); }
                 */
                // System.out.println("after while check " + awaitCounter);

            }

            // System.out.println(awaitCounter);

            if (as instanceof Block) {
                List<ScalaWriter> innerAwaits1 = continuation(((Block) as).getStmts(), new LinkedList<>(), false);
                currentMethodWriters.addAll(innerAwaits1);
            }

            if (as instanceof IfStmt) {
                IfStmt sie = (IfStmt) as;
                Block ifBlock = sie.getThen();
                Block elseBlock = sie.getElse();


                List<ScalaWriter> innerAwaits1 = continuation(ifBlock.getStmts(), new LinkedList<>(), false);
                currentMethodWriters.addAll(innerAwaits1);

                if (sie.hasElse()) {
                    List<ScalaWriter> innerAwaits2 = continuation(elseBlock.getStmts(), new LinkedList<>(), false);

                    currentMethodWriters.addAll(innerAwaits2);
                }

                /*
                 * for (JavaWriter javaWriter : currentMethodWriters) {
                 * visitLabelWhile(sw, javaWriter); }
                 */
                // System.out.println("after while check " + awaitCounter);

            }

            if (as instanceof CaseStmt) {
                // System.out.println("before while check " + awaitCounter);
                CaseStmt sc = (CaseStmt) as;

                StringWriter auxsw = new StringWriter();
                ScalaWriter auxw = new ScalaWriter(auxsw);

                sc.getExpr().accept(this, auxw);

                // caseKey = findVariableTypeInScope(auxsw.toString());

                for (CaseBranchStmt lcb : sc.getBranchList()) {

                    Block caseBlock = lcb.getRight();
                    // String oldKey = caseKey;
                    lcb.getLeft().accept(this, scopew);

                    // TreeSet<VarDefinition> blockScope = new TreeSet<>();
                    // variablesInScope.push(blockScope);

                    List<ScalaWriter> innerAwaits1 = continuation(caseBlock.getStmts(), new LinkedList<>(), false);

                    // variablesInScope.pop();

                    currentMethodWriters.addAll(innerAwaits1);
                    // caseKey = oldKey;

                }
                // caseKey = "";

                /*
                 * for (JavaWriter javaWriter : currentMethodWriters) {
                 * visitLabelWhile(sw, javaWriter); }
                 */
                // System.out.println("after while check " + awaitCounter);

            }

            if (as instanceof AwaitStmt) {

                StringBuilder label = new StringBuilder(classes.peek());
                label.append(currentMethod.getName());
                label.append("Await" + (awaitCounter));

                StringWriter auxsw = new StringWriter();
                ScalaWriter auxw = new ScalaWriter(auxsw);
                auxw.continuationLevel = -1;

                try {
                    String returnType = currentMethod.type() != null ? currentMethod.type() : "void";
                    List<String> parameters = new ArrayList<>();
                    for (TreeSet<VarDefinition> defs : variablesInScope) {
                        for (VarDefinition varDefinition : defs) {
                            parameters.add(varDefinition.getType());

                            parameters.add(getDuplicate(varDefinition.getName(), auxw));
                        }
                    }
                    startContinuation(label, auxw, returnType, parameters);

                } catch (IOException e) {
                    // TODO Auto-generated catch block
                    e.printStackTrace();
                }
                currentMethodLabels.add(auxsw);
                currentMethodWriters.add(auxw);
                awaitCounter++;
            }

//            if(withDC){
//                PureExp cost = CompilerUtils.getAnnotationValueFromName(as.getAnnotationList(), "ABS.DC.Cost");
//                if (cost != null) {
//                    StringBuilder label = new StringBuilder(classes.peek());
//                    label.append(currentMethod.getName());
//                    label.append("Await" + (awaitCounter));
//
//                    StringWriter auxsw = new StringWriter();
//                    ScalaWriter auxw = new ScalaWriter(auxsw);
//                    auxw.continuationLevel = -1;
//
//                    try {
//                        String returnType = currentMethod.type() != null ? currentMethod.type() : "void";
//                        List<String> parameters = new ArrayList<>();
//                        for (TreeSet<VarDefinition> defs : variablesInScope) {
//                            for (VarDefinition varDefinition : defs) {
//                                parameters.add(varDefinition.getType());
//
//                                parameters.add(getDuplicate(varDefinition.getName(), auxw));
//                            }
//                        }
//                        startContinuation(label, auxw, returnType, parameters);
//
//                    } catch (IOException e) {
//                        // TODO Auto-generated catch block
//                        e.printStackTrace();
//                    }
//                    currentMethodLabels.add(auxsw);
//                    currentMethodWriters.add(auxw);
//                    awaitCounter++;
//                }
//            }

            if (as instanceof ExpressionStmt) {
                ExpressionStmt exp = (ExpressionStmt) as;
                if (exp.getExp() instanceof EffExp) {
                    EffExp ee = (EffExp) exp.getExp();
                    if (ee instanceof SyncCall) {

                        SyncCall smc = (SyncCall) ee;
                        String methodName = smc.getMethod().equals(METHOD_GET) ? "get" : smc.getMethod();
                        if (!getCalleeId(smc, new ScalaWriter(new StringWriter())).equals(LITERAL_THIS) || callHasAwait(smc, methodName)) {
                            StringBuilder label = new StringBuilder(classes.peek());
                            label.append(currentMethod.getName());
                            label.append("Await" + (awaitCounter));

                            StringWriter auxsw = new StringWriter();
                            ScalaWriter auxw = new ScalaWriter(auxsw);
                            auxw.continuationLevel = -1;
                            try {
                                String returnType = currentMethod.type() != null ? currentMethod.type() : "void";
                                List<String> parameters = new ArrayList<>();
                                for (TreeSet<VarDefinition> defs : variablesInScope) {
                                    for (VarDefinition varDefinition : defs) {
                                        parameters.add(varDefinition.getType());
                                        parameters.add(getDuplicate(varDefinition.getName(), auxw));
                                    }
                                }

                                // if (smcReturnType != null) {
                                // parameters.add("ABSFutureTask[Any]");
                                // parameters.add("f_par");
                                // }

                                startContinuation(label, auxw, returnType, parameters);

                            } catch (IOException e) {
                                // TODO Auto-generated catch block
                                e.printStackTrace();
                            }
                            currentMethodLabels.add(auxsw);
                            currentMethodWriters.add(auxw);
                            awaitCounter++;
                        }

                    }

                    if (ee instanceof GetExp) {
                        GetExp g = (GetExp) ee;
                        StringWriter guardsw = new StringWriter();
                        ScalaWriter guardw = new ScalaWriter(guardsw);
                        g.getPureExp().accept(this, guardw);

                        if (!guardsw.toString().startsWith("tmp")) {

                            StringBuilder label = new StringBuilder(classes.peek());
                            label.append(currentMethod.getName());
                            label.append("Await" + (awaitCounter));
                            StringWriter auxsw = new StringWriter();
                            ScalaWriter auxw = new ScalaWriter(auxsw);
                            auxw.continuationLevel = -1;

                            try {
                                String returnType = currentMethod.type() != null ? currentMethod.type() : "void";
                                List<String> parameters = new ArrayList<>();
                                for (TreeSet<VarDefinition> defs : variablesInScope) {
                                    for (VarDefinition varDefinition : defs) {
                                        parameters.add(varDefinition.getType());

                                        parameters.add(getDuplicate(varDefinition.getName(), auxw));
                                    }
                                }
                                startContinuation(label, auxw, returnType, parameters);

                            } catch (IOException e) {
                                // TODO Auto-generated catch block
                                e.printStackTrace();
                            }
                            currentMethodLabels.add(auxsw);
                            currentMethodWriters.add(auxw);
                            awaitCounter++;
                        }
                    }
                }
            }

            // TODO assignment SDecAss, SFieldAss, Sass
            if (as instanceof AssignStmt) {
                AssignStmt sas = (AssignStmt) as;

                if (sas.getValue() instanceof EffExp) {

                    EffExp ee = (EffExp) sas.getValue();

                    StringBuilder varName = new StringBuilder("");
                    varName.append(javaTypeTranslator.translateKeyword(sas.getVar().getName()));

                    String methodName = null;
                    if (ee instanceof SyncCall) {
                        SyncCall smc = (SyncCall) ee;

                        methodName = smc.getMethod().equals(METHOD_GET) ? "get" : smc.getMethod();

                        if (!getCalleeId(smc, new ScalaWriter(new StringWriter())).equals(LITERAL_THIS) || callHasAwait(smc, methodName)) {

                            StringBuilder label = new StringBuilder(classes.peek());
                            label.append(currentMethod.getName());
                            label.append("Await" + (awaitCounter));

                            StringBuilder futureName = new StringBuilder();
                            futureName.append("future_");
                            futureName.append(varName.toString());
                            StringWriter auxsw = new StringWriter();
                            ScalaWriter auxw = new ScalaWriter(auxsw);
                            auxw.continuationLevel = -1;

                            try {
                                String returnType = currentMethod.type() != null ? currentMethod.type() : "void";
                                List<String> parameters = new ArrayList<>();
                                for (TreeSet<VarDefinition> defs : variablesInScope) {
                                    for (VarDefinition varDefinition : defs) {
                                        parameters.add(varDefinition.getType());
                                        parameters.add(getDuplicate(varDefinition.getName(), auxw));
                                    }
                                }

                                StringWriter tsw = new StringWriter();
                                ScalaWriter tw = new ScalaWriter(tsw);
                                sas.getVar().getType().toUse().accept(this, tw);

                                parameters.add(String.format("%s", tsw.toString()));
                                parameters.add(futureName.toString());

                                startContinuation(label, auxw, returnType, parameters);

                                auxw.emitStatement("%s = %s", getDuplicate(varName.toString(), auxw), futureName);

                            } catch (IOException e) {
                                // TODO Auto-generated catch block
                                e.printStackTrace();
                            }
                            currentMethodLabels.add(auxsw);
                            currentMethodWriters.add(auxw);
                            awaitCounter++;
                        }

                    }

                    if (ee instanceof NewExp) {
                        NewExp ne = (NewExp) ee;
                        String qname = ne.getType().getQualifiedName();
                        if (checkAwait(CONSTRUCTOR, qname)) {

                            StringBuilder label = new StringBuilder(classes.peek());
                            label.append(currentMethod.getName());
                            label.append("Await" + (awaitCounter));

                            StringBuilder futureName = new StringBuilder();
                            futureName.append("future_");
                            futureName.append(varName.toString());
                            StringWriter auxsw = new StringWriter();
                            ScalaWriter auxw = new ScalaWriter(auxsw);
                            auxw.continuationLevel = -1;

                            try {
                                String returnType = currentMethod.type() != null ? currentMethod.type() : "void";
                                List<String> parameters = new ArrayList<>();
                                for (TreeSet<VarDefinition> defs : variablesInScope) {
                                    for (VarDefinition varDefinition : defs) {
                                        parameters.add(varDefinition.getType());
                                        parameters.add(getDuplicate(varDefinition.getName(), auxw));
                                    }
                                }

//                                StringWriter tsw = new StringWriter();
//                                ScalaWriter tw = new ScalaWriter(tsw);
//                                sas.getVar().getType().toUse().accept(this, tw);
//
//                                parameters.add(String.format("%s", tsw.toString()));
//                                parameters.add(futureName.toString());
//
                                startContinuation(label, auxw, returnType, parameters);

                                //auxw.emitStatement("%s = %s", getDuplicate(varName.toString(), auxw), futureName);

                            } catch (IOException e) {
                                // TODO Auto-generated catch block
                                e.printStackTrace();
                            }
                            currentMethodLabels.add(auxsw);
                            currentMethodWriters.add(auxw);
                            awaitCounter++;
                        }
                    }


                    if (ee instanceof GetExp) {

                        StringWriter getsw = new StringWriter();
                        ScalaWriter getw = new ScalaWriter(getsw);
                        ((GetExp) ee).getPureExp().accept(this, getw);

                        if (!getsw.toString().startsWith("tmp")) {
                            StringBuilder label = new StringBuilder(classes.peek());
                            label.append(currentMethod.getName());
                            label.append("Await" + (awaitCounter));
                            StringWriter auxsw = new StringWriter();
                            ScalaWriter auxw = new ScalaWriter(auxsw);
                            auxw.continuationLevel = -1;

                            try {
                                String returnType = currentMethod.type() != null ? currentMethod.type() : "void";
                                List<String> parameters = new ArrayList<>();
                                for (TreeSet<VarDefinition> defs : variablesInScope) {
                                    for (VarDefinition varDefinition : defs) {
                                        parameters.add(varDefinition.getType());

                                        parameters.add(getDuplicate(varDefinition.getName(), auxw));
                                    }
                                }

                                StringBuilder valueName = new StringBuilder(getsw.toString());
                                valueName.append("value");
                                StringWriter tsw = new StringWriter();
                                ScalaWriter tw = new ScalaWriter(tsw);
                                sas.getVar().getType().toUse().accept(this, tw);
                                String valueType = tsw.toString();

                                parameters.add(String.format("%s", tsw.toString()));
                                parameters.add(valueName.toString());

                                startContinuation(label, auxw, returnType, parameters);

                                auxw.emitStatement("%s = %s", getDuplicate(varName.toString(), auxw), valueName);
                                parameters.add(valueType);
                                parameters.add(valueName.toString());

                                // parameters.add(String.format("ABSFutureTask[%s]",
                                // findVariableTypeInScope(sas.l_)));
                                // parameters.add(getsw.toString());

                                startContinuation(label, auxw, returnType, parameters);

                                // auxw.emitStatement("%s = %s.get()", sas.l_,
                                // getsw);

                            } catch (IOException e) {
                                // TODO Auto-generated catch block
                                e.printStackTrace();
                            }
                            currentMethodLabels.add(auxsw);
                            currentMethodWriters.add(auxw);
                            awaitCounter++;
                        }
                    }

                }
            }

            if (as instanceof VarDeclStmt) {
                VarDecl das = ((VarDeclStmt) as).getVarDecl();
                if (das.hasInitExp() && (das.getInitExp() instanceof EffExp)) {
                    EffExp ee = (EffExp) das.getInitExp();
                    String methodName = null;

                    StringBuilder varName = new StringBuilder("");
                    varName.append(javaTypeTranslator.translateKeyword(das.getName()));

                    if (ee instanceof SyncCall) {
                        SyncCall smc = (SyncCall) ee;
                        methodName = smc.getMethod().equals(METHOD_GET) ? "get" : smc.getMethod();
                        if (!getCalleeId(smc, new ScalaWriter(new StringWriter())).equals(LITERAL_THIS) || callHasAwait(smc, methodName)) {


                            StringBuilder label = new StringBuilder(classes.peek());
                            label.append(currentMethod.getName());
                            label.append("Await" + (awaitCounter));

                            StringWriter auxsw = new StringWriter();
                            ScalaWriter auxw = new ScalaWriter(auxsw);
                            auxw.continuationLevel = -1;

                            StringBuilder futureName = new StringBuilder();
                            futureName.append("future_");
                            futureName.append(varName.toString());

                            try {
                                String returnType = currentMethod.type() != null ? currentMethod.type() : "void";
                                List<String> parameters = new ArrayList<>();
                                for (TreeSet<VarDefinition> defs : variablesInScope) {
                                    for (VarDefinition varDefinition : defs) {
                                        if (!varDefinition.getName()
                                                .equals(javaTypeTranslator.translateKeyword(das.getName()))) {
                                            parameters.add(varDefinition.getType());
                                            parameters.add(getDuplicate(varDefinition.getName(), auxw));
                                        }
                                    }
                                }

                                StringWriter tsw = new StringWriter();
                                ScalaWriter tw = new ScalaWriter(tsw);
                                das.getAccess().accept(this, tw);

                                parameters.add(String.format("%s", tsw.toString()));
                                parameters.add(futureName.toString());

                                startContinuation(label, auxw, returnType, parameters);
                                auxw.emitStatement("var %s : %s = %s", varName, tsw.toString(), futureName);

                            } catch (IOException e) {
                                // TODO Auto-generated catch block
                                e.printStackTrace();
                            }
                            currentMethodLabels.add(auxsw);
                            currentMethodWriters.add(auxw);
                            awaitCounter++;
                        }
                    }

                    if (ee instanceof NewExp) {
                        NewExp ne = (NewExp) ee;
                        String qname = ne.getType().getQualifiedName();
                        if (checkAwait(CONSTRUCTOR, qname)) {

                            StringBuilder label = new StringBuilder(classes.peek());
                            label.append(currentMethod.getName());
                            label.append("Await" + (awaitCounter));

                            StringBuilder futureName = new StringBuilder();
                            futureName.append("future_");
                            futureName.append(varName.toString());
                            StringWriter auxsw = new StringWriter();
                            ScalaWriter auxw = new ScalaWriter(auxsw);
                            auxw.continuationLevel = -1;

                            try {
                                String returnType = currentMethod.type() != null ? currentMethod.type() : "void";
                                List<String> parameters = new ArrayList<>();
                                for (TreeSet<VarDefinition> defs : variablesInScope) {
                                    for (VarDefinition varDefinition : defs) {
                                        parameters.add(varDefinition.getType());
                                        parameters.add(getDuplicate(varDefinition.getName(), auxw));
                                    }
                                }

//                                StringWriter tsw = new StringWriter();
//                                ScalaWriter tw = new ScalaWriter(tsw);
//                                das.getAccess().accept(this, tw);
//
//                                parameters.add(String.format("%s", tsw.toString()));
//                                parameters.add(futureName.toString());

                                startContinuation(label, auxw, returnType, parameters);

                                //auxw.emitStatement("%s = %s", getDuplicate(varName.toString(), auxw), futureName);

                            } catch (IOException e) {
                                // TODO Auto-generated catch block
                                e.printStackTrace();
                            }
                            currentMethodLabels.add(auxsw);
                            currentMethodWriters.add(auxw);
                            awaitCounter++;
                        }
                    }

                    if (ee instanceof GetExp) {

                        StringWriter getsw = new StringWriter();
                        ScalaWriter getw = new ScalaWriter(getsw);
                        ((GetExp) ee).getPureExp().accept(this, getw);

                        if (!getsw.toString().startsWith("tmp")) {

                            StringBuilder label = new StringBuilder(classes.peek());
                            label.append(currentMethod.getName());
                            label.append("Await" + (awaitCounter));
                            StringWriter auxsw = new StringWriter();
                            ScalaWriter auxw = new ScalaWriter(auxsw);
                            auxw.continuationLevel = -1;
                            try {
                                String returnType = currentMethod.type() != null ? currentMethod.type() : "void";
                                List<String> parameters = new ArrayList<>();
                                for (TreeSet<VarDefinition> defs : variablesInScope) {
                                    for (VarDefinition varDefinition : defs) {
                                        if (!varDefinition.getName()
                                                .equals(javaTypeTranslator.translateKeyword(das.getName()))) {
                                            parameters.add(varDefinition.getType());
                                            parameters.add(getDuplicate(varDefinition.getName(), auxw));
                                        }
                                    }
                                }
                                StringBuilder valueName = new StringBuilder(getsw.toString());
                                valueName.append("value");

                                StringWriter tsw = new StringWriter();
                                ScalaWriter tw = new ScalaWriter(tsw);
                                das.getAccess().accept(this, tw);

                                String valueType = tsw.toString();
                                parameters.add(valueType);
                                parameters.add(valueName.toString());
                                // parameters.add(String.format("ABSFutureTask[%s]",
                                // getTypeName(das.t_)));
                                // parameters.add(getsw.toString());

                                startContinuation(label, auxw, returnType, parameters);

                                auxw.emitStatement("var %s : %s = %s", varName, valueType, valueName);

                            } catch (IOException e) {
                                // TODO Auto-generated catch block
                                e.printStackTrace();
                            }
                            currentMethodLabels.add(auxsw);
                            currentMethodWriters.add(auxw);
                            awaitCounter++;

                        }
                    }
                }
            }
        }
        variablesInScope.pop();

        if (isInMethod)

        {
            for (ScalaWriter javaWriter : currentMethodWriters) {
                try {

                    javaWriter.endMethod();
                    javaWriter.close();
                } catch (IOException e) {
                    // TODO Auto-generated catch block
                    e.printStackTrace();
                }
            }
        } else {
            for (ScalaWriter javaWriter : currentMethodWriters) {
                javaWriter.continuationLevel = variablesInScope.size();
            }
        }
        return currentMethodWriters;

    }

    public void visitMethodBody(Block mcb, ScalaWriter notNeeded) {

        TreeMap<Integer, Stmt> annHandlers = new TreeMap<>(new Comparator<Integer>() {
            @Override
            public int compare(Integer integer, Integer t1) {
                return t1 - integer;
            }
        });
        for (Stmt annStm : mcb.getStmts()) {
            if (withDC && firstDCPass == 0) {
                PureExp cost = AnnotationHelper.getAnnotationValueFromName(annStm.getAnnotationList(), "ABS.DC.Cost");
                if (cost != null) {
                    if (!currentMethod.containsAwait()) {
                        currentMethod.setContainsAwait(true);
                        awaitsDetected = true;
                    }
                    List<PureExp> pars = new LinkedList<>();
                    DataConstructorExp rtype = new DataConstructorExp("Speed", new org.abs_models.frontend.ast.List<>());
                    pars.add(cost.treeCopyNoTransform());
                    pars.add(rtype);

                    //Call acquire = AbsASTBuilderUtil.getCall(AbsASTBuilderUtil.getFnApp("thisDC"), "acquireResource", false, cost.treeCopyNoTransform(),rtype);
                    AwaitAsyncCall aws = new AwaitAsyncCall(AbsASTBuilderUtil.getFnApp("thisDC"), "acquireResource", ListUtils.toASTList(pars));
                    Stmt s = AbsASTBuilderUtil.getExpStmt(aws);
                    annHandlers.put(mcb.getStmts().getIndexOfChild(annStm), s);
                }
            }
            annStm.accept(this, notNeeded);

        }
        if (withDC && firstDCPass == 0) {
            for (Integer k : annHandlers.keySet()
                    ) {
                mcb.getStmts().insertChild(annHandlers.get(k), k);
            }
        }
    }

    protected void visitFunctions(ModuleDecl m, ScalaWriter w) {
        try {
            for (Decl decl : elements.get(AbsElementType.FUNCTION)) {
                decl.accept(this, functionsWriter);
            }
            functionsWriter.endType();

            /*
             * List<String> pairPar = new ArrayList<>(); pairPar.add("F");
             * pairPar.add("first"); pairPar.add("S"); pairPar.add("second");
             *
             * beginElementKind(jw, ElementKind.CLASS, "Pair[F,S]",
             * DEFAULT_MODIFIERS, null, null, pairPar, false); jw.endType();
             *
             * Set<Modifier> mods = new HashSet<>(DEFAULT_MODIFIERS);
             * mods.add(Modifier.ABSTRACT);
             *
             * beginElementKind(jw, ElementKind.CLASS, "Maybe[T]", mods, null,
             * null); jw.endType();
             *
             * beginElementKind(jw, ElementKind.OTHER, "Nothing[T]",
             * DEFAULT_MODIFIERS, "Maybe[T]", null, new ArrayList<>(), false);
             *
             * jw.endType();
             *
             * pairPar.clear(); pairPar.add("T"); pairPar.add("member");
             *
             * beginElementKind(jw, ElementKind.OTHER, "Just[T]",
             * DEFAULT_MODIFIERS, "Maybe[T]", null, pairPar, false);
             *
             * jw.endType();nn//
             */

        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }

    protected void visitMain(ModuleDecl m, ScalaWriter w) {
        try {
            ScalaWriter jw = (ScalaWriter) javaWriterSupplier.apply(MAIN_CLASS_NAME);
            jw.emitPackage(packageName);
            // emitDefaultImports(jw);

            visitImports(m.getImports(), jw);
            jw.emitEmptyLine();

            final String identifier = MAIN_CLASS_NAME;
            EnumSet<Modifier> mainModifiers = EnumSet.of(Modifier.PUBLIC, Modifier.STATIC);
            beginElementKind(jw, ElementKind.CONSTRUCTOR, identifier, DEFAULT_MODIFIERS, ABS_API_ACTOR_CLASS, null,
                    null, true);
            classes.push("Main");
            // labelMap.put("Main", new LinkedList<>());

            jw.emitEmptyLine();

            emitImplicitConversions(jw);
            List<String> javaMainMethodParameters = new ArrayList<>();

            jw.beginMethod(String.format("%s[%s]", ABSFUTURE_CLASS, VOID_WRAPPER_CLASS_NAME), "mainMessage",
                    mainModifiers, javaMainMethodParameters, null);
            createMethodDefinition(String.format("%s[%s]", ABSFUTURE_CLASS, VOID_WRAPPER_CLASS_NAME), "mainMessage",
                    new LinkedList<>());

            // jw.emitStatement("Context context =
            // Configuration.newConfiguration().buildContext()");
            // emitThisActorRegistration(jw);
            jw.emitEmptyLine();
            jw.emitSingleLineComment("Init section: %s", this.packageName);

            //this.setDc(new ClassDeploymentComponent(null, null, "Main", EmptyMap[Resourcetype,Rational]));
            TreeSet<VarDefinition> blockScope = new TreeSet<>();
            variablesInScope.push(blockScope);

            awaitCounter = 0;
            asyncPCounter = 0;
            syncPCounter = 0;

            // jw.emitStatement("%s.init()",
            // DeploymentComponent.class.getName());

            if (withDC)
                jw.emitStatement("this.%s(new %s(%s,%s, %s, EmptyMap[Resourcetype,Rational]));\n", SET_DC, CLASS_DC, LITERAL_NULL, LITERAL_NULL, "\"Main\"");

            m.getBlock().accept(this, jw);

            jw.emitEmptyLine();
            jw.emit("", true);
            jw.emitStatementEnd();
            // jw.emitStatement("context.stop()");

            variablesInScope.pop();

            awaitCounter = 0;
            asyncPCounter = 0;
            syncPCounter = 0;

            Block mainBlock = m.getBlock();
            org.abs_models.frontend.ast.List<Stmt> copyOfMcb = mainBlock.getStmtList();
            System.out.println("Building continuations for Main");
            continuation(copyOfMcb, new LinkedList<>(), true);
            // visitStatementsBlock(mcb.listannstm_, w);
            // variablesInScope.pop();
            System.out.println("Done continuations for Main");
            variablesInScope.clear();

            jw.endMethod();
            for (StringWriter stringWriter : currentMethodLabels) {
                jw.emit(stringWriter.toString());

            }
            currentMethodLabels.clear();

            javaMainMethodParameters = Arrays.asList("Array[String]", "args");
            jw.beginMethod("Unit", "main", mainModifiers, javaMainMethodParameters, null);
            jw.emitStatement("var msg: Callable[ABSFuture[Void]] =  ()=>this.mainMessage");
            jw.emitStatement("this.send(msg)");

            jw.endMethod();

            // for (StringWriter continuation : labelMap.get("Main")) {
            // w.emit(continuation.toString());
            // }

            jw.endType();
            classes.pop();

            // labelMap.remove("Main");
            close(jw, w);
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }

    protected void visitStatementAssignmentExp(Exp exp, String varName, String varType, ScalaWriter w)
            throws IOException {

        StringWriter auxsw = new StringWriter();
        ScalaWriter auxw = new ScalaWriter(auxsw);
        auxw.continuationLevel = w.continuationLevel;
        auxw.duplicateReplacements = w.duplicateReplacements;
        if (exp instanceof PureExp) {
            PureExp pe = (PureExp) exp;
            if (pe instanceof DataConstructorExp)
                if (varType != null)
                    initType = varType;
        }
        exp.accept(this, auxw);

        varName = getDuplicate(varName, w);
        initType = "";
        if (varType == null) {
            w.emit(varName + " = " + auxsw.toString(), true);
            w.emitStatementEnd();
        } else {

            w.emitField(varType, varName, new HashSet<>(), auxsw.toString());
        }

        if (exp instanceof EffExp) {
            EffExp ee = (EffExp) exp;
            if (ee instanceof NewExp) {
                NewExp ne = (NewExp) ee;

                Stmt stmt = CompilerUtils.findStmtForExpression(ne);
                boolean isNewDC = ne.getType().isDeploymentComponentType();

//                if(isNewDC&&withDC){
//                    w.emitStatement("%s.%s(%s.asInstanceOf[%s])", TAS, ADD_DC, varName, CLASS_DC);
//                }

                String qname = ne.getType().getQualifiedName();
                if (checkAwait(CONSTRUCTOR, qname)) {

                    StringBuilder label = new StringBuilder(classes.peek());
                    label.append(currentMethod.getName());

                    if (!w.isScope && !w.checkAwaits) {
                        label.append("Await" + (awaitCounter));
                    }

                    StringBuilder valueName = new StringBuilder("unusedValue: Void");

                    String methodCall = generateContinuationMethodInvocation("this", label.toString(), w, 'w', awaitCounter, true);
//                    StringBuilder mc = new StringBuilder(methodCall);
//                    if(mc.lastIndexOf("(")+1==mc.indexOf(")"))
//                        mc.insert(mc.indexOf(")"), valueName);
//                    else
//                        mc.insert(mc.indexOf(")"), ","+valueName);

                    try {
                        //w.emitStatement("var %s: %s=>%s = (%s)=>%s", label + "m", type, currentMethod.type(), valueName,
                        //       methodCall);
                        if (fromConstructor)
                            w.emitStatement("%s.%s = getSpawn(%s.%s, (%s)=>%s, %s.HIGH_PRIORITY, false)", LITERAL_THIS, CONS_FUT, varName, GET_CONSTRUCTOR_FUTURE, valueName, methodCall,
                                    ABS_API_INTERFACE_CLASS);
                        else

                            w.emitStatement("return getSpawn(%s.%s, (%s)=>%s, %s.HIGH_PRIORITY, false)", varName, GET_CONSTRUCTOR_FUTURE, valueName, methodCall,
                                    ABS_API_INTERFACE_CLASS);
                        w.avoiddc = true;
                        if (!w.isScope && !w.checkAwaits) {
                            awaitCounter++;
                        }
                    } catch (IOException e) {
                        // TODO Auto-generated catch block
                        e.printStackTrace();
                    }
                }
            }
        }

        w.emitStatementEnd();
    }

    protected void visitAsyncMethodCall(AsyncCall amc, String resultVarType, String resultVarName,
                                        final boolean isDefined, ScalaWriter w) throws IOException {
        String calleeId = getCalleeId(amc, w);

        String methodName = amc.getMethod().equals(METHOD_GET) ? "get" : amc.getMethod();

        // System.out.println("ASYNC CALL:" + resultVarType);

        String potentialReturnType = resultVarType != null
                ? resultVarType.substring(resultVarType.indexOf("[") + 1, resultVarType.lastIndexOf("]"))
                : amc.getType().getSimpleName();

        String msgVarName = createMessageVariableName(calleeId);
        String msgStatement = generateMessageStatement(msgVarName, potentialReturnType,
                generateJavaMethodInvocation(calleeId, methodName, amc.getParams(), w, 'a', asyncPCounter));
        w.emit(msgStatement, true);
        w.emitStatementEnd();
        String responseVarName = resultVarName != null ? resultVarName : createMessageResponseVariableName(msgVarName);

        String sendStm = generateMessageInvocationStatement(calleeId, isDefined, resultVarType, msgVarName,
                responseVarName, w);
        w.emit(sendStm, true);
        w.emitStatementEnd();
        if (!w.checkAwaits)
            asyncPCounter++;

    }

    protected void visitGet(GetExp g, String resultVarType, String resultVarName, final boolean isDefined,
                            ScalaWriter w) {

        if (w.checkAwaits && !currentMethod.containsAwait()) {
            currentMethod.setContainsAwait(true);
            awaitsDetected = true;
        }
        StringWriter auxsw = new StringWriter();
        ScalaWriter auxw = new ScalaWriter(auxsw);
        auxw.continuationLevel = w.continuationLevel;
        auxw.duplicateReplacements = w.duplicateReplacements;
        g.getPureExp().accept(this, auxw);

        if (!auxsw.toString().contains("tmp")) {

            StringBuilder valueName = new StringBuilder(resultVarName);
            int index = valueName.indexOf(".");
            if (index > 0)
                valueName.deleteCharAt(index);

            StringBuilder label = new StringBuilder(classes.peek());
            label.append(currentMethod.getName());

            if (!w.isScope && !w.checkAwaits) {
                label.append("Await" + (awaitCounter++));
            }

            String methodCall = generateContinuationMethodInvocation("this", label.toString(), w, 'w', awaitCounter, true);
            StringBuilder actualawaitcall = new StringBuilder(methodCall);
            actualawaitcall.insert(actualawaitcall.indexOf(")"), ", " + valueName);
            String ret = currentMethod.type();
            String cgrt = ret.substring(ret.indexOf("[") + 1, ret.lastIndexOf("]"));
            try {
                w.emitStatement("var %s: CallableGet[%s, %s] = (%s)=>%s", label + "m", cgrt, resultVarType, valueName,
                        actualawaitcall);
                w.emitStatement("return getSpawn(%s, %s, %s.HIGH_PRIORITY, true)", auxsw.toString(), label + "m",
                        ABS_API_INTERFACE_CLASS);
                w.avoiddc = true;
            } catch (IOException e) {
                // TODO Auto-generated catch block
                e.printStackTrace();
            }
        } else {
            try {
                if (isDefined) {
                    w.emitStatement("%s = %s.%s", resultVarName, auxsw, SYNC_GET);
                } else {
                    w.emitStatement("var %s : %s = %s.%s", resultVarName, resultVarType, auxsw, SYNC_GET);
                }

            } catch (IOException e) {
                // TODO Auto-generated catch block
                e.printStackTrace();
            }
        }

    }

    protected void visitSyncMethodCall_Sync(SyncCall smc, String resultVarType, String resultVarName, boolean isDefined,
                                            ScalaWriter w) throws IOException {
        String calleeId = getCalleeId(smc, w);
        List<String> params = new ArrayList<>();

        for (PureExp par : smc.getParams()) {
            StringWriter parSW = new StringWriter();
            ScalaWriter parameterWriter = new ScalaWriter(parSW);
            parameterWriter.continuationLevel = w.continuationLevel;
            parameterWriter.duplicateReplacements = w.duplicateReplacements;

            par.accept(this, parameterWriter);
            // if (par instanceof LiteralExp)
            params.add(parSW.toString());
            // else
            // params.add(parSW.toString() + "pureexp");
        }
        String methodName = smc.getMethod().equals(METHOD_GET) ? "get" : smc.getMethod();

        if (w.checkAwaits) {
            if (((callHasAwait(smc, methodName)) || (!calleeId.equals(LITERAL_THIS))) && !currentMethod.containsAwait()) {
                currentMethod.setContainsAwait(true);
                awaitsDetected = true;
            }
        } else {

            StringWriter tsw = new StringWriter();
            ScalaWriter tw = new ScalaWriter(tsw);
            smc.getType().toUse().accept(this, tw);

            String methodRet = tsw.toString();
            String potentialReturnType = String.format("%s[%s]", ABSFUTURE_CLASS, methodRet);
            String msgVar = createMessageVariableName(calleeId);

            resultVarName = getDuplicate(resultVarName, w);

            String javaMethodCall = String.format("%s.%s(%s)", calleeId, methodName, String.join(COMMA_SPACE, params));

            StringBuilder label = new StringBuilder(classes.peek());
            label.append(currentMethod.getName());

            //if (methodName.equals("addElement") && currentMethod.getName().equals("run"))
            //    System.out.println(methodName + " ca " + callHasAwait(smc, methodName));

//            if(fromConstructor){
//                if (resultVarName != null) {
//                    if (isDefined)
//                        w.emitStatement("%s = %s.%s", resultVarName, javaMethodCall, SYNC_GET);
//                    else
//                        w.emitStatement("var %s : %s = %s.%s", resultVarName, resultVarType, javaMethodCall, SYNC_GET);
//
//                } else
//                    w.emitStatement(javaMethodCall);
//                return;
//            }

            if (!w.isScope && !w.checkAwaits) {
                label.append("Await" + (awaitCounter));
            }

            if (!calleeId.equals(LITERAL_THIS))
                w.beginControlFlow("if(%s.sameCog(%s))", calleeId, LITERAL_THIS);

            if (callHasAwait(smc, methodName)) {

                w.emitStatement("var %s : %s = %s", msgVar, potentialReturnType, javaMethodCall);

            } else {
                if (resultVarName != null) {
                    if (isDefined)
                        w.emitStatement("%s = %s.%s", resultVarName, javaMethodCall, SYNC_GET);
                    else
                        w.emitStatement("var %s : %s = %s.%s", resultVarName, resultVarType, javaMethodCall, SYNC_GET);

                } else
                    w.emit(javaMethodCall, true);
            }

            w.emitStatementEnd();
            if (callHasAwait(smc, methodName)) {
                String awaitCall = generateContinuationMethodInvocation("this", label.toString(), w, 'w', awaitCounter, true);

                visitSyncMethodCall_Async(msgVar, methodRet, resultVarName, w, awaitCall, false);
            } else {
                if (!calleeId.equals(LITERAL_THIS)) {

                    String awaitCall = generateContinuationMethodInvocation("this", label.toString(), w, 'w', awaitCounter, false);
                    StringBuilder actualawaitcall = new StringBuilder(awaitCall);
                    int l = awaitCall.indexOf("(");
                    int r = awaitCall.indexOf(")");
                    if (resultVarName != null)
                        actualawaitcall.insert(actualawaitcall.indexOf(")"), (awaitCall.substring(l, r).length() > 1 ? ", " : "") + resultVarName);
                    if (fromConstructor)
                        w.emitStatement("%s = %s", CONS_FUT, actualawaitcall);
                    else
                        w.emitStatement("return %s", actualawaitcall);
                }
            }
            if (!calleeId.equals(LITERAL_THIS)) {
                w.endControlFlow();
                w.beginControlFlow("else");

                String awaitCall = generateContinuationMethodInvocation("this", label.toString(), w, 'w', awaitCounter, true);

                //String futName = resultVarName + "f";
                String message = String.format("()=>%s", javaMethodCall);
                String sendStatement = generateMessageInvocationStatement(calleeId, true, potentialReturnType, message, msgVar, w);
                w.emitStatement(sendStatement);
                visitSyncMethodCall_Async(msgVar, methodRet, resultVarName, w, awaitCall, true);


                w.endControlFlow();
            }
            if (!calleeId.equals(LITERAL_THIS) || callHasAwait(smc, methodName)) {
                awaitCounter++;
            }
        }
    }

    protected void visitSyncMethodCall_Async(String possibleF, String resultVarType, String resultVarName,
                                             ScalaWriter w, String awaitCall, boolean cog) throws IOException {

        // TODO : complete sync->async
        // String calleeId = getCalleeId(smc, w);
        // List<String> params = new ArrayList<>();
        // for (PureExp par : smc.getParams()) {
        // StringWriter parSW = new StringWriter();
        // ScalaWriter parameterWriter = new ScalaWriter(parSW);
        // parameterWriter.continuationLevel = w.continuationLevel;
        // parameterWriter.duplicateReplacements = w.duplicateReplacements;
        //
        // par.accept(this, parameterWriter);
        // if (par instanceof LiteralExp)
        // params.add(parSW.toString());
        // else
        // params.add(parSW.toString() + "pureexp");
        // }
        // String msgVarName = createMessageVariableName(calleeId);
        // String methodName = smc.getMethod().equals(METHOD_GET) ? "get" :
        // smc.getMethod();


        // System.out.println("RV = " + resultVarName);
        StringBuilder futureName = new StringBuilder();
        if (!(resultVarName == null || resultVarName.length() == 0)) {
            futureName.append("future_");
            futureName.append(resultVarName);
            int index = futureName.indexOf(".");
            if (index > 0)
                futureName.deleteCharAt(index);
        }


        StringBuilder extraP = new StringBuilder();
        if (possibleF != null || cog) {
            if (!(resultVarName == null || resultVarName.length() == 0)) {
                extraP.append(futureName.toString() + "_par: " + (resultVarType == null ? "Void" : resultVarType));

            } else {
                extraP.append(possibleF.toString() + "_par: " + (resultVarType == null ? "Void" : resultVarType));

            }

            String extraPName = extraP.substring(0, extraP.indexOf(":"));

            StringBuilder actualawaitcall = new StringBuilder(awaitCall);
            int l = awaitCall.indexOf("(");
            int r = awaitCall.indexOf(")");
            if (resultVarName != null) {
                actualawaitcall.insert(actualawaitcall.indexOf(")"), (awaitCall.substring(l, r).length() > 1 ? ", " : "") + extraPName);
            }

            //
            // if ((w.continuationLevel > -1 || w.avoidDuplicates)) {
            // w.duplicateReplacements.peek().put(futureName.toString(),
            // "w_" + awaitCounter + "$" + futureName.toString());
            //
            // }

            // String syncCall = generateJavaMethodInvocation(calleeId, methodName,
            // smc.getParams(), w, 's', syncPCounter);

            // String awaitCall = generateContinuationMethodInvocation("this",
            // label.toString(), w, 'w', awaitCounter);

            // System.out.println(awaitCall);

            // String msgStatement = generateMessageStatement(msgVarName,
            // potentialReturnType, syncCall);
            // w.emit(msgStatement, true);
            // w.emitStatementEnd();

            String getSpawnStm;
            getSpawnStm = generateMessageSyncInvocationStatement("this", possibleF, "(" + extraP + ")=>" + actualawaitcall, cog);

            w.emit(getSpawnStm, true);
            w.emitStatementEnd();

            w.avoiddc = true;
        }
    }

    protected void visitImports(final org.abs_models.frontend.ast.List<Import> list, ScalaWriter w) throws IOException {
        emitDefaultImports(w);
        w.emitStaticImports(this.staticImports);
        for (Import import1 : list) {
            import1.accept(this, w);
        }
        w.emitEmptyLine();
    }

    protected void emitDefaultImports(ScalaWriter w) throws IOException {
        // w.emitStaticImports(DEFAULT_STATIC_IMPORTS_PATTERNS);
        w.emitImports(DEFAULT_IMPORTS);
        w.emitStaticImports(DEFAULT_STATIC_IMPORTS);
        w.emitStaticImports(this.packageName + "." + FUNCTIONS_CLASS_NAME + "._");

        // w.emitImports(DEFAULT_IMPORTS_PATTERNS);
    }

    protected void emitPackageLevelImport(ScalaWriter w) throws IOException {
        for (String p : this.packageLevelImports) {
            w.emitImports(this.packageName + "." + p + "._");
        }
    }

    protected void verifyJavaStatic(String fieldType, String fieldName) {
        if (javaTypeTranslator.inStaticTypes(fieldType)) {
            javaTypeTranslator.registerAbstractType(fieldName, javaTypeTranslator.translateStaticType(fieldType));
        }
    }

    private void overrideCompare(String u, ScalaWriter w) throws IOException {
        w.beginMethod("Int", "compare", DEFAULT_MODIFIERS, u, "that");
        w.beginControlFlow("if(this.rank == that.rank)");
        w.emitStatement("return 0");
        w.endControlFlow();
        w.beginControlFlow("else");
        w.emitStatement("return this.rank-that.rank");
        w.endControlFlow();
        w.endMethod();
    }

    protected void beginElementKind(ScalaWriter w, ElementKind kind, String identifier, Set<Modifier> modifiers,
                                    String classParentType, org.abs_models.frontend.ast.List<InterfaceTypeUse> list) throws IOException {
        beginElementKind(w, kind, identifier, modifiers, classParentType, list, null, true);
    }

    /**
     * Begin a Java type.
     *
     * @param w               the Java writer
     * @param kind            See {@link ElementKind}
     * @param identifier      the Java identifier of the type
     * @param modifiers       the set of {@link Modifier}s
     * @param classParentType the extending type that can be <code>null</code>
     * @param list            the implementing interface that can be <code>null</code>
     * @param isActor         indicates if the class should "implement"
     *                        <code>abs.api.Actor</code>
     * @throws IOException              Exception from {@link ScalaWriter}
     * @throws IllegalArgumentException if kind other than "class" or "interface" is requested
     */
    protected void beginElementKind(ScalaWriter w, ElementKind kind, String identifier, Set<Modifier> modifiers,
                                    String classParentType, org.abs_models.frontend.ast.List<InterfaceTypeUse> list, List<String> constructorParameters,
                                    final boolean isActor) throws IOException {
        Set<String> implementsTypes = new HashSet<>();
        if (list != null) {
            for (InterfaceTypeUse interfaceTypeUse : list) {
                implementsTypes.add(interfaceTypeUse.getName());
            }

        }
        String kindName = kind.name().toLowerCase();
        switch (kind) {
            case CLASS:
                w.beginType(identifier, kindName, modifiers, classParentType, constructorParameters,
                        implementsTypes.toArray(new String[0]));
                if (isActor) {
                    emitSerialVersionUid(w);
                }

                return;
            case INTERFACE:
                implementsTypes.add(ABS_API_INTERFACE_CLASS);
                implementsTypes.add(String.format("%s[%s]", ORDERED_INTERFACE_CLASS, ABS_API_INTERFACE_CLASS));
                w.beginType(identifier, kindName, modifiers, null, implementsTypes.toArray(new String[0]));
                return;
            case ENUM:
                w.beginType(identifier, kindName, modifiers);
                return;
            case CONSTRUCTOR:
                w.beginType(identifier, "object", modifiers, classParentType, implementsTypes.toArray(new String[0]));
                return;
            case OTHER:
                w.beginType(identifier, "case class", modifiers, classParentType, constructorParameters,
                        implementsTypes.toArray(new String[0]));
                return;

            default:
                throw new IllegalArgumentException("Unsupported Java element kind: " + kind);
        }
    }

    private void buildProgramDeclarationTypes(ModuleDecl program) {
        /*
         * ABS allows for SAME naming of an interface and an implementing class.
         * To be able to properly compile this, we need to eagerly identify the
         * elements of an ABS program. Strategy of compiling:
         *
         * 1. Compile interfaces to separate files
         *
         * 2. Compile classes to separate files
         */

        elements.clear();
        for (AbsElementType t : EnumSet.allOf(AbsElementType.class)) {
            elements.put(t, new LinkedList<>());
        }

        // 1. Interfaces

        for (Decl decl : program.getDecls()) {
            if (decl.isInterface() && !builtinSet.contains(decl.getName())) {
                dataCollisions.put(decl.getName(), "Class" + decl.getName());
                elements.get(AbsElementType.INTERFACE).add(decl);
            }
        }

        // 2. Classes
        for (Decl decl : program.getDecls()) {
            if (decl.isClass() && !builtinSet.contains(decl.getName())) {
                final String className = decl.getName();
                classNames.put(className, className);
                elements.get(AbsElementType.CLASS).add(decl);
            }
        }
        // 3. Functions
        for (Decl decl : program.getDecls()) {
            if (decl.isFunction() && !builtinSet.contains(decl.getName())) {
                elements.get(AbsElementType.FUNCTION).add(decl);
            }
        }
        // 4. Data
        for (Decl decl : program.getDecls()) {
            if ((decl.isDataType() || decl.isDataConstructor()) && (!builtinSet.contains(decl.getName()))) {
                elements.get(AbsElementType.DATA).add(decl);
            }
        }
        // 5. T
        for (Decl decl : program.getDecls()) {
            if ((decl.isTypeParameter() || decl.isTypeSyn()) && (!builtinSet.contains(decl.getName()))) {
                elements.get(AbsElementType.TYPE).add(decl);
            }
        }
        // 6. Exceptions
        for (Decl decl : program.getDecls()) {
            if (decl.isException() && !builtinSet.contains(decl.getName())) {
                elements.get(AbsElementType.EXCEPTION).add(decl);
            }
        }
    }

    private void emitSerialVersionUid(ScalaWriter w) throws IOException {
        w.emitField("java.lang.Long", "serialVersionUID", EnumSet.of(Modifier.PRIVATE, Modifier.FINAL), "1L");
        w.emitEmptyLine();
    }

    protected void close(ScalaWriter childWriter, ScalaWriter parentWriter) throws IOException {
        if (childWriter != parentWriter) {
            childWriter.close();
        }
    }

    private VarDefinition createVarDefinition(String varName, String varType) {
        ModuleDecl current = currentModule();
        if (current == null) {
            throw new IllegalStateException("No current module is available.");
        }
        String clazz = current.getName();
        String fqClassName = this.packageName + "." + clazz;
        VarDefinition vd = new VarDefinition(varName, varType);
        variables.put(fqClassName, vd);
        return vd;

    }

    private String createMessageVariableName(String calleeId) {
        return "msg_" + Math.abs(calleeId.hashCode()) + "" + RANDOM.nextInt(1000);
    }

    private String createMessageResponseVariableName(String msgVarName) {
        return msgVarName + "_response";
    }

    private void createMethodDefinition(String returnType, String name, List<String> parameters) {
        String className = currentClass();
        if (className == null) {
            throw new IllegalStateException("No current 'class' is available.");
        }
        String fqClassName = this.packageName + "." + className;
        MethodDefinition md = new MethodDefinition(fqClassName, returnType, name, parameters);
        currentMethod = md;
        methods.put(fqClassName, md);

        if (!programMethods.containsKey(fqClassName + "." + name))
            programMethods.put(fqClassName + "." + name, md);
        else {
            currentMethod = programMethods.get(fqClassName + "." + name);
            md.setContainsAwait(currentMethod.containsAwait());
            programMethods.put(fqClassName + "." + name, md);
            currentMethod = md;
        }
    }

    /**
     * Creates a Java method invocation:
     * <p>
     * <pre>
     * myObj.myMethod(p1, p2, p3)
     * </pre>
     *
     * @param object the callee object
     * @param method the method of the callee object
     * @param list   the parameters of the method that can be empty string
     * @return a string representing a Java method invocation statement
     */

    protected String generateJavaMethodInvocation(String object, String method, org.abs_models.frontend.ast.List<PureExp> list,
                                                  ScalaWriter w, char c, int counter) {
        object = getDuplicate(object, w);
        List<String> params = new ArrayList<>();
        List<String> duplicateParameters = new LinkedList<>();
        for (PureExp par : list) {
            StringWriter parSW = new StringWriter();
            ScalaWriter parameterWriter = new ScalaWriter(parSW);
            parameterWriter.continuationLevel = w.continuationLevel;
            parameterWriter.duplicateReplacements = w.duplicateReplacements;

            par.accept(this, parameterWriter);
            String string;
            if (par instanceof Access) {
                params.add(parSW.toString());
                string = parSW.toString();
            } else {
                params.add(parSW.toString() + "pureexp");
                string = parSW.toString() + "pureexp";
            }

            boolean found = false;

            if (string.contains("pureexp")) {
                duplicateParameters.add(string.replace("pureexp", ""));
            } else {
                StringWriter psw = new StringWriter();
                ScalaWriter tw = new ScalaWriter(psw);

                Type t = par.getType();
                t.toUse().accept(this, tw);
                String varType = psw.toString();
                for (HashMap<String, String> hashMap1 : w.duplicateReplacements) {
                    if (hashMap1.containsKey(string)) {
                        final String stringName = "f_" + c + counter + hashMap1.get(string);
                        duplicateParameters.add(stringName);
                        try {
                            w.emitStatement("var %s : %s = %s", stringName, varType,
                                    hashMap1.get(string));
                        } catch (IOException e) {
                            // TODO Auto-generated catch block
                            e.printStackTrace();
                        }
                        found = true;
                        break;

                    }
                }
                if (!found)
                    if (par.getType() != null) {
                        final String stringName = "f_" + c + counter + string.replaceAll("\\.", "");
                        ;
                        duplicateParameters.add(stringName);
                        try {
                            w.emitStatement("var %s : %s = %s", stringName, varType, string);
                        } catch (IOException e) {
                            // TODO Auto-generated catch block
                            e.printStackTrace();
                        }
                    } else if (string.contains("$")) {
                        final String stringName = "f_" + c + counter + string;
                        duplicateParameters.add(stringName);
                        try {

                            w.emitStatement("var %s : %s = %s", stringName, varType, string);
                        } catch (IOException e) {
                            // TODO Auto-generated catch block
                            e.printStackTrace();
                        }
                    } else
                        duplicateParameters.add(string);
            }

        }
        return String.format("%s.%s(%s)", object, method, duplicateParameters == null || duplicateParameters.isEmpty()
                ? "" : String.join(COMMA_SPACE, duplicateParameters));
    }

    private String generateContinuationMethodInvocation(String object, String method, ScalaWriter w, char c,
                                                        int counter, boolean lambda) {

        object = getDuplicate(object, w);
        boolean found = false;
        List<String> duplicateParameters = new LinkedList<>();
        for (TreeSet<VarDefinition> defs : variablesInScope) {
            for (VarDefinition varDefinition : defs) {
                found = false;
                String string = varDefinition.getName();
                for (HashMap<String, String> hashMap1 : w.duplicateReplacements) {
                    if (hashMap1.containsKey(string)) {
                        if (lambda) {
                            final String stringName = "f_" + c + counter + hashMap1.get(string);
                            duplicateParameters.add(stringName);
                            try {
                                w.emitStatement("var %s : %s = %s", stringName, varDefinition.getType(),
                                        hashMap1.get(string));
                            } catch (IOException e) {
                                // TODO Auto-generated catch block
                                e.printStackTrace();
                            }
                        } else {
                            duplicateParameters.add(hashMap1.get(string));
                        }
                        found = true;
                        break;

                    }
                }
                if (!found) {
                    if (lambda) {
                        final String stringName = "f_" + c + counter + string;
                        duplicateParameters.add(stringName);
                        try {
                            w.emitStatement("var %s : %s = %s", stringName, varDefinition.getType(), string);
                        } catch (IOException e) {
                            // TODO Auto-generated catch block
                            e.printStackTrace();
                        }
                    } else
                        duplicateParameters.add(string);

                }
            }
        }
        return String.format("%s.%s(%s)", object, method, duplicateParameters == null || duplicateParameters.isEmpty()
                ? "" : String.join(COMMA_SPACE, duplicateParameters));

    }

    /**
     * Create an asynchronous message in the context of ABS API which is either
     * an instance of {@link Runnable} or a {@link Callable}.
     *
     * @param msgVarName the variable name to use the created message
     * @param returnType the return type of the message; if <code>null</code>, it means
     *                   to use {@link Runnable}
     * @param expression the Java expression to use for the body of the lambda
     *                   expression
     * @return a string in Java representing a lambda expression for a
     * {@link Runnable} or a {@link Callable}
     */
    protected String generateMessageStatement(String msgVarName, String returnType, String expression) {
        String newRetturnType = String.format("%s[%s]", ABSFUTURE_CLASS, returnType);
        String actualReturnType = resolveMessageType(newRetturnType);
        return String.format("var %s : %s = () => %s", msgVarName, actualReturnType, expression);
    }

    /**
     * Create a Java statement when sending a message to an {@link } in the
     * ABS API.
     *
     * @param target          the receiver identifier of the message
     * @param msgReturnType   the expected return type of the message; if <code>null</code>,
     *                        the generated {@link } will be over {@link Void}
     * @param msgVarName      the variable name of the message
     * @param responseVarName the variable of the generated {@link }; can be
     *                        <code>null</code>
     * @return a Java statement string for such a call
     */
    protected String generateMessageInvocationStatement(String target, final boolean isDefined, String msgReturnType,
                                                        String msgVarName, String responseVarName, ScalaWriter w) {

        responseVarName = getDuplicate(responseVarName, w);

        final String method = "send";
        if (responseVarName.endsWith("_response")) {
            return String.format("%s.%s(%s)", target, method, msgVarName);
        }
        if (!isDefined) {
            return String.format("%s = %s.%s(%s)", responseVarName, target, method, msgVarName);
        }
        String returnType = msgReturnType;
        return String.format("var %s : %s = %s.%s (%s)", responseVarName, returnType, target, method, msgVarName);
    }

    /**
     * @param target
     * @return
     */
    protected String generateMessageSyncInvocationStatement(String target, String msgVarName, String contVarName, boolean strict) {
        final String method = GET_SPAWN;

        if (fromConstructor)
            return String.format("%s = %s.%s(%s, %s, %s.HIGH_PRIORITY, %s)", CONS_FUT, target, method, msgVarName, contVarName,
                    ABS_API_INTERFACE_CLASS, strict);
        else
            return String.format("return %s.%s(%s, %s, %s.HIGH_PRIORITY, %s)", target, method, msgVarName, contVarName,
                    ABS_API_INTERFACE_CLASS, strict);
        // if (!isDefined) {
        // return String.format("%s = %s.%s(%s, %s)", responseVarName, target,
        // method, msgVarName, contVarName);
        // }
    }

    private String translateDataColl(String key) {
        if (dataCollisions.containsKey(key))
            return dataCollisions.get(key);
        else
            return key;
    }

    private void emitImplicitConversions(ScalaWriter w) throws IOException {
        w.emit(" implicit def f2int(f: Float):Int = f.round.toInt;\n");
        w.emit(" implicit def f2int(r: Rational):Int = r.toInt;\n");

//
//     w.emit("implicit def fun2Call[R](f: () => R) = new Callable[R] { def call
//     : R = f() }\n");
//     w.emit("implicit def funcToRunnable( func : () => Unit ) = new
//     Runnable(){ def run() = func() }\n");
//     w.emit("implicit def funcToCallablewFut[R,V]( f : (ABSFuture[V]) => R ) =
//     new CallablewFut[R,V] { def run(v: ABSFuture[V]) : R = f(v) }\n");
//     w.emit("implicit def funcToRunnablewFut[V]( f : (ABSFuture[V]) => Unit )
//     = new RunnablewFut[V] { def run(v: ABSFuture[V]) : Unit = f(v) }\n");
//     w.emit("implicit def funcToCallableGet[T, V](f: (V) => T) = new
//     CallableGet[T, V] { def run(v: V): T = f(v) }\n");
//     w.emit("implicit def funcToRunnableGet[V](f: (V) => Unit) = new
//     RunnableGet[V] { def run(v: V): Unit = f(v) }\n");
    }

    private String currentClass() {
        return this.classes.peek();
    }

    private ModuleDecl currentModule() {
        return this.modules.peek();
    }

    /**
     * @param w
     * @param fieldType
     * @param fieldIdentifier
     * @param initialValue
     * @param isFinal
     * @return
     * @throws IOException
     */
    protected ScalaWriter emitField(ScalaWriter w, String fieldType, String fieldIdentifier, String initialValue,
                                    final boolean isFinal) throws IOException {
        EnumSet<Modifier> modifiers = EnumSet.noneOf(Modifier.class);

        fieldIdentifier = getDuplicate(fieldIdentifier, w);

        if (isFinal) {
            modifiers.add(Modifier.FINAL);
        }
        if (initialValue != null) {
            return w.emitField(fieldType, fieldIdentifier, modifiers, initialValue);
        } else {
            return w.emitField(fieldType, fieldIdentifier, modifiers);
        }
    }

    protected String getDuplicate(String name, ScalaWriter w) {

        for (HashMap<String, String> hashMap : w.duplicateReplacements) {
            if (hashMap.containsKey(name)) {
                return hashMap.get(name);
            }
        }
        return name;
    }

    private void startContinuation(StringBuilder label, ScalaWriter auxw, String returnType, List<String> parameters)
            throws IOException {
        // System.out.println(label);
        // String futureReturnType = String.format("%s[%s]", ABSFUTURE_CLASS,
        // returnType);
        auxw.beginMethod(returnType, label.toString(), DEFAULT_MODIFIERS, parameters, null);

        for (String string : auxw.methodParameters.keySet()) {

            String newName = "w_" + awaitCounter + "$" + string;
            auxw.duplicateReplacements.peekLast().put(string, newName);
            emitField(auxw, auxw.methodParameters.get(string), newName, string, false);
        }
        // System.out.println("Starting continuation with " +
        // auxw.duplicateReplacements);
    }

    private boolean callHasAwait(SyncCall smc, String methodName) {
        StringWriter typew = new StringWriter();
        Type tu = smc.getCallee().getType();
        if (tu.toUse() == null) {
            if (tu.isBoundedType()) {
                Type bt = ((BoundedType) tu).getBoundType();
                if (bt != null && bt.toUse() != null) {
                    bt.toUse().accept(this, new ScalaWriter(typew));
                } else
                    typew.append(tu.getQualifiedName());
            } else
                typew.append(tu.getQualifiedName());
        } else {
            tu.toUse().accept(this, new ScalaWriter(typew));
        }
        String qname = typew.toString();

        return checkAwait(methodName, qname);
    }

    private boolean checkAwait(String methodName, String qname) {
        String key = qname + "." + methodName;
        if (programMethods.containsKey(key))
            return programMethods.get(key).containsAwait();
        else {
            int endPoint = qname.lastIndexOf('.') + 1;
            StringBuilder k = new StringBuilder(qname.substring(0, endPoint));
            String coll = translateDataColl(qname.substring(endPoint));
            k.append(coll);
            k.append("." + methodName);
            if (programMethods.containsKey(k.toString()))
                return programMethods.get(k.toString()).containsAwait();
        }
        return false;
    }

    private String resolveMessageType(String returnType) {
        String actualReturnType = null;
        if (returnType != null) {
            if (isVoid(returnType)) {
                actualReturnType = "Runnable";
            } else {
                actualReturnType = String.format("Callable[%s]", returnType);
            }
        } else {
            actualReturnType = "Runnable";
        }
        return actualReturnType;
    }

    private boolean isVoid(String returnType) {

        return returnType.equals("Unit");
    }

    private void logNotImplemented(String format, Object... args) {
        String msg = "Not implemented: " + String.format(format, args);
        logWarn(msg);
    }

    private void logNotSupported(String format, Object... args) {
        String msg = "Not supported: " + String.format(format, args);
        logWarn(msg);
    }

    private void logWarn(String msg) {
        LOGGER.warning(msg);
    }

    protected String getCalleeId(AsyncCall amc, ScalaWriter w) {
        StringWriter auxsw = new StringWriter();
        ScalaWriter auxw = new ScalaWriter(auxsw);
        auxw.continuationLevel = w.continuationLevel;
        auxw.duplicateReplacements = w.duplicateReplacements;
        // System.out.println("amc calee writer "+ auxw.duplicateReplacements);
        amc.getCallee().accept(this, auxw);
        String calleeId = auxsw.toString();
        String variable = "Actor" + Math.abs(calleeId.hashCode());
        try {
            w.emitStatement("val %s = %s", variable, calleeId);
        } catch (IOException e) {
            e.printStackTrace();
        }
        return variable;
    }

    protected String getCalleeId(SyncCall smc, ScalaWriter w) {
        StringWriter auxsw = new StringWriter();
        ScalaWriter auxw = new ScalaWriter(auxsw);
        auxw.continuationLevel = w.continuationLevel;
        auxw.duplicateReplacements = w.duplicateReplacements;
        smc.getCallee().accept(this, auxw);
        String calleeId = auxsw.toString();
        return calleeId;
    }

}
