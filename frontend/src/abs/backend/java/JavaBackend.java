/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved.
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.backend.java;

import java.io.File;
import java.io.IOException;
import java.lang.reflect.Method;
import java.util.*;
import java.util.List;

import abs.backend.common.InternalBackendException;
import abs.backend.java.codegeneration.JavaCode;
import abs.backend.java.codegeneration.JavaCodeGenerationException;
import abs.backend.java.lib.runtime.ABSFut;
import abs.backend.java.lib.runtime.ABSObject;
import abs.backend.java.lib.types.*;
import abs.common.NotImplementedYetException;
import abs.frontend.ast.*;
import abs.frontend.parser.Main;
import abs.frontend.typechecker.*;

public class JavaBackend extends Main {

    public final static String CHARSET = "UTF-8";

    public static void main(final String... args) {
        doMain(args);
    }

    public static int doMain(final String... args) {
        int result = 0;
        JavaBackend backEnd = new JavaBackend();
        try {
            result = backEnd.compile(args);
        } catch (NotImplementedYetException e) {
            System.err.println(e.getMessage());
            result = 1;
        } catch (Exception e) {
            System.err.println("An error occurred during compilation:\n" + e.getMessage());

            if (backEnd.debug) {
                e.printStackTrace();
            }

            result = 1;
        }
        return result;
    }

    private File destDir = new File("gen/");
    private boolean sourceOnly = false;
    private boolean untypedJavaGen = false;
    private boolean includeDebug = false;

    @Override
    public List<String> parseArgs(String[] args) throws InternalBackendException {
        List<String> restArgs = super.parseArgs(args);
        List<String> remainingArgs = new ArrayList<>();

        for (int i = 0; i < restArgs.size(); i++) {
            String arg = restArgs.get(i);
            if (arg.equals("-d")) {
                i++;
                if (i == restArgs.size()) {
                    throw new InternalBackendException("Destination directory name not given after '-d'");
                } else {
                    destDir = new File(args[i]);
                }
            } else if (arg.equals("-sourceonly")) {
                this.sourceOnly = true;
            } else if (arg.equals("-dynamic")) {
                this.untypedJavaGen = true;
            } else if (arg.equals("-no-debuginfo")) {
                this.includeDebug = false;
            } else if (arg.equals("-debuginfo")) {
                this.includeDebug = true;
            } else if(arg.equals("-java")) {
                // nothing to do
            } else {
                remainingArgs.add(arg);
            }
        }
        return remainingArgs;
    }

    public static void printUsage() {
        System.out.println("Java Backend (-java):\n"
                + "  -d <dir>       generate files to <dir>\n"
                + "  -sourceonly    do not generate class files\n"
                + "  -no-debuginfo  generate code without listener / debugger support (default)\n"
                + "  -debuginfo     generate code with listener / debugger support\n"
                + "  -dynamic       generate dynamically updateable code\n");
    }

    private int compile(String[] args) throws Exception {
                final Model model = parse(args);
        if (model.hasParserErrors() || model.hasErrors() || model.hasTypeErrors()) {
            printErrorMessage();
            return 1;
        }
        destDir.mkdirs();
        if (!destDir.exists()) {
            throw new InternalBackendException("Destination directory " + destDir.getAbsolutePath() + " does not exist!");
        }

        if (!destDir.canWrite()) {
            throw new InternalBackendException("Destination directory " + destDir.getAbsolutePath() + " cannot be written to!");
        }

        compile(model, destDir);
        return 0;
    }

    private void compile(Model m, File destDir) throws IOException, JavaCodeGenerationException {
        JavaCode javaCode = new JavaCode(destDir);
        if (this.untypedJavaGen) {
            if (verbose) System.out.println("Generating dynamic Java code...");
            m.generateJavaCodeDynamic(javaCode, this.includeDebug);
        } else {
            if (verbose) System.out.println("Generating Java code...");
            m.generateJavaCode(javaCode, this.includeDebug);
        }
        if (!sourceOnly) {
            if (verbose) System.out.println("Compiling generated Java code...");
            javaCode.compile();
        }
    }

    private static final Map<String, String> dataTypeMap = initDataTypeMap();

    private static Map<String, String> initDataTypeMap() {
        final Map<String, String> res = new HashMap<>();
        res.put("Int", ABSInteger.class.getName());
        res.put("Rat", ABSRational.class.getName());
        res.put("Float", ABSFloat.class.getName());
        res.put("Bool", ABSBool.class.getName());
        res.put("String", ABSString.class.getName());
        res.put("Fut", ABSFut.class.getName());
        res.put("Unit", ABSUnit.class.getName());
        res.put("Process", ABSProcess.class.getName());
        return res;
    }

    public static boolean isBuiltinDataType(Type absType) {
        if (absType.isDataType())
            return dataTypeMap.containsKey(((DataTypeType)absType).getDecl().getName());
        else
            return false;
    }

    public static String getJavaType(ConstructorArg u) {
        return getJavaType(u.getTypeUse());
    }

    public static String getJavaType(TypeUse absType) {
        return getQualifiedString(absType.getType());
    }

    public static String getQualifiedString(String s) {
        return s;
    }

    public static String getQualifiedString(Name name) {
        return getQualifiedString(name.getString());
    }

    public static String getQualifiedString(Type absType) {
        String res = null;
        if (absType.isDataType()) {
            DataTypeType dt = (DataTypeType) absType;
            res = dataTypeMap.get(dt.getDecl().getName());
            if (res != null)
                return res;

            StringBuilder sb = new StringBuilder();
            if (dt.hasTypeArgs() && !containsUnboundedType(dt.getTypeArgs())) {
                sb.append("<");
                boolean first = true;
                for (Type t : dt.getTypeArgs()) {
                    if (first)
                        first = false;
                    else
                        sb.append(',');
                    sb.append(getQualifiedString(t));
                }
                sb.append(">");
            }
            return getQualifiedString(dt.getDecl()) + sb.toString();
            /*
             * if (dt.hasTypeArgs() && !containsUnboundedType(dt.getTypeArgs()))
             * {
             *
             * sb.append("<"); boolean first = true; for (Type t :
             * dt.getTypeArgs()) { if (first) first = false; else
             * sb.append(','); sb.append(getQualifiedString(t)); }
             * sb.append(">"); }
             */
        } else if (absType.isInterfaceType()) {
            InterfaceType it = (InterfaceType) absType;
            return getQualifiedString(it.getDecl());
        } else if (absType.isTypeParameter()) {
            TypeParameter tp = (TypeParameter) absType;
            return tp.getDecl().getName();
        } else if (absType.isBoundedType()) {
            BoundedType bt = (BoundedType) absType;
            if (bt.hasBoundType())
                return getQualifiedString(bt.getBoundType());
            return "?";
        } else if (absType.isAnyType()) {
            return "java.lang.Object";
        } else if (absType.isUnionType()) {
            return getQualifiedString(((UnionType) absType).getOriginatingClass());
        }

        throw new RuntimeException("Type " + absType.getClass().getName() + " not yet supported by Java backend");
    }

    private static boolean containsUnboundedType(List<Type> typeArgs) {
        for (Type t : typeArgs) {
            if (t.isBoundedType()) {
                BoundedType bt = (BoundedType)t;
                if (!bt.hasBoundType()) {
                    return true;
                }
            }
        }
        return false;
    }

    public static String getQualifiedString(Decl decl) {
        return decl.getModuleDecl().getName() + "." + getJavaName(decl);
    }

    private static final String[] JAVA_RESERVED_WORDS_ARRAY = { "abstract", "do", "import", "public", "throws",
            "boolean", "double", "instanceof", "return", "transient", "break", "else", "int", "short", "try", "byte",
            "extends", "interface", "static", "void", "case", "final", "long", "strictfp", "volatile", "catch",
            "finally", "native", "super", "while", "char", "float", "new", "switch", "class", "for", "package",
            "synchronized", "continue", "if", "private", "this", "default", "implements", "protected", "throw",
            "const", "goto", "null", "true", "false", "abs" };
    private static final Set<String> JAVA_RESERVED_WORDS = new HashSet<>();

    static {
        for (String s : JAVA_RESERVED_WORDS_ARRAY) {
            JAVA_RESERVED_WORDS.add(s);
        }
        // add methods from ABSObject to reserved words:
        for (Method m : ABSObject.class.getMethods()) {
            JAVA_RESERVED_WORDS.add(m.getName());
        }
        // the run method is special, because it can be overridden
        JAVA_RESERVED_WORDS.remove("run");
    }

    public static String getConstructorName(DataTypeDecl dataType, String name) {
        return truncate(dataType.getName() + "_" + name);
    }

    public static String getConstructorName(DataConstructor decl) {
        return getConstructorName(((DataTypeType) decl.getType()).getDecl(), decl.getName());
    }

    public static String getInterfaceName(String name) {
        return truncate(name + "_i");
    }

    public static String getClassName(String name) {
        return truncate(name + "_c");
    }

    public static String getProductName(String name) {
        return truncate(name + "_prod");
    }

    public static String getReconfigurationName(String from, String to) {
        return truncate(from + "__" + to + "_recf");
    }

    public static String getDeltaName(String name) {
        return truncate(name + "_delta");
    }

    public static String getDeltaPackageName(String name) {
        return truncate(JavaBackendConstants.LIB_DELTAS_PACKAGE + "." + name);
    }

    public static String getUpdateName(String name) {
        return truncate(name + "_upd");
    }

    public static String getModifierPackageName(String name) {
        return truncate(JavaBackendConstants.LIB_DELTAS_PACKAGE + "." + name);
    }

    public static String getModifierName() {
        return truncate("Mod_" + getRandomName());
    }

    public static String getFunctionName(String name) {
        return truncate(escapeReservedWords(name) + "_f");
    }

    public static String getMethodName(String name) {
        return escapeReservedWords(name);
    }

    public static String getVariableName(String name) {
        return escapeReservedWords(name);
    }

    private static String escapeReservedWords(String name) {
        if (JAVA_RESERVED_WORDS.contains(name)) {
            return name + "__";
        } else {
            return name;
        }
    }

    public static String getJavaName(Decl decl) {
        String result;
        if (decl instanceof FunctionDecl) {
            result = getFunctionName(decl.getName());
        } else if (decl instanceof DataConstructor) {
            result = getConstructorName((DataConstructor) decl);
        } else if (decl instanceof ClassDecl) {
            result = getClassName(decl.getName());
        } else if (decl instanceof InterfaceDecl) {
            result = getInterfaceName(decl.getName());
        } else {
            result = truncate(decl.getName());
        }
        return result;
    }

    public static String getJavaName(ModuleModifier mod) {
        String result;
        if (mod instanceof ClassModifier) {
            result = getClassName(mod.getName());
        } else {
            result = truncate(mod.getName());
        }
        return result;
    }

    // Shorten name to 255 chars as files with these names are created
    private static String truncate(String s) {
        int maxlength = 200;
        if (s.length() < maxlength) {
            return s;
        } else {
            String prefix = s.substring(0, maxlength);
            int suffix = s.hashCode(); // We do not consider collisions as highly unlikely
            return prefix + suffix;
        }
    }

    /**
     * get the java name main blocks
     */
    public static String getJavaNameForMainBlock() {
        return "Main";
    }

    /**
     * get the fully qualified java name for the main block of a given module
     */
    public static String getFullJavaNameForMainBlock(ModuleDecl module) {
        return module.getName() + "." + getJavaNameForMainBlock();
    }

    /**
     * Just return a randomly generated string
     */
    public static String getRandomName() {
        return Integer.toHexString(UUID.randomUUID().hashCode());
    }
}
