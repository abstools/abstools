/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved.
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.abs_models.backend.java;

import java.io.File;
import java.io.IOException;
import java.lang.reflect.Method;
import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;
import java.nio.charset.UnsupportedCharsetException;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.UUID;

import org.abs_models.Absc;
import org.abs_models.backend.common.InternalBackendException;
import org.abs_models.backend.java.codegeneration.JavaCode;
import org.abs_models.backend.java.codegeneration.JavaCodeGenerationException;
import org.abs_models.backend.java.lib.runtime.ABSFut;
import org.abs_models.backend.java.lib.runtime.ABSObject;
import org.abs_models.backend.java.lib.types.ABSBool;
import org.abs_models.backend.java.lib.types.ABSFloat;
import org.abs_models.backend.java.lib.types.ABSInteger;
import org.abs_models.backend.java.lib.types.ABSProcess;
import org.abs_models.backend.java.lib.types.ABSRational;
import org.abs_models.backend.java.lib.types.ABSString;
import org.abs_models.backend.java.lib.types.ABSUnit;
import org.abs_models.common.NotImplementedYetException;
import org.abs_models.frontend.ast.ClassDecl;
import org.abs_models.frontend.ast.ClassModifier;
import org.abs_models.frontend.ast.ConstructorArg;
import org.abs_models.frontend.ast.DataConstructor;
import org.abs_models.frontend.ast.DataTypeDecl;
import org.abs_models.frontend.ast.Decl;
import org.abs_models.frontend.ast.FunctionDecl;
import org.abs_models.frontend.ast.InterfaceDecl;
import org.abs_models.frontend.ast.Model;
import org.abs_models.frontend.ast.ModuleDecl;
import org.abs_models.frontend.ast.ModuleModifier;
import org.abs_models.frontend.ast.Name;
import org.abs_models.frontend.ast.TypeUse;
import org.abs_models.frontend.parser.Main;
import org.abs_models.frontend.typechecker.BoundedType;
import org.abs_models.frontend.typechecker.DataTypeType;
import org.abs_models.frontend.typechecker.InterfaceType;
import org.abs_models.frontend.typechecker.Type;
import org.abs_models.frontend.typechecker.TypeParameter;
import org.abs_models.frontend.typechecker.UnionType;

public class JavaBackend extends Main {

    /**
     * The charset to use for generated files. This will also be one of the {@link StandardCharsets}, so using it
     * is guaranteed not to throw an {@link UnsupportedCharsetException}.
     */
    public final static Charset CHARSET = StandardCharsets.UTF_8;

    public static int doMain(Absc args) {
        int result = 0;
        JavaBackend backEnd = new JavaBackend();
        backEnd.arguments = args;
        try {
            result = backEnd.compile();
        } catch (NotImplementedYetException e) {
            System.err.println(e.getMessage());
            result = 1;
        } catch (Exception e) {
            System.err.println("An error occurred during compilation:\n" + e.getMessage());

            if (backEnd.arguments.debug) {
                e.printStackTrace();
            }
            result = 1;
        }
        return result;
    }

    // unused -- does not work
    private boolean untypedJavaGen = false;

    public static void printUsage() {
        System.out.println("Java Backend (-java):\n"
                + "  -d <dir>       generate files to <dir>\n"
                + "  -sourceonly    do not generate class files\n"
                + "  -dynamic       generate dynamically updateable code\n");
    }

    private int compile() throws Exception {
        final Model model = parse(arguments.files);
        if (model == null || model.hasParserErrors() || model.hasErrors() || model.hasTypeErrors()) {
            printErrorMessage();
            return 1;
        }
        arguments.destDir.mkdirs();
        if (!arguments.destDir.exists()) {
            throw new InternalBackendException("Destination directory " + arguments.destDir.getAbsolutePath() + " does not exist!");
        }

        if (!arguments.destDir.canWrite()) {
            throw new InternalBackendException("Destination directory " + arguments.destDir.getAbsolutePath() + " cannot be written to!");
        }

        compile(model, arguments.destDir);
        return 0;
    }

    private void compile(Model m, File destDir) throws IOException, JavaCodeGenerationException {
        JavaCode javaCode = new JavaCode(destDir);
        if (this.untypedJavaGen) {
            if (arguments.verbose) System.out.println("Generating dynamic Java code...");
            m.generateJavaCodeDynamic(javaCode, arguments.debug_generated_code);
        } else {
            if (arguments.verbose) System.out.println("Generating Java code...");
            m.generateJavaCode(javaCode, arguments.debug_generated_code);
        }
        if (!arguments.java_sourceOnly) {
            if (arguments.verbose) System.out.println("Compiling generated Java code...");
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
