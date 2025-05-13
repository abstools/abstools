/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved.
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.abs_models.backend.java.codegeneration;

import java.io.PrintStream;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;
import java.util.stream.Collectors;
import java.util.stream.StreamSupport;

import org.abs_models.backend.java.JavaBackend;
import org.abs_models.backend.java.lib.runtime.ABSInitObjectCall;
import org.abs_models.backend.java.lib.runtime.ABSObject;
import org.abs_models.backend.java.lib.runtime.ABSRunMethodCall;
import org.abs_models.backend.java.lib.runtime.ABSRuntime;
import org.abs_models.backend.java.lib.runtime.ABSThread;
import org.abs_models.backend.java.lib.runtime.COG;
import org.abs_models.backend.java.lib.runtime.Task;
import org.abs_models.backend.java.lib.types.ABSClass;
import org.abs_models.backend.java.scheduling.UserSchedulingStrategy;
import org.abs_models.frontend.analyser.SemanticConditionList;
import org.abs_models.frontend.ast.ClassDecl;
import org.abs_models.frontend.ast.FieldDecl;
import org.abs_models.frontend.ast.InterfaceTypeUse;
import org.abs_models.frontend.ast.MethodImpl;
import org.abs_models.frontend.ast.MethodSig;
import org.abs_models.frontend.ast.ParamDecl;
import org.abs_models.frontend.typechecker.InterfaceType;

public class ClassDeclGenerator {

    private final ClassDecl decl;
    private final String className;
    protected final PrintStream stream;
    // All method signatures of all our interfaces
    private HashMap<String, MethodSig> implementedSignatures = new HashMap<>();

    public static void generate(PrintStream stream, ClassDecl decl) {
        ClassDeclGenerator gen = new ClassDeclGenerator(stream, decl);
        gen.generate();
    }

    private ClassDeclGenerator(PrintStream stream, ClassDecl decl) {
        this.stream = stream;
        this.decl = decl;
        // Note that we re-use / abuse a typechecking method here to
        // fill `sigs`; would be nice to have a method in the AST
        // since the Erlang backend needs the same information
        decl.addAllMethodSigs(implementedSignatures, new SemanticConditionList());
        className = JavaBackend.getClassName(decl.getName());
    }

    private void generate() {
        JavaGeneratorHelper.generateHelpLine(decl, stream);
        generateClassHeader();
        generateClassBody();
    }

    private void generateClassBody() {
        stream.println(" {");

        stream.println();
        generateFieldNamesMethod();
        stream.println();
        generateHttpCallableMethodInfoMethod();
        stream.println();
        generateHttpCallableMethods();
        stream.println();
        generateFields();
        if (decl.hasParam() || decl.hasField()) {
            stream.println();
        }
        generateConstructor();
        stream.println();
        generateGetFieldValueMethod();
        stream.println();
        stream.println("public final java.lang.String getClassName() { return \"" + decl.getName() + "\"; }");
        stream.println("public final java.lang.String toString() { return \"" + decl.getModuleDecl().getName() + "." +  decl.getName() + "(\" + __id + \")\"; }");
        stream.println();
        generateCreateNewCOGMethod();
        stream.println();
        generateNewObjectMethods();
        stream.println();
        generateMethods();
        stream.println("}");
    }

    private void generateHttpCallableMethods() {
        if (implementedSignatures.values().stream().noneMatch(MethodSig::isHTTPCallable)) return;
        stream.println("private static final java.util.Map<java.lang.String, java.lang.invoke.MethodHandle> __modelApiMethodCache = new java.util.HashMap<>();");
        stream.println();
        stream.println("public org.abs_models.backend.java.lib.runtime.ABSFut invokeMethod(java.lang.String name, java.util.List<org.abs_models.backend.java.lib.types.ABSValue> arguments) {");
        stream.println("java.lang.invoke.MethodHandle handle = __modelApiMethodCache.get(name);");
        stream.println("if (handle == null) return null;");
        stream.println("else  try {");
        stream.println("return (org.abs_models.backend.java.lib.runtime.ABSFut)handle.bindTo(this).invokeWithArguments(arguments);");
        stream.println("} catch (java.lang.Throwable t) {");
        stream.println("return null;");
        stream.println("}");
        stream.println("}");
        stream.println();
        stream.println("static {");
        stream.println("final java.lang.invoke.MethodHandles.Lookup lookup = java.lang.invoke.MethodHandles.lookup();");
        stream.println("try {");
        for (MethodSig sig : implementedSignatures.values()) {
            if (!sig.isHTTPCallable()) continue;
            String name = sig.getName();
            stream.print("__modelApiMethodCache.put(");
            stream.print("\"" + name + "\", ");
            stream.print("lookup.findVirtual(" + className + ".class, ");
            stream.println("\"async_" + JavaBackend.getMethodName(name) + "\",");
            stream.print(" java.lang.invoke.MethodType.methodType(org.abs_models.backend.java.lib.runtime.ABSFut.class");
            for (ParamDecl d : sig.getParams()) {
                String javaTypeName = JavaBackend.getQualifiedString(d.getTypeUse().getType());
                if (javaTypeName.indexOf('<') != -1) {
                    javaTypeName = javaTypeName.substring(0, javaTypeName.indexOf('<'));
                }
                stream.print(", ");
                stream.print(javaTypeName + ".class");
            }
            stream.println(")));");
        }
        stream.println("} catch (NoSuchMethodException | IllegalAccessException e) {");
        stream.println("            throw new ExceptionInInitializerError(e);");
        stream.println("}");
        stream.println("}");
    }

    private void generateHttpCallableMethodInfoMethod() {
        String separator = "";
        stream.print ("private static final java.util.List<java.util.Map<java.lang.String, java.lang.Object>> __callableMethods = java.util.List.of(");
        for (Map.Entry<String, MethodSig> entry : implementedSignatures.entrySet()) {
            MethodSig sig = entry.getValue();
            if (sig.isHTTPCallable()) {
                stream.println(separator); separator = ",";
                stream.println("java.util.Map.of(");
                stream.println("\"name\", \"" + sig.getName() + "\",");
                stream.print("\"parameters\", java.util.List.of(");
                stream.print(StreamSupport.stream(sig.getParamList().spliterator(), false)
                    .map(param -> "java.util.Map.of(\"name\", \"" + param.getName() + "\","
                                  + "\"type\", \"" + param.getType().toString() + "\")")
                    .collect(Collectors.joining(",")));
                stream.println("),");
                stream.println("\"return\", \"" + sig.getReturnType().getType().toString() + "\"");
                stream.print(")");
            }
        }
        stream.println(");");
        stream.println("public java.util.List<java.util.Map<java.lang.String, java.lang.Object>> getHttpCallableMethodInfo() { return __callableMethods; }");
    }

    private void generateMethods() {
        // methods
        for (MethodImpl m : decl.getMethods()) {
            m.generateJava(stream);
        }
    }

    private void generateNewObjectMethods() {
        // Convenience method for new C
        stream.print("public static final <T extends " + className + "> T createNewLocalObject");
        JavaGeneratorHelper.generateParams(stream, decl.getParams());
        stream.print(" { return (T)");
        stream.print(className + ".__ABS_createNewLocalObject");
        JavaGeneratorHelper.generateParamArgs(stream, "null", decl.getParams());
        stream.println("; }");

        // static constructor method for new C
        stream.print("public static final <T extends " + className + "> T __ABS_createNewLocalObject");
        JavaGeneratorHelper.generateParams(stream, ABSObject.class.getName() + " __ABS_source", decl.getParams());
        stream.println(" {");
        generateObjectConstruction(ABSRuntime.class.getName() + ".getRuntime()");
        stream.println("__ABS_result.__ABS_init();");
        if (decl.isActiveClass()) {
            stream.println("final " + Task.class.getName() + " __ABS_sendingTask = " + ABSThread.class.getName() + ".getCurrentTask();");
            stream.println(ABSRuntime.class.getName() + ".getRuntime().asyncCall(new " + ABSRunMethodCall.class.getName() + "(__ABS_sendingTask,__ABS_source,__ABS_result));");
        }
        stream.println("return (T)__ABS_result;");
        stream.println("}");
    }

    private void generateCreateNewCOGMethod() {
        // Convenience method for new cog C
        stream.print("public static final <T extends " + className + "> T createNewCogObject");
        JavaGeneratorHelper.generateParams(stream, decl.getParams());
        stream.print(" { ");
        stream.print("return (T)");
        stream.print(className + ".__ABS_createNewCogObject");
        // FIXME: does createNewCOGObject() need the DC argument?  Who calls this
        // method?
        JavaGeneratorHelper.generateParamArgs(stream, "null, null, null", decl.getParams());
        stream.println("; }");

        // static constructor method for new cog C
        stream.print("public static final <T extends " + className + "> T __ABS_createNewCogObject");
        JavaGeneratorHelper.generateParams(stream,
                ABSObject.class.getName() + " __ABS_source, "
                + UserSchedulingStrategy.class.getName() + " Strategy, "
                + "ABS.DC.DeploymentComponent_i DC",
                decl.getParams());
        stream.println(" {");
        stream.println("final " + ABSRuntime.class.getName() + " __ABS_runtime = " + ABSRuntime.class.getName() + ".getRuntime();");
        stream.println("final " + COG.class.getName() + " __ABS_cog = Strategy == null ? __ABS_runtime.createCOG(" + className + ".class, DC) : __ABS_runtime.createCOG(" + className + ".class, DC, Strategy);");
        stream.println("final " + ABSThread.class.getName() + " __ABS_thread = " + ABSThread.class.getName() + ".getCurrentThread();");
        stream.println("final " + COG.class.getName() + " __ABS_oldCOG = __ABS_thread.getCurrentCOG();");
        stream.println("final " + Task.class.getName() + " __ABS_sendingTask = __ABS_oldCOG.getScheduler().getActiveTask();");
        stream.println("__ABS_thread.setCOG(__ABS_cog);");
        stream.println("try {");
        generateObjectConstruction("__ABS_runtime");

        stream.println("__ABS_runtime.cogCreated(__ABS_result);");
        stream.println("__ABS_cog.addTask(new " + Task.class.getName() + "(new " +
                ABSInitObjectCall.class.getName() + "(__ABS_sendingTask,__ABS_source,__ABS_result)));");

        if (decl.isActiveClass()) {
            stream.println("__ABS_runtime.asyncCall(new " + ABSRunMethodCall.class.getName() + "(__ABS_sendingTask,__ABS_source,__ABS_result));");
        }
        stream.println("return (T)__ABS_result;");
        stream.println("} finally {");
        stream.println("__ABS_thread.setCOG(__ABS_oldCOG);");
        stream.println("}");
        stream.println("}");
    }

    private void generateObjectConstruction(String runtime) {
        stream.print(className + " __ABS_result = ");
        if (decl.isForeign()) {
            stream.println("(" + className + ") " + runtime + ".getForeignObject");
            JavaGeneratorHelper.generateParamArgs(stream,"\"" + decl.getModuleDecl().getName() + "." + decl.getName() + "\"", decl.getParams());
            stream.println(";");
            stream.print("if (__ABS_result == null) __ABS_result = ");
        }

        stream.print("new " + className);
        JavaGeneratorHelper.generateParamArgs(stream,decl.getParams());
        stream.println(";");

    }

    private void generateGetFieldValueMethod() {
        stream.println("protected final Object getFieldValue(java.lang.String __ABS_fieldName) throws java.lang.NoSuchFieldException {");
        for (ParamDecl p : decl.getParams()) {
            stream.println("if (__ABS_fieldName.equals(\"" + p.getName() + "\")) return " + JavaBackend.getVariableName(p.getName()) + ";");
        }

        for (FieldDecl f : decl.getFields()) {
            stream.println("if (__ABS_fieldName.equals(\"" + f.getName() + "\")) return " + JavaBackend.getVariableName(f.getName()) + ";");
        }
        stream.println("return super.getFieldValue(__ABS_fieldName);");

        stream.println("}");
    }

    private void generateConstructor() {
        // constructor
        stream.print("public " + className);
        JavaGeneratorHelper.generateParams(stream, decl.getParams());
        stream.println(" {");

        for (ParamDecl p : decl.getParams()) {
            stream.println("this." + JavaBackend.getVariableName(p.getName()) + " = " + JavaBackend.getVariableName(p.getName()) + ";");
        }
        stream.println("getCOG().objectCreated(this);");

        stream.println("}");

        stream.println("protected final void __ABS_init() {");

        for (FieldDecl f : decl.getFields()) {
            if (f.hasInitExp()) {
                stream.print("this.");
                stream.print(JavaBackend.getVariableName(f.getName()));
                stream.print(" = ");
                f.getInitExp().generateJava(stream);
                stream.print(";");
                stream.println();
            }
        }

        if (decl.hasInitBlock()) {
            decl.getInitBlock().generateJava(stream);
        }

        stream.println("getCOG().objectInitialized(this);");

        stream.println("}");
    }

    private void generateFields() {
        for (ParamDecl p : decl.getParams()) {
            stream.print("private ");
            p.generateJava(stream);
            stream.println(";");
        }

        for (FieldDecl f : decl.getFields()) {
            f.generateJava(stream);
        }
    }

    private void generateFieldNamesMethod() {
        java.util.List<String> fieldNames = getFieldNames();

        stream.print("private static final java.util.List<java.lang.String> __fieldNames = java.util.List.of(");
        stream.print(fieldNames.stream()
            .map(name -> "\"" + name + "\"")
            .collect(Collectors.joining(", ")));
        stream.println(");");

        stream.println("public final java.util.List<java.lang.String> getFieldNames() { return __fieldNames; }");
    }

    private java.util.List<String> getFieldNames() {
        java.util.List<String> fieldNames = new ArrayList<>();
        for (ParamDecl p : decl.getParams()) {
            fieldNames.add(p.getName());
        }
        for (FieldDecl f : decl.getFields()) {
            fieldNames.add(f.getName());
        }
        return fieldNames;
    }

    private void generateClassHeader() {
        stream.print("public ");
        if (!decl.isForeign())
            stream.print("final ");
        stream.print("class " + className + " extends " + ABSObject.class.getName() + " implements " + ABSClass.class.getName());

        for (InterfaceTypeUse use : decl.getImplementedInterfaceUses()) {
            String iname = JavaBackend.getQualifiedString(((InterfaceType)use.getType()).getDecl());
            stream.print(", " + iname);
        }
    }

}
