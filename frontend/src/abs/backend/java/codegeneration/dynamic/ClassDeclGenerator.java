/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.backend.java.codegeneration.dynamic;

import java.io.PrintStream;
import abs.backend.java.JavaBackend;
import abs.backend.java.lib.runtime.ABSClosure;
import abs.backend.java.lib.runtime.ABSDynamicObject;
import abs.backend.java.lib.runtime.ABSField;
import abs.backend.java.lib.runtime.ABSInitObjectCall;
import abs.backend.java.lib.runtime.ABSObject;
import abs.backend.java.lib.runtime.ABSRunMethodCall;
import abs.backend.java.lib.runtime.ABSRuntime;
import abs.backend.java.lib.runtime.ABSThread;
import abs.backend.java.lib.runtime.COG;
import abs.backend.java.lib.runtime.Task;
import abs.backend.java.lib.runtime.ABSClass;
import abs.backend.java.lib.types.ABSValue;
import abs.frontend.ast.ClassDecl;
import abs.frontend.ast.FieldDecl;
import abs.frontend.ast.MethodImpl;
import abs.frontend.ast.ParamDecl;

public class ClassDeclGenerator {

    private final ClassDecl decl;
    private final String className;
    protected final PrintStream stream;

    ClassDeclGenerator(PrintStream stream, ClassDecl decl) {
        this.stream = stream;
        this.decl = decl;
        className = JavaBackend.getClassName(decl.getName());
    }

    public void generate() {
        DynamicJavaGeneratorHelper.generateHelpLine(decl, stream);
        generateClassHeader();
        generateClassBody();
    }

    private void generateClassBody() {
        stream.println("{");

        stream.println("private static " + ABSClass.class.getName() + " instance;");
        stream.println("public static " + ABSClass.class.getName() + " instantiate() {");

        stream.println("if (instance == null) {");

        stream.println("instance = new " + ABSClass.class.getName() + "();");
        stream.println("instance.setName(\"" + decl.getName() + "\");");
        generateFields();
        // Constructor
        stream.println("instance.setConstructor(" + className + ".CON$TRUCT.instantiate());");
        stream.print("instance.setParams(");
        for (int i = 0; i < decl.getNumParam(); i++) {
            if (i != 0)
                stream.print(", ");
            stream.print("\"" + JavaBackend.getVariableName(decl.getParam(i).getName()) + "\"");
        }
        stream.println(");");
        for (MethodImpl m : decl.getMethods()) {
            // m.generateJava("   ", stream);
            String methodName = m.getMethodSig().getName();
            stream.println("instance.addMethod(\"" + methodName + "\", " + className + "." + methodName + ".instantiate());");
        }
        stream.println("}");
        stream.println("return instance;");
        stream.println("}");

        generateConstructor();

        for (MethodImpl m : decl.getMethods()) {
            stream.println("public static class " + m.getMethodSig().getName() + " extends " + ABSClosure.class.getName() + " {");
            stream.println("private static " + ABSClosure.class.getName() + " instance;");
            stream.println("public static " + ABSClosure.class.getName() + " instantiate() {");
            stream.println("if (instance == null) { instance = new " + m.getMethodSig().getName() + "(); }");
            stream.println("return instance;");
            stream.println("}");
            m.generateJavaDynamic(stream);
            stream.println("}");
        }

        // generateFieldNamesMethod();
        // generateFields();
        // generateConstructor();
        // generateGetFieldValueMethod();

        // stream.println("public final java.lang.String getClassName() { return \""+decl.getName()+"\"; }");

        generateCreateNewCOGMethod();
        generateNewObjectMethods();
        // generateMethods();
        stream.println("}");
    }

    private void generateNewObjectMethods() {
        // Convenience method for new C
        stream.print("public static final <T extends " + ABSDynamicObject.class.getName() + "> T createNewObject");
        DynamicJavaGeneratorHelper.generateParams(stream, decl.getParams());
        stream.print("{ ");
        stream.print("return (T)");
        stream.print(className + ".__ABS_createNewObject");
        DynamicJavaGeneratorHelper.generateParamArgs(stream, "null", decl.getParams());
        stream.println("; }");

        // static constructor method for new C
        stream.print("public static final <T extends " + ABSDynamicObject.class.getName() + "> T __ABS_createNewObject");
        DynamicJavaGeneratorHelper.generateParams(stream, ABSObject.class.getName() + " __ABS_source", decl.getParams());
        stream.println(" {");
        generateObjectConstruction(ABSRuntime.class.getName() + ".getCurrentRuntime()");
        stream.println("__ABS_result.__ABS_init();");
        if (decl.isActiveClass()) {
            stream.println("final " + Task.class.getName() + " __ABS_sendingTask = " + ABSRuntime.class.getName() + ".getCurrentTask();");
            stream.println(ABSRuntime.class.getName() + ".getCurrentRuntime().asyncCall(new "
                    + ABSRunMethodCall.class.getName() + "(__ABS_sendingTask,__ABS_source,__ABS_result));");
        }
        stream.println("return (T)__ABS_result;");
        stream.println("}");
    }

    private void generateCreateNewCOGMethod() {
        // Convenience method for new cog C
        stream.print("public static final <T extends " + ABSDynamicObject.class.getName() + "> T createNewCOG");
        DynamicJavaGeneratorHelper.generateParams(stream, decl.getParams());
        stream.println(" {");
        stream.print("return (T)" + className + ".__ABS_createNewCOG");
        DynamicJavaGeneratorHelper.generateParamArgs(stream, "null", decl.getParams());
        stream.println(";");
        stream.println("}");

        // static constructor method for new cog C
        stream.print("public static final <T extends " + ABSDynamicObject.class.getName() + "> T __ABS_createNewCOG");
        DynamicJavaGeneratorHelper.generateParams(stream, ABSObject.class.getName() + " __ABS_source", decl.getParams());
        stream.println(" {");
        stream.println("final " + ABSRuntime.class.getName() + " __ABS_runtime = " + ABSRuntime.class.getName() + ".getCurrentRuntime();");
        stream.println("final " + COG.class.getName() + " __ABS_cog = __ABS_runtime.createCOG(" + className + ".class);");
        stream.println("final " + ABSThread.class.getName() + " __ABS_thread = " + ABSRuntime.class.getName() + ".getCurrentThread();");
        stream.println("final " + COG.class.getName() + " __ABS_oldCOG = " + ABSRuntime.class.getName() + ".getCurrentCOG();");
        stream.println("final " + Task.class.getName() + " __ABS_sendingTask = " + ABSRuntime.class.getName() + ".getCurrentTask();");
        stream.println("__ABS_thread.setCOG(__ABS_cog);");
        stream.println("try {");
        generateObjectConstruction("__ABS_runtime");

        stream.println(";");
        stream.println("__ABS_runtime.cogCreated(__ABS_result);");
        stream.println("__ABS_cog.getScheduler().addTask(new " + Task.class.getName() + "(new "
                + ABSInitObjectCall.class.getName() + "(__ABS_sendingTask,__ABS_source,__ABS_result)));");

        if (decl.isActiveClass()) {
            stream.println("__ABS_runtime.asyncCall(new " + ABSRunMethodCall.class.getName()
                    + "(__ABS_sendingTask,__ABS_source,__ABS_result));");
        }
        stream.println("return (T)__ABS_result;");
        stream.println("} finally {");
        stream.println("__ABS_thread.setCOG(__ABS_oldCOG);");
        stream.println("}");
        stream.println("}");
    }

    private void generateObjectConstruction(String runtime) {
        stream.print(ABSDynamicObject.class.getName() + " __ABS_result = ");
        if (decl.isForeign()) {
            stream.println("(" + className + ") " + runtime + ".getForeignObject(\"" + decl.getModule().getName() + "."
                    + decl.getName() + "\");");
            stream.print("if (__ABS_result == null) __ABS_result = ");
        }

        stream.print("new " + ABSDynamicObject.class.getName());
        DynamicJavaGeneratorHelper.generateParamArgs(stream, className + ".instantiate()", decl.getParams());
        stream.println(";");

    }

    private void generateConstructor() {
        // constructor
        stream.println("public static class " + "CON$TRUCT" + " extends " + ABSClosure.class.getName() + " {");
        stream.println("private static " + ABSClosure.class.getName() + " instance;");
        stream.println("public static " + ABSClosure.class.getName() + " instantiate() {");
        stream.println("if (instance == null) { instance = new CON$TRUCT(); }");
        stream.println("return instance;");
        stream.println("}");
        stream.println("public " + ABSValue.class.getName() + " exec(final " + ABSDynamicObject.class.getName() + " thisP, "
                + ABSValue.class.getName() + "... args) {");

        for (FieldDecl f : decl.getFields()) {
            if (f.hasInitExp()) {
                stream.print("thisP.setFieldValue(\"");
                stream.print(JavaBackend.getVariableName(f.getName()));
                stream.print("\", ");
                f.getInitExp().generateJavaDynamic(stream);
                stream.println(");");
            }
        }
        if (decl.hasInitBlock()) {
            decl.getInitBlock().generateJavaDynamic(stream);
        }
        stream.println("return null;");
        stream.println("}");
        stream.println("}");
    }

    private void generateFields() {
        for (ParamDecl p : decl.getParams()) {
            stream.println("instance.addField(\"" + p.getName() + "\", new " + ABSField.class.getName() + "());");
            // stream.print("   private ");
            // p.generateJava(stream);
            // stream.print(";");
            // TODO: INITIALIZER
        }

        for (FieldDecl f : decl.getFields()) {
            stream.println("instance.addField(\"" + f.getName() + "\", new " + ABSField.class.getName() + "());");
            // f.generateJava("   private ", stream);
            // TODO: INITIALIZER
        }
    }

    private void generateClassHeader() {
        stream.print("public ");
        stream.print("class " + className + " ");
    }

}
