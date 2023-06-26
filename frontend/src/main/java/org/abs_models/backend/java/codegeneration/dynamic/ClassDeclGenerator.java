/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.abs_models.backend.java.codegeneration.dynamic;

import java.io.PrintStream;
import org.abs_models.backend.java.JavaBackend;
import org.abs_models.backend.java.lib.runtime.ABSClosure;
import org.abs_models.backend.java.lib.runtime.ABSDynamicObject;
import org.abs_models.backend.java.lib.runtime.ABSDynamicRuntime;
import org.abs_models.backend.java.lib.runtime.ABSField;
import org.abs_models.backend.java.lib.runtime.ABSInitObjectCall;
import org.abs_models.backend.java.lib.runtime.ABSObject;
import org.abs_models.backend.java.lib.runtime.ABSRunMethodCall;
import org.abs_models.backend.java.lib.runtime.ABSRuntime;
import org.abs_models.backend.java.lib.runtime.ABSThread;
import org.abs_models.backend.java.lib.runtime.COG;
import org.abs_models.backend.java.lib.runtime.Task;
import org.abs_models.backend.java.lib.runtime.ABSDynamicClass;
import org.abs_models.backend.java.lib.types.ABSValue;
import org.abs_models.frontend.ast.ClassDecl;
import org.abs_models.frontend.ast.FieldDecl;
import org.abs_models.frontend.ast.MethodImpl;
import org.abs_models.frontend.ast.ParamDecl;

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

    private void generateClassHeader() {
        stream.print("public final class " + className + " ");
    }

    private void generateClassBody() {
        // singleton() method instantiates strictly one dynamic class object and does initialisation,
        // i.e. adds fields, constructor and methods.
        stream.println("{");
        stream.println("private static " + ABSDynamicClass.class.getName() + " instance;");
        stream.println("public static void setInstance(" + ABSDynamicClass.class.getName() + " cls) { instance = cls; }");
        stream.println("public static " + ABSDynamicClass.class.getName() + " singleton() {");
        stream.println("if (instance == null) {");
        stream.println("instance = new " + ABSDynamicClass.class.getName() + "();");
        stream.println("instance.setName(\"" + decl.getName() + "\");");

        // add class parameters
        for (ParamDecl p : decl.getParams()) {
            String name = JavaBackend.getVariableName(p.getName());
            stream.println("instance.addField(\"" + name + "\", new " + ABSField.class.getName() + "());");
        }
        
        // add fields
        for (FieldDecl f : decl.getFields()) {
            String name = JavaBackend.getVariableName(f.getName());
            stream.println("instance.addField(\"" + name + "\", " + className + ".field$" + name + ".singleton());");
        }

        // add constructor
        stream.println("instance.setConstructor(" + className + ".CON$TRUCT.singleton());");
        stream.print("instance.setParams(");
        for (int i = 0; i < decl.getNumParam(); i++) {
            if (i != 0)
                stream.print(", ");
            stream.print("\"" + JavaBackend.getVariableName(decl.getParam(i).getName()) + "\"");
        }
        stream.println(");");
        
        // add methods
        for (MethodImpl m : decl.getMethods()) {
            String methodName = m.getMethodSig().getName();
            stream.println("instance.addMethod(\"" + methodName + "\", " + className + "." + methodName + ".singleton());");
        }
        stream.println("}");
        stream.println("return instance;");
        stream.println("}");

        
        // generate field inner classes
        for (FieldDecl field : decl.getFields()) {
            field.generateJavaDynamic(stream);
        }
        
        // generate CON$TRUCT inner class
        generateConstructor();

        // generate method inner classes 
        for (MethodImpl method : decl.getMethods()) {
            method.generateJavaDynamic(stream);

            // FIXME not sure how to handle FLI methods
            if (method.isForeign()) {
                stream.println("/* FLI method: not implemented yet */");
                DynamicJavaGeneratorHelper.generateFLIMethod(stream, method);
            }
        }

        generateCreateNewCOGMethod();
        generateNewObjectMethods();
        stream.println("}");
        
    }

    private void generateNewObjectMethods() {
        // Convenience method for new C
        stream.print("public static final <T extends " + ABSDynamicObject.class.getName() + "> T createNewLocalObject");
        DynamicJavaGeneratorHelper.generateParams(stream, decl.getParams());
        stream.print("{ ");
        stream.print("return (T)");
        stream.print(className + ".__ABS_createNewLocalObject");
        DynamicJavaGeneratorHelper.generateParamArgs(stream, "null", decl.getParams());
        stream.println("; }");

        // static constructor method for new C
        stream.print("public static final <T extends " + ABSDynamicObject.class.getName() + "> T __ABS_createNewLocalObject");
        DynamicJavaGeneratorHelper.generateParams(stream, ABSObject.class.getName() + " __ABS_source", decl.getParams());
        stream.println(" {");
        stream.println("final " + ABSDynamicRuntime.class.getName() + " __ABS_runtime = " + ABSDynamicRuntime.class.getName() + ".getCurrentRuntime();");
        generateObjectConstruction(ABSRuntime.class.getName() + ".getCurrentRuntime()");
        stream.println("__ABS_result.__ABS_init();");
        if (decl.isActiveClass()) {
            stream.println("final " + Task.class.getName() + " __ABS_sendingTask = " + ABSRuntime.class.getName() + ".getCurrentTask();");
            stream.println(ABSRuntime.class.getName() + ".getCurrentRuntime().asyncCall(new "
                    + ABSRunMethodCall.class.getName() + "(__ABS_sendingTask,__ABS_source,__ABS_result));");
        }
        stream.println("__ABS_runtime.registerObject((T)__ABS_result);");
        stream.println("return (T)__ABS_result;");
        stream.println("}");
    }

    private void generateCreateNewCOGMethod() {
        // Convenience method for new cog C
        stream.print("public static final <T extends " + ABSDynamicObject.class.getName() + "> T createNewCogObject");
        DynamicJavaGeneratorHelper.generateParams(stream, decl.getParams());
        stream.println(" {");
        stream.print("return (T)" + className + ".__ABS_createNewCogObject");
        DynamicJavaGeneratorHelper.generateParamArgs(stream, "null", decl.getParams());
        stream.println(";");
        stream.println("}");

        // static constructor method for new cog C
        stream.print("public static final <T extends " + ABSDynamicObject.class.getName() + "> T __ABS_createNewCogObject");
        DynamicJavaGeneratorHelper.generateParams(stream, ABSObject.class.getName() + " __ABS_source", decl.getParams());
        stream.println(" {");
        stream.println("final " + ABSDynamicRuntime.class.getName() + " __ABS_runtime = " + ABSDynamicRuntime.class.getName() + ".getCurrentRuntime();");
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
        stream.println("__ABS_runtime.registerObject((T)__ABS_result);");
        stream.println("return (T)__ABS_result;");
        stream.println("} finally {");
        stream.println("__ABS_thread.setCOG(__ABS_oldCOG);");
        stream.println("}");
        stream.println("}");
    }

    private void generateObjectConstruction(String runtime) {
        stream.print(ABSDynamicObject.class.getName() + " __ABS_result = ");
        if (decl.isForeign()) {
            stream.println("(" + className + ") " + runtime + ".getForeignObject(\"" + decl.getModuleDecl().getName() + "."
                    + decl.getName() + "\");");
            stream.print("if (__ABS_result == null) __ABS_result = ");
        }

        stream.print("new " + ABSDynamicObject.class.getName());
        DynamicJavaGeneratorHelper.generateParamArgs(stream, className + ".singleton()", decl.getParams());
        stream.println(";");

    }

    private void generateConstructor() {
        // constructor
        stream.println("public static class " + "CON$TRUCT" + " extends " + ABSClosure.class.getName() + " {");
        stream.println("private static " + ABSClosure.class.getName() + " instance;");
        stream.println("public static " + ABSClosure.class.getName() + " singleton() {");
        stream.println("if (instance == null) { instance = new CON$TRUCT(); }");
        stream.println("return instance;");
        stream.println("}");
        stream.println("public " + ABSValue.class.getName() + " exec(final " + ABSDynamicObject.class.getName() + " thisP, "
                + ABSValue.class.getName() + "... args) {");

        
        // Fields and parameters are initialised in ABSDynamicObject::initializeFields(ABSValue[] params)
        // so this whole CON$TRUCT is probably only needed for the InitBlock
        
//        stream.println("// Initialise fields");
//        stream.println("for (String f : thisP.getFieldNames()) {");
//        stream.println("thisP.setFieldValue(f, thisP.getClazz().getField(f).init(thisP));");
//        stream.println("}");

        // initialise fields (old, static way)
//        for (FieldDecl f : decl.getFields()) {
//            if (f.hasInitExp()) {
//                stream.print("thisP.setFieldValue(\"");
//                stream.print(JavaBackend.getVariableName(f.getName()));
//                stream.print("\", ");
//                f.getInitExp().generateJavaDynamic(stream);
//                stream.println(");");
//            }
//        }

        if (decl.hasInitBlock()) {
            decl.getInitBlock().generateJavaDynamic(stream);
        }
        stream.println("return null;");
        stream.println("}");
        stream.println("}");
    }
}
