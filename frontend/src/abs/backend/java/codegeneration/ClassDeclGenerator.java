/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.backend.java.codegeneration;

import java.io.PrintStream;
import java.util.ArrayList;

import abs.backend.java.JavaBackend;
import abs.backend.java.lib.runtime.ABSInitObjectCall;
import abs.backend.java.lib.runtime.ABSObject;
import abs.backend.java.lib.runtime.ABSRunMethodCall;
import abs.backend.java.lib.runtime.ABSRuntime;
import abs.backend.java.lib.runtime.ABSThread;
import abs.backend.java.lib.runtime.COG;
import abs.backend.java.lib.runtime.Task;
import abs.backend.java.lib.types.ABSClass;
import abs.backend.java.lib.types.ABSValue;
import abs.backend.java.scheduling.UserSchedulingStrategy;
import abs.frontend.ast.ClassDecl;
import abs.frontend.ast.FieldDecl;
import abs.frontend.ast.InterfaceTypeUse;
import abs.frontend.ast.MethodImpl;
import abs.frontend.ast.ParamDecl;
import abs.frontend.typechecker.InterfaceType;

public class ClassDeclGenerator {

    private final ClassDecl decl;
    private final String className;
    protected final PrintStream stream;

    public ClassDeclGenerator(PrintStream stream, ClassDecl decl) {
        this.stream = stream;
        this.decl = decl;
        className = JavaBackend.getClassName(decl.getName());
    }

    public void generate() {
        JavaGeneratorHelper.generateHelpLine(decl, stream);
        generateClassHeader();
        generateClassBody();
    }

    private void generateClassBody() {
        stream.println(" {");

        generateFieldNamesMethod();
        generateFields();
        generateConstructor();
        generateGetFieldValueMethod();

        stream.println("public final java.lang.String getClassName() { return \"" + decl.getName() + "\"; }");

        generateCreateNewCOGMethod();
        generateNewObjectMethods();
        generateMethods();
        stream.println("}");
    }

    private void generateMethods() {
        // methods
        for (MethodImpl m : decl.getMethods()) {
            m.generateJava(stream);
        }
    }

    private void generateNewObjectMethods() {
        // Convenience method for new C
        stream.print("public static final <T extends " + className + "> T createNewObject");
        JavaGeneratorHelper.generateParams(stream, decl.getParams());
        stream.print(" { return (T)");
        stream.print(className + ".__ABS_createNewObject");
        JavaGeneratorHelper.generateParamArgs(stream, "null", decl.getParams());
        stream.println("; }");

        // static constructor method for new C
        stream.print("public static final <T extends " + className + "> T __ABS_createNewObject");
        JavaGeneratorHelper.generateParams(stream, ABSObject.class.getName() + " __ABS_source", decl.getParams());
        stream.println(" {");
        generateObjectConstruction(ABSRuntime.class.getName() + ".getCurrentRuntime()");
        stream.println("__ABS_result.__ABS_init();");
        if (decl.isActiveClass()) {
            stream.println("final " + Task.class.getName() + " __ABS_sendingTask = " + ABSRuntime.class.getName() + ".getCurrentTask();");
            stream.println(ABSRuntime.class.getName() + ".getCurrentRuntime().asyncCall(new " + ABSRunMethodCall.class.getName() + "(__ABS_sendingTask,__ABS_source,__ABS_result));");
        }
        stream.println("return (T)__ABS_result;");
        stream.println("}");
    }

    private void generateCreateNewCOGMethod() {
        // Convenience method for new cog C
        stream.print("public static final <T extends " + className + "> T createNewCOG");
        JavaGeneratorHelper.generateParams(stream, decl.getParams());
        stream.print(" { ");
        stream.print("return (T)");
        stream.print(className + ".__ABS_createNewCOG");
        // FIXME: does createNewCog() need the DC argument?  Who calls this
        // method?
        JavaGeneratorHelper.generateParamArgs(stream, "null, null, null", decl.getParams());
        stream.println("; }");

        // static constructor method for new cog C    
        stream.print("public static final <T extends " + className + "> T __ABS_createNewCOG");
        JavaGeneratorHelper.generateParams(stream, 
                ABSObject.class.getName() + " __ABS_source, "
                + UserSchedulingStrategy.class.getName() + " Strategy, "
                + "ABS.DC.DeploymentComponent_i DC",
                decl.getParams());
        stream.println(" {");
        stream.println("final " + ABSRuntime.class.getName() + " __ABS_runtime = " + ABSRuntime.class.getName() + ".getCurrentRuntime();");
        stream.println("final " + COG.class.getName() + " __ABS_cog = Strategy == null ? __ABS_runtime.createCOG(" + className + ".class, DC) : __ABS_runtime.createCOG(" + className + ".class, DC, Strategy);");
        stream.println("final " + ABSThread.class.getName() + " __ABS_thread = " + ABSRuntime.class.getName() + ".getCurrentThread();");
        stream.println("final " + COG.class.getName() + " __ABS_oldCOG = " + ABSRuntime.class.getName() + ".getCurrentCOG();");
        stream.println("final " + Task.class.getName() + " __ABS_sendingTask = " + ABSRuntime.class.getName() + ".getCurrentTask();");
        stream.println("__ABS_thread.setCOG(__ABS_cog);");
        stream.println("try {");
        generateObjectConstruction("__ABS_runtime");

        stream.println(";");
        stream.println("__ABS_runtime.cogCreated(__ABS_result);");
        stream.println("__ABS_cog.getScheduler().addTask(new " + Task.class.getName() + "(new " + 
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
        stream.println("protected final " + ABSValue.class.getName() + " getFieldValue(java.lang.String __ABS_fieldName) throws java.lang.NoSuchFieldException {");
        for (ParamDecl p : decl.getParams()) {
            stream.println("if (\"" + p.getName() + "\".equals(__ABS_fieldName)) return " + JavaBackend.getVariableName(p.getName()) + ";");
        }

        for (FieldDecl f : decl.getFields()) {
            stream.println("if (\"" + f.getName() + "\".equals(__ABS_fieldName)) return " + JavaBackend.getVariableName(f.getName()) + ";");
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

        stream.print("private static final java.lang.String[] __fieldNames = new java.lang.String[] { ");

        boolean first = true;
        for (String fieldName : fieldNames) {
            if (first) first = false;
            else stream.print(", ");
            stream.print("\"" + fieldName + "\"");
        }
        stream.println(" };");

        stream.println("public final java.util.List<java.lang.String> getFieldNames() { return java.util.Arrays.asList(__fieldNames); }");
    }

    private java.util.List<String> getFieldNames() {
        java.util.List<String> fieldNames = new ArrayList<String>();
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
