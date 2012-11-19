/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.backend.java.codegeneration;

import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.IOException;
import java.io.OutputStream;
import java.io.PrintStream;
import java.lang.reflect.Method;
import abs.backend.java.JavaBackend;
import abs.backend.java.JavaBackendConstants;
import abs.backend.java.lib.runtime.ABSBuiltInFunctions;
import abs.backend.java.lib.runtime.ABSFut;
import abs.backend.java.lib.runtime.ABSRuntime;
import abs.backend.java.lib.runtime.AbstractAsyncCall;
import abs.backend.java.lib.runtime.Task;
import abs.backend.java.lib.types.ABSBool;
import abs.backend.java.lib.types.ABSValue;
import abs.backend.java.scheduling.SimpleTaskScheduler;
import abs.backend.java.scheduling.TaskScheduler;
import abs.backend.java.scheduling.TaskSchedulingStrategy;
import abs.common.Constants;
import abs.common.Position;
import abs.frontend.ast.ASTNode;
import abs.frontend.ast.Annotation;
import abs.frontend.ast.AsyncCall;
import abs.frontend.ast.AwaitStmt;
import abs.frontend.ast.ClassDecl;
import abs.frontend.ast.Cog;
import abs.frontend.ast.DataTypeUse;
import abs.frontend.ast.Decl;
import abs.frontend.ast.ExpGuard;
import abs.frontend.ast.FnApp;
import abs.frontend.ast.FunctionDecl;
import abs.frontend.ast.HasTypeParameters;
import abs.frontend.ast.LetExp;
import abs.frontend.ast.List;
import abs.frontend.ast.MethodImpl;
import abs.frontend.ast.MethodSig;
import abs.frontend.ast.ParamDecl;
import abs.frontend.ast.ParametricDataTypeDecl;
import abs.frontend.ast.ParametricFunctionDecl;
import abs.frontend.ast.PureExp;
import abs.frontend.ast.ReturnStmt;
import abs.frontend.ast.ThisExp;
import abs.frontend.ast.TypeParameterDecl;
import abs.frontend.ast.TypedAnnotation;
import abs.frontend.ast.TypedVarOrFieldDecl;
import abs.frontend.ast.VarDecl;
import abs.frontend.ast.VarOrFieldDecl;
import abs.frontend.ast.VarUse;
import abs.frontend.typechecker.Type;
import abs.frontend.typechecker.TypeCheckerHelper;

public class JavaGeneratorHelper {

    private static final String FLI_METHOD_PREFIX = "fli_";

    public static void generateHelpLine(ASTNode<?> node, PrintStream stream) {
        Position pos = new Position(node);
        stream.println("// " + pos.getPositionString());
    }

    public static void generateArgs(PrintStream stream, List<PureExp> args, java.util.List<Type> types) {
        generateArgs(stream, null, args, types);
    }

    public static void generateArgs(PrintStream stream, String firstArg, List<PureExp> args, java.util.List<Type> types) {
        stream.print("(");
        boolean first = true;

        if (firstArg != null) {
            stream.print(firstArg);
            first = false;
        }

        for (int i = 0; i < args.getNumChild(); i++) {
            PureExp e = args.getChild(i);
            if (!first)
                stream.print(", ");
            e.generateJava(stream);
            if (types.get(i).isIntType() && e.getType().isRatType())
                stream.print(".truncate()");
            first = false;
        }
        stream.print(")");
    }

    public static void generateParamArgs(PrintStream stream, List<ParamDecl> params) {
        generateParamArgs(stream,null,params);
    }
    public static void generateParamArgs(PrintStream stream, String firstArg, List<ParamDecl> params) {
        stream.print("(");
        boolean first = true;
        
        if (firstArg != null) {
            stream.print(firstArg);
            first = false;
        }

        for (ParamDecl d : params) {
            if (!first)
                stream.print(", ");
            stream.print(JavaBackend.getVariableName(d.getName()));
            first = false;
        }
        stream.print(")");
    }

    public static void generateParams(PrintStream stream, List<ParamDecl> params) {
        generateParams(stream, null, params);
    }

    public static void generateParams(PrintStream stream, String firstArg, List<ParamDecl> params) {
        stream.print("(");

        boolean first = true;
        if (firstArg != null) {
            stream.print(firstArg);
            first = false;
        }

        for (ParamDecl d : params) {
            if (!first)
                stream.print(", ");
            // stream.print("final ");
            d.generateJava(stream);
            first = false;
        }
        stream.print(")");
    }

    public static void generateTypeParameters(PrintStream stream, Decl dtd,
            boolean plusExtends) {
        List<TypeParameterDecl> typeParams = null;
        if (dtd instanceof HasTypeParameters) {
            typeParams = ((HasTypeParameters)dtd).getTypeParameters();
        } else
            return;
        if (typeParams.getNumChild() > 0) {
            stream.print("<");
            boolean isFirst = true;
            for (TypeParameterDecl d : typeParams) {
                if (isFirst)
                    isFirst = false;
                else
                    stream.print(",");
                stream.print(d.getName());
                if (plusExtends)
                    stream.print(" extends " + ABSValue.class.getName());
            }
            stream.print(">");
        }
    }

    public static void generateBuiltInFnApp(PrintStream stream, FnApp app) {
        FunctionDecl d = (FunctionDecl) app.getDecl();
        String name = d.getName();
        if (!builtInFunctionExists(name)) {
            throw new RuntimeException("The built in function '" + name + "' is not implemented in the Java backend.");
        }
        stream.print(ABSBuiltInFunctions.class.getName() + "." + name);
        String firstArgs = null;
        if (Constants.isFunctionalBreakPointFunctionName(d.getModuleDecl().getName() + "." + name))
            firstArgs = generateFunctionalBreakPointArgs(app);
        generateArgs(stream, firstArgs, app.getParams(), d.getTypes());
    }

    private static String generateFunctionalBreakPointArgs(FnApp app) {
        return new StringBuilder("\"").append(app.getCompilationUnit().getFileName().replace("\\", "\\\\"))
                .append("\", ").append(app.getStartLine()).toString();
    }

    private static boolean builtInFunctionExists(String name) {
        for (Method m : ABSBuiltInFunctions.class.getMethods()) {
            if (m.getName().equals(name)) {
                return true;
            }
        }
        return false;
    }

    public static String getDebugString(ASTNode<?> node) {
        return getDebugString(node, node.getStartLine());
    }

    public static String getDebugString(ASTNode<?> node, int line) {
        String fileName = node.getCompilationUnit().getFileName().replace("\\", "\\\\");
        return "if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep(\""
                + fileName + "\"," + line + ");";
    }
    
    public static void generateMethodSig(String indent, PrintStream stream, MethodSig sig, boolean async) {
        generateMethodSig(indent, stream, sig, async, "", "");
    }
    
    public static void generateMethodSig(String indent, PrintStream stream, MethodSig sig, boolean async, String modifier, String prefix) {
       JavaGeneratorHelper.generateHelpLine(sig,stream);
       stream.print(indent+"public "+modifier+" ");
       if (async) {
           prefix = "async_";
           stream.print(ABSFut.class.getName()+"<");
       }
       
       sig.getReturnType().generateJava(stream);
       
       if (async)
           stream.print(">");
       stream.print(" "+prefix+JavaBackend.getMethodName(sig.getName()));
       JavaGeneratorHelper.generateParams(stream, sig.getParams());
    }
    
    public static void generateAsyncMethod(String indent, PrintStream stream, MethodImpl method) {
       final MethodSig sig = method.getMethodSig();
       generateMethodSig(indent,stream,sig,true,"final","");
       stream.println("{ return ("+ABSFut.class.getName()+")");
       generateAsyncCall(stream, "this", null, method.getContextDecl().getType(), null, sig.getParams(), sig);
       stream.println(indent+"; }");
    }
    
    
    public static void generateAsyncCall(PrintStream stream, AsyncCall call) {
       final PureExp callee = call.getCallee();
       final List<PureExp> params = call.getParams();
       final MethodSig sig = call.getMethodSig();
       
       generateAsyncCall(stream, null, callee, callee.getType(), params, null, sig);
    }

    private static void generateAsyncCall(PrintStream stream, final String calleeString, 
          final PureExp callee, final Type calleeType, final List<PureExp> args, 
          final List<ParamDecl> params,
          final MethodSig sig) 
    {
        final java.util.List<Type> paramTypes
            = sig.getTypes();
        stream.print(ABSRuntime.class.getName()+".getCurrentRuntime().asyncCall(");
        String targetType = JavaBackend.getQualifiedString(calleeType);
        stream.print("new "+AbstractAsyncCall.class.getName()+"<"+targetType+">(this,");
        if (callee instanceof ThisExp) {
            if (calleeString != null)
                stream.print(calleeString);
            else
                callee.generateJava(stream);
        } else { 
            stream.print(ABSRuntime.class.getName()+".checkForNull(");
            if (calleeString != null)
                stream.print(calleeString);
            else
                callee.generateJava(stream);
            stream.print(')');
        }
        stream.print(") {");
        int i = 0;
        for (Type t : paramTypes) {
            stream.print(JavaBackend.getQualifiedString(t)+" arg"+i+";");
            i++;
        }

        generateTaskGetArgsMethod(stream, paramTypes.size());
        generateTaskInitMethod(stream, paramTypes);

        stream.print(" public java.lang.String methodName() { return \""+sig.getName()+"\"; }");

        stream.print(" public Object execute() {");
        stream.print(" return target."+JavaBackend.getMethodName(sig.getName())+"(");
        for (i = 0; i < paramTypes.size(); i++) {
            if (i > 0) stream.print(",");
            stream.print("arg"+i);
            if (paramTypes.get(i).isIntType()) stream.print(".truncate()");
        }
        stream.print(");");
        stream.println(" }}");
        stream.print("     .init");
        if (args != null)
            JavaGeneratorHelper.generateArgs(stream,args, paramTypes);
        else
            JavaGeneratorHelper.generateParamArgs(stream, params);
        stream.print(")");
    }

    private static void generateTaskInitMethod(PrintStream stream, final java.util.List<Type> paramTypes) {
        int i;
        stream.print("    public "+abs.backend.java.lib.runtime.AsyncCall.class.getName()+"<?> init(");
           i = 0;
           for (Type t : paramTypes) {
               if (i > 0) stream.print(",");
               stream.print(JavaBackend.getQualifiedString(t)+" _arg"+i);
               i++;
           }
           stream.print(") {");
           for (i = 0; i < paramTypes.size(); i++) {
               stream.print("arg"+i+" = _arg"+i+";");
           }
           stream.print(" return this; }");
    }

    private static void generateTaskGetArgsMethod(PrintStream stream, final int n) {
        stream.print(" public java.util.List<"+ABSValue.class.getName()+"> getArgs() { return java.util.Arrays.asList(new "+ABSValue.class.getName()+"[] { ");
           generateArgStringList(stream, n);
           stream.print(" }); } ");
    }

    private static void generateArgStringList(PrintStream stream, final int n) {
        for (int i = 0; i < n; i++) {
               if (i > 0) stream.print(",");
               stream.print("arg"+i);
           }
    }

    
    public static void generateClassDecl(PrintStream stream, final ClassDecl decl) {
        new ClassDeclGenerator("", stream, decl).generate();
    }
    
    public static void generateMethodImpl(String indent, PrintStream stream, final MethodImpl m) {
      // Async variant
      JavaGeneratorHelper.generateAsyncMethod(indent,stream,m);

        // Sync variant
      generateMethodSig(indent,stream,m.getMethodSig(),false,"final","");
      generateMethodBody(indent, stream, m, false);
      
      if (m.isForeign()) {
          generateFLIMethod(indent,stream,m);
      }      
        
    }

    public static void generateMethodBody(String indent, PrintStream stream, final MethodImpl m, boolean isFliMethod) {
        boolean addReturn = false;
        if (m.getMethodSig().getReturnType().getType().isUnitType()) {
          if (m.getBlock().getNumStmt() == 0 ||
              (! (m.getBlock().getStmt(m.getBlock().getNumStmt()-1) instanceof ReturnStmt))) {
            addReturn = true;
          }
        }

        stream.println("{ __ABS_checkSameCOG(); ");
          
          if (!isFliMethod && m.isForeign()) {
              stream.print(indent+"return this.");
              stream.print(FLI_METHOD_PREFIX+JavaBackend.getMethodName(m.getMethodSig().getName()));
              JavaGeneratorHelper.generateParamArgs(stream, m.getMethodSig().getParams());
              stream.println(";");
          } else {
              stream.println("    if (__ABS_getRuntime().debuggingEnabled()) {");
              stream.println("       "+Task.class.getName()+"<?> __ABS_currentTask = __ABS_getRuntime().getCurrentTask();");
              stream.println("       __ABS_currentTask.newStackFrame(this,\""+m.getMethodSig().getName()+"\");");
              for (ParamDecl d : m.getMethodSig().getParams()) {
                  stream.print("     __ABS_currentTask.setLocalVariable(");
                  stream.println("\""+d.getName()+"\","+d.getName()+");");
              }
              stream.println("    }");
              m.getBlock().generateJava(indent, stream, addReturn);
          
          }
          stream.println("}");
    }

    private static void generateFLIMethod(String indent, PrintStream stream, MethodImpl m) {
        generateMethodSig(indent, stream, m.getMethodSig(), false, "", FLI_METHOD_PREFIX);
        generateMethodBody(indent,stream,m, true);
    }

    /**
     * removes the gen folder and all its contents
     * @param code
     */
    public static void cleanGenFolder(JavaCode code) {
        File genDir = code.getSrcDir(); 
        cleanGenFolderRecursively(genDir);
    }

    private static void cleanGenFolderRecursively(File dir) {
        if (dir == null) throw new IllegalArgumentException();
        if (!dir.exists()) return;
        for (File f : dir.listFiles()) {
            if (f.isDirectory()) {
                cleanGenFolderRecursively(f);
            } else {
                if (f.getAbsolutePath().endsWith(".java")
                        || f.getAbsolutePath().endsWith(".class")) {
                    f.delete();
                }
            }
        }
        dir.delete();
    }

    public static void printEscapedString(PrintStream stream, String content) {
        for (int i=0; i<content.length(); i++) {
            char c = content.charAt(i);
            switch (c) {
            case '\t':
                stream.append('\\').append('t');
                break;
            case '\b':
                stream.append('\\').append('b');
                break;
            case '\n':
                stream.append('\\').append('n');
                break;
            case '\r':
                stream.append('\\').append('r');
                break;
            case '\f':
                stream.append('\\').append('f');
                break;
            case '\'':
                stream.append('\\').append('\'');
                break;
            case '\"':
                stream.append('\\').append('\"');
                break;
            case '\\':
                stream.append('\\').append('\\');
                break;
            default:
                stream.append(c);
            }
        }
    }

    public static void generateExprGuard(ExpGuard expGuard, PrintStream beforeAwaitStream, PrintStream stream) {
        PureExp expr = expGuard.getPureExp();
        
        replaceLocalVariables((PureExp)expr.copy(), beforeAwaitStream);
        
        stream.print("new "+JavaBackendConstants.EXPGUARD+"() { public "+ABSBool.class.getName()+" evaluateExp() { return ");
        expGuard.getPureExp().generateJava(stream);
        stream.print("; }}");
    }
    
    /**
     * replace all uses of local variables and parameters by a use of a newly introduced
     * temporary final local variable 
     */
    private static void replaceLocalVariables(ASTNode<?> astNode, PrintStream beforeAwaitStream) {
        if (isLocalVarUse(astNode)) {
            VarUse v = (VarUse) astNode;
            replaceVarUse(beforeAwaitStream, v, (TypedVarOrFieldDecl) v.getDecl());                
        } else {
            // process children:
            for (int i=0; i < astNode.getNumChild(); i++) {
                ASTNode<?> child = astNode.getChild(i);
                replaceLocalVariables(child, beforeAwaitStream);
            }
        }
    }

    /***
     * checks if astNode is a use of a local variable or parameter
     */
    private static boolean isLocalVarUse(ASTNode<?> astNode) {
        if (astNode instanceof VarUse) {
            VarUse v = (VarUse) astNode;
            VarOrFieldDecl decl = v.getDecl();
            if (decl instanceof VarDecl || decl instanceof ParamDecl) {
                return !(decl.getParent() instanceof LetExp);
            }
        }
        return false;
    }

    
    private static long tempCounter = 0;
    
    /**
     * replaces a varUse v of the local variable vDecl by a new temporary variable, which will be
     * written to beforeAwaitStream      
     */
    private static void replaceVarUse(PrintStream beforeAwaitStream, VarUse v, TypedVarOrFieldDecl vDecl) {
        String name = JavaBackend.getVariableName(vDecl.getName());
        String tempName = "temp$"+tempCounter+"$"+name;
        tempCounter = Math.max(tempCounter+1, 0);
        // copy value of variable to temporary, final variable
        beforeAwaitStream.print("final ");
        vDecl.getAccess().generateJava(beforeAwaitStream);
        beforeAwaitStream.print(" " + tempName+" = "+name+";");
        // replace variable name with name of temporary variable
        v.setName(tempName);
    }

    public static void generateAwaitStmt(AwaitStmt awaitStmt, String indent, PrintStream stream) {
        OutputStream exprOStream = new ByteArrayOutputStream();
        PrintStream exprStream = new PrintStream(exprOStream);
        // Necessary temporary variables are written to "stream" and the 
        // await-expression is written to exprStream
        awaitStmt.getGuard().generateJavaGuard(stream, exprStream);
        stream.print(indent+JavaBackendConstants.ABSRUNTIME+".await(");
        stream.print(exprOStream.toString());
        stream.println(");");
    }

    public static void generateCog(PrintStream stream, Cog cog) {
        
        // generate user-defined scheduler class if annotation is present
        for (Annotation a : cog.getAnnotationList()) {
            if (a instanceof TypedAnnotation) {
                TypedAnnotation ta = (TypedAnnotation)a;
                if (((DataTypeUse)ta.getAccess()).getName().equals("Scheduler")) {
                    generateUserSchedulingStrategy(cog, ta.getValue());
                }
            }
        }
    }
    
    public static void generateUserSchedulingStrategy(Cog cog, PureExp exp) {
        String className = JavaBackend.getSchedulerName("UserSchedulingStrategy_" + JavaBackend.getRandomName());
        PrintStream stream = null;
        try {
            JavaCode.Package pkg = cog.getModuleDecl().getJavaPackage();
            File file = pkg.createJavaFile(className);
            stream = new JavaCodeStream(file);

            stream.println("package " + pkg.packageName + ";");
            stream.print("public final class " + className);
            stream.println(" implements " + TaskSchedulingStrategy.class.getName() + " {");
            stream.print("public " + SimpleTaskScheduler.TaskInfo.class.getCanonicalName());
            stream.print(" schedule(" + TaskScheduler.class.getName() + " scheduler, ");
            stream.println(java.util.List.class.getName() + "<" + SimpleTaskScheduler.TaskInfo.class.getCanonicalName() + "> schedulableTasks) {");
            stream.println("");
            
            stream.println(java.util.List.class.getName() + "<ABS.Scheduler.Process> queue = new " + java.util.List.class.getName() + "<ABS.Scheduler.Process>();");
            stream.println("// TODO map List<TaskInfo> schedulableTasks --> List<Process> queue");
            stream.println("");
            stream.print("ABS.Scheduler.Process process = ");
            exp.generateJava(stream);
            stream.println(";");
            
            stream.println("");
            stream.println(SimpleTaskScheduler.TaskInfo.class.getCanonicalName() +  " task = new " + SimpleTaskScheduler.TaskInfo.class.getCanonicalName() + "();");
            stream.println("// TODO map ABS.Scheduler.Process process --> TaskInfo task");
            stream.println("return task;");
            stream.println("}");
            stream.println("}");

            // TODO 
            // connect generated TaskSchedulingStrategy to the cog's TaskScheduler
            
            // TODO
            // reflect the runtime queue in the functional layer (i.e. a queue datatype)

        } catch (JavaCodeGenerationException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        } catch (IOException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        } finally {
            if (stream != null)
                stream.close();
        }
    }

}
