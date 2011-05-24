/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.backend.java;

import java.io.PrintStream;

import beaver.Symbol;

import abs.backend.java.debugging.DebugPosition;
import abs.backend.java.lib.runtime.ABSBuiltInFunctions;
import abs.backend.java.lib.runtime.ABSRuntime;
import abs.backend.java.lib.runtime.Config;
import abs.backend.java.lib.types.ABSDataType;
import abs.backend.java.lib.types.ABSType;
import abs.backend.java.lib.types.ABSValue;
import abs.common.Position;
import abs.frontend.ast.ASTNode;
import abs.frontend.ast.DataTypeDecl;
import abs.frontend.ast.Decl;
import abs.frontend.ast.FnApp;
import abs.frontend.ast.FunctionDecl;
import abs.frontend.ast.List;
import abs.frontend.ast.ParamDecl;
import abs.frontend.ast.ParametricDataTypeDecl;
import abs.frontend.ast.ParametricFunctionDecl;
import abs.frontend.ast.PureExp;
import abs.frontend.ast.Stmt;
import abs.frontend.ast.TypeParameterDecl;

public class JavaGeneratorHelper {

    public static void generateHelpLine(ASTNode<?> node, PrintStream stream) {
        Position pos = new Position(node);
        stream.println("// " + pos.getPositionString());
    }

    public static void generateArgs(PrintStream stream, List<PureExp> args) {
        generateArgs(stream, null, args);
    }

    public static void generateArgs(PrintStream stream, String firstArg, List<PureExp> args) {
        stream.print("(");
        boolean first = true;

        if (firstArg != null) {
            stream.print(firstArg);
            first = false;
        }

        for (PureExp e : args) {
            if (!first)
                stream.print(", ");

            e.generateJava(stream);
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
        if (dtd instanceof ParametricDataTypeDecl) {
            typeParams = ((ParametricDataTypeDecl)dtd).getTypeParameters();
        } else
        if (dtd instanceof ParametricFunctionDecl) {
            typeParams = ((ParametricFunctionDecl)dtd).getTypeParameters();
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
        stream.print(ABSBuiltInFunctions.class.getName() + "." + name);
        generateArgs(stream, app.getParams());
    }

    public static String getDebugString(Stmt stmt) {
        return getDebugString(stmt, stmt.getStart());
    }

    public static String getDebugString(Stmt stmt, int pos) {
        int line = Symbol.getLine(pos);
        String fileName = stmt.getCompilationUnit().getFileName().replace("\\", "\\\\");
        return "if (__ABS_getRuntime().debuggingEnabled()) __ABS_getRuntime().nextStep(\""
                + fileName + "\"," + line + ");";
    }

}
