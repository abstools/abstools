package abs.backend.java;

import java.io.PrintStream;

import beaver.Symbol;

import abs.backend.java.debugging.DebugPosition;
import abs.backend.java.lib.runtime.ABSBuiltInFunctions;
import abs.backend.java.lib.runtime.Config;
import abs.backend.java.lib.types.ABSDataType;
import abs.backend.java.lib.types.ABSType;
import abs.backend.java.lib.types.ABSValue;
import abs.common.Position;
import abs.frontend.ast.ASTNode;
import abs.frontend.ast.FnApp;
import abs.frontend.ast.FunctionDecl;
import abs.frontend.ast.List;
import abs.frontend.ast.ParamDecl;
import abs.frontend.ast.PureExp;
import abs.frontend.ast.Stmt;
import abs.frontend.ast.TypeParameterDecl;

public class JavaGeneratorHelper {

    public static void generateHelpLine(ASTNode<?> node, PrintStream stream) {
        Position pos = new Position(node);
        stream.println("// "+pos.getPositionString());
    }
    
    public static void generateArgs(PrintStream stream, List<PureExp> args) {
        stream.print("(");
        boolean first = true;
        for (PureExp e : args) {
          if (!first) 
              stream.print(", ");

           e.generateJava(stream);
          first = false;
        }
        stream.print(")");
        
    }

    public static void generateParamArgs(PrintStream stream, List<ParamDecl> params) {
        stream.print("(");
        boolean first = true;
        for (ParamDecl d : params) {
            if (!first)
                stream.print(", ");
            stream.print(JavaBackend.getVariableName(d.getName()));
            first = false;
        }
        stream.print(")");
    }
    
    public static void generateParams(PrintStream stream, List<ParamDecl> params) {
        stream.print("(");
        boolean first = true;
        for (ParamDecl d : params) {
            if (!first)
                stream.print(", ");
            //stream.print("final ");
            d.generateJava(stream);
            first = false;
        }
        stream.print(")");
    }
    
    public static void generateTypeParameters(PrintStream stream, List<TypeParameterDecl> typeParams, boolean plusExtends) {
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
				  stream.print(" extends "+ABSValue.class.getName());
		  }  
		  stream.print(">");
	  }	
    }
    
    public static void generateBuiltInFnApp(PrintStream stream, FnApp app) {
        FunctionDecl d = (FunctionDecl) app.getDecl();
        String name = d.getName();
        stream.print(ABSBuiltInFunctions.class.getName()+"."+name);
        generateArgs(stream, app.getParams());
    }
    
    public static String getDebugString(Stmt stmt) {
        return getDebugString(stmt, stmt.getStart());
    }
        
    public static String getDebugString(Stmt stmt, int pos) {
        int line = Symbol.getLine(pos);
        String fileName = stmt.getFileName();
        return "if ("+Config.class.getName()+".DEBUGGING) "+JavaBackendConstants.ABSRUNTIME+".nextStep(\""+fileName+"\","+line+");";
    }

}
