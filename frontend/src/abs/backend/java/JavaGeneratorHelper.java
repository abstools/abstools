package abs.backend.java;

import java.io.PrintStream;

import abs.frontend.ast.List;
import abs.frontend.ast.ParamDecl;
import abs.frontend.ast.PureExp;

public class JavaGeneratorHelper {

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
    
    public static void generateParams(PrintStream stream, List<ParamDecl> params) {
        stream.print("(");
        boolean first = true;
        for (ParamDecl d : params) {
            if (!first)
                stream.print(", ");
            d.generateJava(stream);
            first = false;
        }
        stream.print(")");
    }
}
