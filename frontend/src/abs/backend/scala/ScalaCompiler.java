/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.backend.scala;

import java.io.File;
import java.io.PrintWriter;

import abs.backend.java.utils.StringUtil;
import abs.common.StringUtils;
import abs.frontend.ast.MethodSig;
import abs.frontend.ast.Model;
import abs.frontend.ast.ParamDecl;
import abs.frontend.parser.Main;

/**
 * 
 * @author Andri Saar <andri@cs.ioc.ee>
 *
 */
public class ScalaCompiler extends Main {
    public static void main(final String... args) {
        try {
            new ScalaCompiler().compile(args);
        }
        catch (Exception e) {
            e.printStackTrace(System.err);
            System.exit(1);
        }
    }

    public void compile(String[] args) throws Exception {
        final Model model = parse(args);
        
        if (model.hasParserErrors() || model.hasErrors() || model.hasTypeErrors())
            return;
        
        File outputDir = new File("/tmp/output");
        outputDir.mkdirs();

        model.generateScala(outputDir);
    }
    
    
    public static final void generateMessages(Iterable<MethodSig> methods, PrintWriter writer) {
        writer.println("\tsealed trait Message");
        for (MethodSig s : methods) {
                // run method is special, that will be handled by MyObject.Run
                if (s.getName().equals("run"))
                        continue;
  
                writer.format("\tcase %s %s",
                        (s.getNumParam() == 0 ? "object" : "class"),
                        StringUtils.capitalize(s.getName()));
                
                if (s.getNumParam() > 0) {
                        boolean f = false;
                        
                        writer.write("(");
                        
                        for (ParamDecl param: s.getParams()) {
                                if (f)
                                        writer.write(", ");
                                writer.write(param.getName() + ": "); param.getAccess().generateScala("", writer);                                  
                                f = true;
                        }
                        
                        writer.write(")");
                }
                
                writer.println(" extends Message");
        }
    }
}
