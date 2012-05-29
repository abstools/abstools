/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.backend.java.codegeneration.dynamic;

import java.io.PrintStream;

public class CodeGenerator {
    final PrintStream stream;
    String indent; 
    CodeGenerator(String indent, PrintStream stream) {
        this.stream = stream;
        this.indent = indent;
    }
    
    public void generate() {
        
    }
    
    protected void incIndent() {
        indent = indent+"   ";
    }
    
    protected void decIndent() {
        indent = indent.substring(0, indent.length()-3);
    }
    
    protected void indentPrint(String s) {
        stream.print(indent);
        stream.print(s);
    }
    
    protected void indentPrintln(String s) {
        indentPrint(s);
        stream.println();
    }
    
    protected void print(String s) {
        stream.print(s);
    }
    
    protected void println(String s) {
        stream.println(s);
    }
}
