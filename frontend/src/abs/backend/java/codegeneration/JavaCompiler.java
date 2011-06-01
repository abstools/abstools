/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.backend.java.codegeneration;

import java.io.PrintWriter;
import java.io.StringWriter;

import org.eclipse.jdt.core.compiler.batch.BatchCompiler;

class JavaCompiler {
    private static final String DEFAULT_PREFIX = "-source 5 -nowarn -noExit ";

    public static void main(String... args) {
        if (!compile(args))
            System.exit(1);
    }

    public static boolean compile(JavaCode code) {
        return compile(code.getFileNames());
    }

    public static boolean compile(String[] args) {
        StringBuffer sb = new StringBuffer();
        for (String s : args) {
            sb.append(s+" ");
        }
        return compile(sb.toString());
    }
    
    public static boolean compile(String args) {
        StringWriter outWriter = new StringWriter();
        StringWriter errWriter = new StringWriter();
        String errorString = null;
        boolean res = BatchCompiler.compile(DEFAULT_PREFIX + args, new PrintWriter(outWriter), new PrintWriter(errWriter), null);
        if (!res) {
            errorString = errWriter.toString();
            throw new RuntimeException(errorString);
        }
        return res;
    }
}
