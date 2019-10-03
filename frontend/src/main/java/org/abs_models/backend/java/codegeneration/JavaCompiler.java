/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved.
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.abs_models.backend.java.codegeneration;

import java.io.PrintWriter;
import java.io.StringWriter;

import com.google.common.collect.ObjectArrays;

import org.abs_models.backend.java.JavaBackend;
import org.eclipse.jdt.core.compiler.batch.BatchCompiler;

class JavaCompiler {
    private static final String[] DEFAULT_PREFIX = {"-encoding", JavaBackend.CHARSET.name(), " -source", "5", "-nowarn", "-noExit"};

    public static void main(String... args) throws JavaCodeGenerationException {
        if (!compile(args))
            System.exit(1);
    }

    public static boolean compile(String[] args) throws JavaCodeGenerationException {
        String[] allargs = ObjectArrays.concat(DEFAULT_PREFIX, args, String.class);
        StringWriter outWriter = new StringWriter();
        StringWriter errWriter = new StringWriter();
        boolean res = BatchCompiler.compile(allargs, new PrintWriter(outWriter), new PrintWriter(errWriter), null);
        if (!res) {
            String errorString = errWriter.toString();
            throw new JavaCodeGenerationException("There seems to be a bug in the ABS Java backend. " +
                    "The generated code contains errors:\n" + errorString);
        }
        return res;
    }
}
