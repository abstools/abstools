/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.backend.java.codegeneration;

import java.util.Collection;


import AST.*;

/**
 * This is a standard Java compiler implemented from JastAddJ it takes Java
 * source files and generates JVM bytecode.
 * 
 * @author Jan Schäfer
 * 
 */
class JavaCompiler extends Frontend {
    private Collection errors;

    public static void main(String... args) {
        if (!compile(args))
            System.exit(1);
    }

    public static boolean compile(JavaCode code) {
        return compile(code.getFileNames());
    }

    public static boolean compile(String... args) {
        JavaCompiler compiler = new JavaCompiler();
        boolean res = compiler.process(args, new BytecodeParser(), new JavaParser() {
            public CompilationUnit parse(java.io.InputStream is, String fileName) throws java.io.IOException,
                    beaver.Parser.Exception {
                return new parser.JavaParser().parse(is, fileName);
            }
        });
        if (compiler.errors != null) {
            throw new RuntimeException(compiler.errors.iterator().next().toString());
        }
        return res;
    }

    @Override
    protected void processErrors(Collection errors, CompilationUnit unit) {
        this.errors = errors;
        super.processErrors(errors, unit);
    }

    protected void processNoErrors(CompilationUnit unit) {
        unit.transformation();
        unit.generateClassfile();
    }

    protected String name() {
        return "Java5Compiler";
    }

    protected String version() {
        return "R20071015";
    }
}
