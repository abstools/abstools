/**
 * Copyright (c) 2014 Rudolf Schlatte. All rights reserved.
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.frontend.antlr.parser;

import java.io.*;

import org.antlr.v4.runtime.*;
import org.antlr.v4.runtime.tree.*;

import abs.common.Constants;
import abs.frontend.ast.*;
import abs.frontend.parser.ASTPreProcessor;

public class ABSParserWrapper {

    boolean raiseExceptions = false;
    boolean stdlib = true;
    File file = null;

    public ABSParserWrapper () {}

    public ABSParserWrapper (File file, boolean raiseExceptions, boolean stdlib)
    {
        this.file = file;
        this.raiseExceptions = raiseExceptions;
        this.stdlib = stdlib;
    }

    public CompilationUnit parse(Reader reader) throws IOException {
        String path = "<unknown path>";
        if (file != null) path = file.getPath();
        ANTLRInputStream input = new ANTLRInputStream(reader);
        abs.frontend.antlr.parser.ABSLexer lexer
            = new abs.frontend.antlr.parser.ABSLexer(input);
        CommonTokenStream tokens = new CommonTokenStream(lexer);
        abs.frontend.antlr.parser.ABSParser aparser
            = new abs.frontend.antlr.parser.ABSParser(tokens);
        SyntaxErrorCollector errorlistener
            = new SyntaxErrorCollector(file, raiseExceptions);
        aparser.removeErrorListeners();
        aparser.addErrorListener(errorlistener);
        ParseTree tree = aparser.goal();
        if (errorlistener.parserErrors.isEmpty()) {
            ParseTreeWalker walker = new ParseTreeWalker();
            CreateJastAddASTListener l = new CreateJastAddASTListener(file);
            walker.walk(l, tree);
            CompilationUnit u
                = new ASTPreProcessor().preprocess(l.getCompilationUnit());
            if (stdlib) {
                for (ModuleDecl d : u.getModuleDecls()) {
                    if (!Constants.STDLIB_NAME.equals(d.getName()))
                        d.getImports().add(new StarImport(Constants.STDLIB_NAME));
                }
            }
            return u;
        } else {
            @SuppressWarnings("rawtypes")
                CompilationUnit u = new CompilationUnit(path,new List(),new List(),new List(),new Opt(),new List(),new List(),new List());
            u.setParserErrors(errorlistener.parserErrors);
            return u;
        }
    }
}
