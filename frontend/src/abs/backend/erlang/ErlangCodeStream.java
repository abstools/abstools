/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.backend.erlang;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.PrintStream;
import java.io.UnsupportedEncodingException;

public class ErlangCodeStream extends PrintStream {
    private static final int INDENT_LENGTH = 4;
    private static final String INDENT1;
    private boolean newline = true;
    static {
        String s = "";
        for (int i = 0; i < INDENT_LENGTH; i++)
            s += " ";
        INDENT1 = s;
    }

    private String indent = "";

    public ErlangCodeStream(File file) throws FileNotFoundException, UnsupportedEncodingException {
        super(file, "UTF-8");
    }

    public ErlangCodeStream incIndent() {
        indent = indent + INDENT1;
        return this;
    }

    public ErlangCodeStream decIndent() {
        if (indent.length() >= INDENT_LENGTH)
            indent = indent.substring(0, indent.length() - INDENT_LENGTH);
        return this;
    }

    @Override
    public void print(String s) {
        if (newline) {
            super.print(indent);
            newline = false;
        }
        super.print(s);
    }

    @Override
    public void println(String s) {
        if (newline) {
            super.print(indent);
            newline = false;
        }
        super.println(s);
        newline = true;
    }

    @Override
    public void println() {
        if (newline) {
            super.print(indent);
            newline = false;
        }
        super.println();
        newline = true;
    }

    public void pf(String f, Object... o) {
        format(f, o);
        println();
    }

}
