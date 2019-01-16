/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved.
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.abs_models.backend.java.codegeneration;

import java.io.BufferedOutputStream;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.OutputStream;
import java.io.PrintStream;
import java.io.UnsupportedEncodingException;

import com.google.common.base.Strings;

import org.abs_models.backend.java.JavaBackend;

public class JavaCodeStream extends PrintStream {

    private static final String LINE_SEPARATOR_PROPERTY = "line.separator";
    private static final int INDENT_LENGTH = 4;
    private static final String INDENT1 = Strings.repeat(" ", INDENT_LENGTH);

    private Boolean startNewLine = true;
    private String indent = "";

    private JavaCodeStream(OutputStream out) throws UnsupportedEncodingException {
        super(out, false, JavaBackend.CHARSET.name());
    }

    public static JavaCodeStream from(OutputStream out) {
        // Set the line separator to LF while the PrintStream is created so it will print LF on println() calls.
        System.setProperty(LINE_SEPARATOR_PROPERTY, "\n");
        try {
            return new JavaCodeStream(out);
        } catch (UnsupportedEncodingException e) {
            // the used charset must be supported by all Java runtime implementations
            throw new IllegalStateException(e);
        } finally {
            System.setProperty(LINE_SEPARATOR_PROPERTY, System.lineSeparator());
        }
    }

    public static JavaCodeStream from(File file) throws FileNotFoundException {
        return from(new BufferedOutputStream(new FileOutputStream(file)));
    }

    public void incIndent() {
        indent = indent + INDENT1;
    }

    public void decIndent() {
        if (indent.length() >= INDENT_LENGTH)
            indent = indent.substring(0, indent.length() - INDENT_LENGTH);
    }

    /*
     * print statement that tries to be smart about code indentation:
     * if the previous print statement ended with a line separator,
     * then it will indent the current line.
     *
     * @see java.io.PrintStream#print(java.lang.String)
     */
    @Override
    public void print(String s) {
        //System.err.println("=== " + indent.length() + " === " + s + " ===");
        if (startNewLine) {
            if (s.startsWith("}") || s.startsWith(")"))
                decIndent();
            super.print(indent);
        }


        super.print(s);
        startNewLine = false;
    }

    /*
     * println statement that tries to be smart about code indentation:
     * If string ends with "{", it will increase the indentation,
     * if string is equal to "}", it will decrease indentation.
     *
     * This to work requires some collaboration from the programmer:
     * - open braces should be the last character in a line of code, i.e. println("... {")
     * - closing braces should be the first character in a line of code, i.e. println("} ...")
     *
     * @see java.io.PrintStream#println(java.lang.String)
     */
    @Override
    public void println(String s) {
        //System.err.println("*** " + indent.length() + " *** " + s + " ***");
        if (s.startsWith("}") || s.equals(")"))
            decIndent();

        if (startNewLine)
            super.print(indent);
        super.print(s);
        super.println();
        startNewLine = true;

        if (s.endsWith("{") || s.endsWith("("))
            incIndent();
    }

    @Override
    public void println() {
        startNewLine = true;
        super.println();
    }
}
