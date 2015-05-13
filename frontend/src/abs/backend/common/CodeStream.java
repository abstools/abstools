/**
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.backend.common;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.OutputStream;
import java.io.PrintStream;
import java.io.UnsupportedEncodingException;

/**
 * Provides stream, which tracks indentation
 * 
 * @author Georg Göri
 */
public class CodeStream extends PrintStream {
    private static final String INDENT = "    ";
    private boolean newline = true;

    private String indent = "";

    public CodeStream(File file) throws FileNotFoundException, UnsupportedEncodingException {
        super(file, "UTF-8");
    }

    public CodeStream(OutputStream out, String initIndent) {
        super(out);
        indent = initIndent;
    }

    public String getIndent() {
        return indent;
    }

    public void setIndent(String s) {
        this.indent = s;
    }

    public CodeStream incIndent() {
        indent = indent + INDENT;
        return this;
    }

    public CodeStream decIndent() {
        if (indent.length() > 0)
            indent = indent.substring(0, indent.length() - INDENT.length());
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
