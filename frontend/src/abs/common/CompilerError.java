/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.common;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.Reader;
import java.io.StringReader;

public abstract class CompilerError {

    protected File file;
    private String sourceCode;

    public void setSourceLine(String line) {
        sourceLine = line;
    }

    public String getHelpMessage() {
        StringBuilder helpMessage = new StringBuilder();
        if (getFileName() != null) {
            helpMessage.append(getFileName() + ":");
        }

        helpMessage.append(getLine() + ":" + getColumn() + ":" + getMessage());

        final String sourceLine = getSourceLine();
        if (sourceLine != null) {

            final String lineWithoutTabs = replaceTabs(sourceLine);
            helpMessage.append("\n" + lineWithoutTabs + "\n");

            for (int c = 0; c < getColumn() - 1; c++) {
                helpMessage.append('-');
            }

            int ntabs = countTabs(sourceLine, getColumn());
            for (int i = 0; i < ntabs; i++) {
                helpMessage.append("---");
            }

            helpMessage.append('^');

        }

        return helpMessage.toString();
    }

    private int countTabs(String string, int upToChar) {
        int ntabs = 0;
        for (int i = 0; i < string.length(); i++) {
            if (i > upToChar)
                return ntabs;
            if (string.charAt(i) == '\t')
                ntabs++;
        }
        return 0;
    }

    public String getFileName() {
        if (file != null)
            return file.toString();
        return null;
    }

    // source line containing the error, for caching reasons, derived from
    // sourceCode
    private String sourceLine;

    private String getSourceLine() {
        if (sourceLine == null) {
            if (file != null && file.canRead()) {
                sourceLine = readLineFromFile();
            }

            if (sourceCode != null) {
                sourceLine = readLineFromSource();
            }
        }

        return sourceLine;
    }

    private String replaceTabs(String string) {
        return string.replaceAll("\\t", "    ");
    }

    private String readLineFromSource() {
        return readLineFromReader(new StringReader(sourceCode));
    }

    private String readLineFromFile() {
        try {
            return readLineFromReader(new FileReader(file));
        } catch (FileNotFoundException e) {
            e.printStackTrace();
            return null;
        }
    }

    private String readLineFromReader(Reader r) {
        try {
            BufferedReader reader = new BufferedReader(r);
            String res = null;
            for (int i = 0; i < getLine(); i++) {
                res = reader.readLine();
            }
            return res;

        } catch (Exception e) {
            e.printStackTrace();
            return null;
        }
    }

    public void setFile(String fileName) {
        this.file = new File(fileName);
    }

    public void setFile(File file) {
        this.file = file;
    }

    public void setSourceCode(String sourceCode) {
        this.sourceCode = sourceCode;
    }

    public abstract String getMessage();

    public abstract int getColumn();

    public abstract int getLine();
    
    @Override
    public String toString() {
        StringBuffer buf = new StringBuffer();
        // Surprise: according to the docs, new StringBuffer(null) is different from new StringBuffer().append(null)
        buf.append(getFileName());
        buf.append('@');
        buf.append(getLine());
        buf.append(':');
        buf.append(getMessage());
        return buf.toString();
    }
}
