/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.abs_models.backend.tests;

/**
 * A printer for {@link StringBasedABSTestRunnerGenerator}
 * @author pwong
 *
 */
public class TestRunnerScriptBuilder implements Appendable, CharSequence {
    
    private final StringBuilder builder = new StringBuilder();
    static final String INDENT = "\t";
    static final String NEWLINE = System.getProperty("line.separator");
    
    private StringBuilder indentation = new StringBuilder();
    private boolean toindent = false;

    TestRunnerScriptBuilder increaseIndent() { 
        indentation.append(INDENT); 
        return this; 
    }
    
    TestRunnerScriptBuilder newLine() {
        toindent = true;
        builder.append(NEWLINE);
        return this;
    }
    
    TestRunnerScriptBuilder decreaseIndent() { 
        indentation.deleteCharAt(indentation.length()-1); 
        return this; 
    }
    
    private void indent() {
        if (toindent) {
            builder.append(indentation);
            toindent = false;
        }
    }
    
    public TestRunnerScriptBuilder append(Object obj) {
        indent();
        builder.append(obj);
        return this;
    }
    
    @Override
    public TestRunnerScriptBuilder append(CharSequence csq) {
        indent();
        builder.append(csq);
        return this;
    }

    @Override
    public TestRunnerScriptBuilder append(CharSequence csq, int start, int end) {
        indent();
        builder.append(csq,start,end);
        return this;
    }

    @Override
    public TestRunnerScriptBuilder append(char c) {
        indent();
        builder.append(c); 
        return this;
    }

    @Override
    public int length() {
        return builder.length();
    }

    @Override
    public char charAt(int index) {
        return builder.charAt(index);
    }

    @Override
    public CharSequence subSequence(int start, int end) {
        return builder.subSequence(start, end);
    }
    
    @Override
    public String toString() {
        return builder.toString();
    }
    
}
