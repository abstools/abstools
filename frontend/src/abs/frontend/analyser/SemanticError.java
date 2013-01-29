/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.frontend.analyser;

import java.io.File;

import abs.common.CompilerError;
import abs.frontend.ast.ASTNode;
import abs.frontend.ast.Name;

public class SemanticError extends CompilerError {
    public final ErrorMessage msg;
    public final String[] args;
    public final ASTNode<?> node;

    protected SemanticError() {
        msg = null;
        args = null;
        node = null;
    }
    
    public SemanticError(ASTNode<?> node, ErrorMessage msg, String... args) {
        this.node = node;
        this.msg = msg;
        this.args = args;
    }

    public SemanticError(ASTNode<?> node, ErrorMessage msg, Name... args) {
        this.node = node;
        this.msg = msg;

        this.args = new String[args.length];
        for (int i = 0; i < args.length; i++) {
            this.args[i] = args[i].getString();
        }
    }

    @Override
    public String getFileName() {
        if (file == null) {
            String name = node.getFileName();
            if (name == null)
                return "<unkown>";
            file = new File(name);
        }
        return super.getFileName();
    }

    @Override
    public int getLine() {
        return node.getStartLine();
    }

    @Override
    public int getColumn() {
        return node.getStartColumn();
    }

    public ASTNode<?> getNode() {
        return node;
    }

    @Override
    public String getMessage() {
        return getMsg();
    }

    public String getMsg() {
        return msg.withArgs(args);
    }

    public String getMsgWithHint(String absCode) {
        return getHelpMessage() + "\n" + getHint(absCode);
    }

    public String getHint(String absCode) {
        int prevIndex = 0;
        int endIndex = -1;
        endIndex = absCode.indexOf('\n', prevIndex);
        for (int l = 1; l < getLine() && endIndex != -1; l++) {
            prevIndex = endIndex;
            endIndex = absCode.indexOf('\n', prevIndex);
        }
        String line = "";
        if (endIndex == -1)
            line = absCode.substring(prevIndex);
        else
            line = absCode.substring(prevIndex, endIndex);
        StringBuffer lineHint = new StringBuffer();
        for (int c = 0; c < getColumn() - 1; c++) {
            lineHint.append('-');
        }
        lineHint.append('^');
        return line + "\n" + lineHint;
    }
}
