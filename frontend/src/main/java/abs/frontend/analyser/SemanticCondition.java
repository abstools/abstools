/**
 * Copyright (c) 2016, The Envisage Project. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.frontend.analyser;

import java.io.File;

import abs.common.CompilerCondition;
import abs.frontend.ast.ASTNode;

public abstract class SemanticCondition extends CompilerCondition {

    public ErrorMessage msg;
    public String[] args;
    public ASTNode<?> node;

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
