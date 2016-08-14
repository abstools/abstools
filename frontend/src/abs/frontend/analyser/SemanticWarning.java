/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.frontend.analyser;

import abs.frontend.ast.ASTNode;
import abs.frontend.ast.Name;

public class SemanticWarning extends SemanticCondition {

    public SemanticWarning(ASTNode<?> node, ErrorMessage msg, String... args) {
        this.node = node;
        this.msg = msg;
        this.args = args;
    }

    public SemanticWarning(ASTNode<?> node, ErrorMessage msg, Name... args) {
        this.node = node;
        this.msg = msg;

        this.args = new String[args.length];
        for (int i = 0; i < args.length; i++) {
            this.args[i] = args[i].getString();
        }
    }

    @Override
    public boolean isError() {
        return false;
    }

}
