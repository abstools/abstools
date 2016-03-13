/**
 * Copyright (c) 2016, The Envisage Project. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */

package abs.frontend.typechecker.ext;

import abs.frontend.analyser.ErrorMessage;
import abs.frontend.analyser.SemanticWarning;
import abs.frontend.ast.*;

public class MainBlockChecker extends DefaultTypeSystemExtension {
    protected MainBlockChecker(Model m) {
        super(m);
    }

    @Override
    public void checkModel(Model model) {
        int nMainBlocks = 0;
        String moduleName = null;
        for (CompilationUnit u : model.getCompilationUnits()) {
            for (ModuleDecl m : u.getModuleDecls()) {
                if (m.hasBlock()) {
                    nMainBlocks = nMainBlocks + 1;
                }
            }
        }
        if (nMainBlocks == 0) {
            CompilationUnit c = model.getCompilationUnit(0);
            errors.add(new SemanticWarning(c, ErrorMessage.MAIN_BLOCK_NOT_FOUND, "dummy string to keep constructor happy"));
        } else if (nMainBlocks > 1) {
            Block b = model.getMainBlock();
            errors.add(new SemanticWarning(b, ErrorMessage.MAIN_BLOCK_AMBIGUOUS, ((ModuleDecl)(b.getParent().getParent())).getName()));
        }
    }
}

