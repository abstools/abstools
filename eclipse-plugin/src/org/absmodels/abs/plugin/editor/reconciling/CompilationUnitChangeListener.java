package org.absmodels.abs.plugin.editor.reconciling;

import abs.frontend.ast.CompilationUnit;

public interface CompilationUnitChangeListener {
    void onCompilationUnitChange(CompilationUnit newCu);
}
