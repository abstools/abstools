package eu.hatsproject.absplugin.editor.reconciling;

import abs.frontend.ast.CompilationUnit;

public interface CompilationUnitChangeListener {
    void onCompilationUnitChange(CompilationUnit newCu);
}
