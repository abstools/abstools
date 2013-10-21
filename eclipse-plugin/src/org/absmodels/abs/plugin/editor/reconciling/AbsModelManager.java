package org.absmodels.abs.plugin.editor.reconciling;

import abs.frontend.ast.CompilationUnit;
import abs.frontend.ast.Model;

/**
 * Manages a model of an ABS project. The model can contain unsaved compilation units 
 * which are directly taken from the editor
 */
public interface AbsModelManager {

    void updateModel(CompilationUnit compilationUnit, boolean withTypechecks);

    Model getModel();

    CompilationUnit getCompilationUnit(String absoluteFilePath);

}
