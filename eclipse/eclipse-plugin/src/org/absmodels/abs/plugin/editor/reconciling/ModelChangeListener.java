package org.absmodels.abs.plugin.editor.reconciling;

import abs.frontend.ast.Model;

public interface ModelChangeListener {
    void onModelChange(Model newModel);
}
