package eu.hatsproject.absplugin.editor.reconciling;

import abs.frontend.ast.Model;

public interface ModelChangeListener {
    void onModelChange(Model newModel);
}
