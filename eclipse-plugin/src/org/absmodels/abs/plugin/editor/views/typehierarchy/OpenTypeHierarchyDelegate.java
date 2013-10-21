package org.absmodels.abs.plugin.editor.views.typehierarchy;

import org.absmodels.abs.plugin.editor.ABSEditor;
import org.absmodels.abs.plugin.util.InternalASTNode;
import org.absmodels.abs.plugin.util.UtilityFunctions;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.TextSelection;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.ui.IEditorActionDelegate;
import org.eclipse.ui.IEditorPart;

import abs.frontend.ast.ASTNode;
import abs.frontend.ast.CompilationUnit;

/**
 * delegate to open the type hierarchy for the type under the cursor 
 */
public class OpenTypeHierarchyDelegate implements IEditorActionDelegate {
    private IEditorPart editor;
    
    
    @Override
    public void run(IAction action) {
        if(!(editor instanceof ABSEditor)){
            return;
        }
        ABSEditor abseditor = (ABSEditor) editor;
        TextSelection sel = (TextSelection)abseditor.getSelectionProvider().getSelection();

        TypeHierarchyView view = TypeHierarchyView.get();
        if (view == null) {
            return;
        }
        InternalASTNode<?> node = getNodeUnderCursor(abseditor, sel);
        view.setNode(node);
        view.bringToTop();
    }
    
    InternalASTNode<?> getNodeUnderCursor(ABSEditor abseditor, TextSelection sel) {
        InternalASTNode<CompilationUnit> cu = abseditor.getCompilationUnit();
        if (cu == null) {
            return null;
        }
        IDocument doc = abseditor.getDocumentProvider().getDocument(editor.getEditorInput());
        synchronized (cu.getNature()) {
            try {
                ASTNode<?> node = UtilityFunctions.getASTNodeOfOffset(doc, cu.getASTNode(), sel.getOffset());
                return new InternalASTNode(node, cu.getNature());
            } catch (BadLocationException e) {
                e.printStackTrace();
            }
        }
        return null;
    }

    @Override
    public void selectionChanged(IAction action, ISelection selection) {
    }

    @Override
    public void setActiveEditor(IAction action, IEditorPart targetEditor) {
        this.editor = targetEditor;
    }

}
