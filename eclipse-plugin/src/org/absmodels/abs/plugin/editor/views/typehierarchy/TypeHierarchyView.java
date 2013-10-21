package org.absmodels.abs.plugin.editor.views.typehierarchy;

import org.absmodels.abs.plugin.editor.AbsHyperlinkDetector;
import org.absmodels.abs.plugin.editor.outline.ABSContentOutlineStyledLabelProvider;
import org.absmodels.abs.plugin.util.Constants;
import org.absmodels.abs.plugin.util.InternalASTNode;
import org.absmodels.abs.plugin.util.UtilityFunctions;
import org.absmodels.abs.plugin.util.UtilityFunctions.EditorPosition;
import org.eclipse.core.resources.IProject;
import org.eclipse.jface.viewers.DoubleClickEvent;
import org.eclipse.jface.viewers.IDoubleClickListener;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.TreeSelection;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.IViewPart;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.part.ViewPart;

import abs.frontend.ast.ASTNode;
import abs.frontend.ast.ClassDecl;
import abs.frontend.ast.NewExp;
import abs.frontend.ast.TypeDecl;
import abs.frontend.ast.TypeUse;
import abs.frontend.typechecker.Type;

/**
 * Display a type hierarchies in a tree viewer.
 * Jumps to the declaration of the type, when it is doubleclicked. 
 */
public class TypeHierarchyView extends ViewPart implements IDoubleClickListener {

    private TreeViewer treeViewer;
    private IProject project;

    @Override
    public void createPartControl(Composite parent) {
        treeViewer = new TreeViewer(parent);
        treeViewer.setContentProvider(new TypeHierarchyContentProvider());
        treeViewer.setLabelProvider(new ABSContentOutlineStyledLabelProvider());
        treeViewer.setAutoExpandLevel(2);
        treeViewer.addDoubleClickListener(this);
    }

    @Override
    public void setFocus() {
    }

    /**
     * return the current view. Opens the view if it is not found. 
     */
    public static TypeHierarchyView get() {
        IWorkbenchPage activePage = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getActivePage();
        IViewPart view = activePage.findView(Constants.ABS_TYPE_HIERARCHY_VIEW);
        if (view instanceof TypeHierarchyView) {
            return (TypeHierarchyView) view;
        }
        try {
            return (TypeHierarchyView) activePage.showView(Constants.ABS_TYPE_HIERARCHY_VIEW);
        } catch (PartInitException e) {
            e.printStackTrace();
            return null;
        }
    }

    public void setNode(InternalASTNode<?> inode) {
        project = inode.getProject();
        Object node = inode.getASTNode();
        if (node instanceof ClassDecl) {
            setTreeViewerInput(((ClassDecl) node).getType());
        } else if (node instanceof TypeDecl) {
            setTreeViewerInput(((TypeDecl) node).getType());
        } else if (node instanceof TypeUse) {
            setTreeViewerInput(((TypeUse) node).getType());
        } else if (node instanceof NewExp) {
            setTreeViewerInput(((NewExp) node).getType());
        } else {
            UtilityFunctions.showErrorMessage("Can not show type hierarchy for element under cursor: " 
                    + node.getClass().getSimpleName());
        }
    }

    private void setTreeViewerInput(Type type) {
        treeViewer.setInput(type);
    }

    @Override
    public void doubleClick(DoubleClickEvent event) {
        // when an element is doubleclicked, jump to its declaration
        ISelection selected = treeViewer.getSelection();
        if (selected instanceof TreeSelection) {
            TreeSelection treeSelection = (TreeSelection) selected;
            Object elem = treeSelection.getFirstElement();
            if (elem instanceof ASTNode<?>) {
                ASTNode<?> node = (ASTNode<?>) elem;
                EditorPosition pos = AbsHyperlinkDetector.getPosition(node);
                UtilityFunctions.jumpToPosition(project, pos);
            }
        }
    }

    /**
     * bring this view to the top
     */
    public void bringToTop() {
        IWorkbenchPage activePage = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getActivePage();
        activePage.bringToTop(this);
    }


}
