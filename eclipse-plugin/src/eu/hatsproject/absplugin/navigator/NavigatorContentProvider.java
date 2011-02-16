package eu.hatsproject.absplugin.navigator;

import java.util.ArrayList;
import java.util.List;

import static eu.hatsproject.absplugin.editor.outline.ABSContentOutlineUtils.getNatureForObject;
import static eu.hatsproject.absplugin.editor.outline.ABSContentOutlineUtils.getChildrenOf;
import static eu.hatsproject.absplugin.util.UtilityFunctions.getAbsNature;
import static eu.hatsproject.absplugin.util.Constants.EMPTY_OBJECT_ARRAY;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IWorkspaceRoot;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.jface.viewers.ITreeContentProvider;
import org.eclipse.jface.viewers.Viewer;

import abs.frontend.ast.ASTNode;
import abs.frontend.ast.Model;
import abs.frontend.ast.ModuleDecl;
import eu.hatsproject.absplugin.builder.AbsNature;
import eu.hatsproject.absplugin.editor.outline.ABSContentOutlineProvider;
import eu.hatsproject.absplugin.editor.outline.ABSContentOutlineUtils;
import eu.hatsproject.absplugin.util.Constants;
import eu.hatsproject.absplugin.util.InternalASTNode;

public class NavigatorContentProvider implements ITreeContentProvider {
	
	ABSContentOutlineProvider outlineProvider = new ABSContentOutlineProvider();
	
	@Override
	public Object[] getChildren(Object parentElement) {

		if (parentElement instanceof InternalASTNode) {
			return outlineProvider.getChildren(((InternalASTNode<?>) parentElement));
		} else if (parentElement instanceof IProject) {
			if (((IProject) parentElement).isOpen()) {
				AbsNature nature = getNatureForObject(parentElement);
				return ModulePath.getRootHierarchy(nature).toArray();
			}
		} else if (parentElement instanceof ModulePath) {
			ModulePath mPath = (ModulePath) parentElement;
			ArrayList<Object> children = new ArrayList<Object>();

			children.addAll(mPath.getChildModulePathsAndModuleDecls());
			InternalASTNode<ModuleDecl> intNode = mPath.getModuleDecl();
			
			//if the module path has a matching module declaration unfold its content..
			if (intNode != null) {
				ModuleDecl md = (ModuleDecl) intNode.getASTNode();
				ArrayList<ASTNode<?>> chld = getChildrenOf(md);
				ASTNode<?>[] chldArr = chld.toArray(new ASTNode<?>[chld.size()]);
				
				List<InternalASTNode<ASTNode<?>>> wrapASTNodes =
					InternalASTNode.wrapASTNodes(chldArr, mPath.getNature());
				
				children.addAll(wrapASTNodes);
				return (children.toArray());
			}
			return children.toArray();
		}

		return EMPTY_OBJECT_ARRAY;
	}

	@Override
	public Object[] getElements(Object inputElement) {
				if (inputElement instanceof InternalASTNode ||  inputElement instanceof IProject) {
						return getChildren(inputElement);
				} else if (inputElement instanceof Object[]) {
					return (Object[]) inputElement;
				} else if (inputElement instanceof IWorkspaceRoot){
					return ((IWorkspaceRoot)inputElement).getProjects();
				}
		return EMPTY_OBJECT_ARRAY;
	}

	@Override
	public boolean hasChildren(java.lang.Object element) {
		AbsNature nature = ABSContentOutlineUtils.getNatureForObject(element);

		if (nature != null) {
			
			if (element instanceof InternalASTNode || element instanceof IFile){
				return outlineProvider.hasChildren(element);
			} else if (element instanceof IProject){
				return hasChildren((IProject) element);
			} else if (element instanceof IWorkspaceRoot){
				return ((IWorkspaceRoot)element).getProjects().length > 0;
			} else if (element instanceof ModulePath){
				return ((ModulePath) element).getChildModulePathsAndModuleDecls().size() > 0;
			}	
		}
		return false;

	}
	
	private boolean hasChildren(IProject proj) {
		try {
			if (proj.hasNature(Constants.NATURE_ID) && proj.isOpen()) {
				AbsNature nature = getAbsNature(proj);

				if (nature != null) {

					Model model = nature.getCompleteModel();

					if (model != null) {
						return model.getModuleDecls().size() > 0;
					}
				}
			}
		} catch (CoreException e) {
			return false;
		}

		return false;
	}

	@Override
	public void dispose() {
		//no-op
	}

	@Override
	public Object getParent(Object element) {
		if (element instanceof ASTNode) {
			return ((ASTNode<?>) element).getParent();
		}
		return null;
	}

	@Override
	public void inputChanged(Viewer viewer, Object oldInput, Object newInput) {
		
	}

}
