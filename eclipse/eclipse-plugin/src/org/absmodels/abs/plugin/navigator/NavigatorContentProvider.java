/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.absmodels.abs.plugin.navigator;

import java.util.ArrayList;
import java.util.List;

import static org.absmodels.abs.plugin.editor.outline.ABSContentOutlineUtils.getChildrenOf;
import static org.absmodels.abs.plugin.util.UtilityFunctions.getAbsNature;

import org.absmodels.abs.plugin.Activator;
import org.absmodels.abs.plugin.builder.AbsNature;
import org.absmodels.abs.plugin.editor.outline.ABSContentOutlineProvider;
import org.absmodels.abs.plugin.util.Constants;
import org.absmodels.abs.plugin.util.InternalASTNode;
import org.absmodels.abs.plugin.util.UtilityFunctions;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.jface.viewers.ITreeContentProvider;
import org.eclipse.ui.model.WorkbenchContentProvider;

import abs.frontend.ast.ASTNode;
import abs.frontend.ast.Model;
import abs.frontend.ast.ModuleDecl;

public class NavigatorContentProvider extends WorkbenchContentProvider implements ITreeContentProvider {
	
	final ABSContentOutlineProvider outlineProvider = new ABSContentOutlineProvider();
	
	@Override
	public Object[] getChildren(Object parentElement) {

		if (parentElement instanceof InternalASTNode) {
			return outlineProvider.getChildren(parentElement);
		} else
		    if (parentElement instanceof IProject) {
			if (((IProject) parentElement).isOpen()) {
				AbsNature nature = UtilityFunctions.getAbsNature((IProject)parentElement);
				assert nature != null;
				return ModulePath.getRootHierarchy(nature).toArray();
			}
		} else if (parentElement instanceof ModulePath) {
			ModulePath mPath = (ModulePath) parentElement;
			ArrayList<Object> children = new ArrayList<Object>();

			children.addAll(mPath.getChildModulePathsAndModuleDecls());
			InternalASTNode<ModuleDecl> intNode = mPath.getModuleDecl();
			
			//if the module path has a matching module declaration unfold its content..
			if (intNode != null) {
				ModuleDecl md = intNode.getASTNode();
				ArrayList<ASTNode<?>> chld = getChildrenOf(md);
				ASTNode<?>[] chldArr = chld.toArray(new ASTNode<?>[chld.size()]);
				
				List<InternalASTNode<ASTNode<?>>> wrapASTNodes =
					InternalASTNode.wrapASTNodes(chldArr, mPath.getNature());
				
				children.addAll(wrapASTNodes);
				return (children.toArray());
			}
			return children.toArray();
		}
//
//		return new String[]{"1", " 2", "  3"};
	
		return super.getChildren(parentElement);
	}

	@Override
	public Object[] getElements(Object inputElement) {
		return getChildren(inputElement);
	}

	@Override
	public boolean hasChildren(java.lang.Object element) {
	    return (getChildren(element).length >= 1);
	    
//	    System.out.println(element);
//	    
//		AbsNature nature = ABSContentOutlineUtils.getNatureForObject(element);
//
//		if (nature != null) {
//			
//			if (element instanceof InternalASTNode || element instanceof IFile){
//				return outlineProvider.hasChildren(element);
//			} else if (element instanceof IProject){
//				return hasChildren((IProject) element);
//			} else if (element instanceof IWorkspaceRoot){
//				return ((IWorkspaceRoot)element).getProjects().length > 0;
//			} else if (element instanceof ModulePath){
//				return ((ModulePath) element).getChildModulePathsAndModuleDecls().size() > 0;
//			}	
//		}
//		return false;

	}
	
	private boolean hasChildren(IProject proj) {
		try {
			if (proj.isOpen() && proj.hasNature(Constants.NATURE_ID)) {
				AbsNature nature = getAbsNature(proj);

				if (nature != null) {

					Model model = nature.getCompleteModel();

					if (model != null) {
						return model.getModuleDecls().size() > 0;
					}
				}
			}
		} catch (CoreException e) {
			Activator.logException(e);
			return false;
		}

		return false;
	}

	@Override
	public void dispose() {
	    super.dispose();
		//no-op
	}

//	@Override
//	public Object getParent(Object element) {
//		if (element instanceof ASTNode) {
//			return ((ASTNode<?>) element).getParent();
//		}
//		return null;
//	}
//
//	@Override
//	public void inputChanged(Viewer viewer, Object oldInput, Object newInput) {
//		
//	}

}
