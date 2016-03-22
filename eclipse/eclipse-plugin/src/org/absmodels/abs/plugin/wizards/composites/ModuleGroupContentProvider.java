/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.absmodels.abs.plugin.wizards.composites;

import static org.absmodels.abs.plugin.util.Constants.EMPTY_OBJECT_ARRAY;

import java.util.ArrayList;
import java.util.Collection;

import org.absmodels.abs.plugin.builder.AbsNature;
import org.absmodels.abs.plugin.util.Constants;
import org.absmodels.abs.plugin.util.InternalASTNode;
import org.absmodels.abs.plugin.util.UtilityFunctions;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IWorkspace;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.jface.viewers.ITreeContentProvider;
import org.eclipse.jface.viewers.Viewer;

import abs.frontend.ast.Model;
import abs.frontend.ast.ModuleDecl;

public class ModuleGroupContentProvider implements ITreeContentProvider {

	@Override
	public void dispose() {
		//no-op
	}

	@Override
	public void inputChanged(Viewer viewer, Object oldInput, Object newInput) {
		//no-op
	}

	@Override
	public Object[] getElements(Object inputElement) {
		return getChildren(inputElement);
	}

	@Override
	public Object[] getChildren(Object element) {
		if (element instanceof IWorkspace) {
			return ((IWorkspace) element).getRoot().getProjects();
		} else if (element instanceof IProject){
			return getChildrenOf((IProject) element);
		}
					
		return EMPTY_OBJECT_ARRAY;
	}

	/**
	 * Returns the children of a project.
	 * 
	 * @param project
	 * @return The children of the Project or an empty object array if the
	 *         project is closed or not an ABS project or an Exception occurs
	 */
	private Object[] getChildrenOf(IProject project) {
		try{
			if (project.isAccessible() && project.hasNature(Constants.NATURE_ID)){
				AbsNature nature = UtilityFunctions.getAbsNature(project);
				if (nature != null){
					synchronized(nature.modelLock){
						ArrayList<InternalASTNode<?>> decls = new ArrayList<InternalASTNode<?>>();
						Model model = nature.getCompleteModel();
						
						//something could be compiled in the project (i.e. the project is not empty...)
						if (model != null) {
							Collection<ModuleDecl> moduleDecls = model.getModuleDecls();

							for (ModuleDecl m : moduleDecls) {
								String name = m.getName();
								/* Don't show internal resources which would be read-only anyway.
								 * This is either the standard lib, or e.g. ABS.FLI, ABS.DC */
								if (name.startsWith("ABS.")) {
									continue;
								}
								decls.add(new InternalASTNode<ModuleDecl>(m,nature));
							}
						}
						return decls.toArray();
					}
				}
			}				
		}catch(CoreException ce){
			ce.printStackTrace();
			return EMPTY_OBJECT_ARRAY;
		}
		
		return EMPTY_OBJECT_ARRAY;
	}

	@Override
	public Object getParent(Object element) {
        if (element instanceof IResource) {
			return ((IResource) element).getParent();
		}else if (element instanceof InternalASTNode){
			return ((InternalASTNode<?>) element).getProject();
		}
        return null;
	}

	@Override
	public boolean hasChildren(Object element) {
		return getChildren(element).length > 0;
	}

}
