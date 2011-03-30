/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package eu.hatsproject.absplugin.wizards.composites;

import static eu.hatsproject.absplugin.util.Constants.EMPTY_OBJECT_ARRAY;

import java.util.ArrayList;
import java.util.Collection;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IWorkspace;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.jface.viewers.ITreeContentProvider;
import org.eclipse.jface.viewers.Viewer;

import abs.frontend.ast.Model;
import abs.frontend.ast.ModuleDecl;
import eu.hatsproject.absplugin.builder.AbsNature;
import eu.hatsproject.absplugin.util.Constants;
import eu.hatsproject.absplugin.util.InternalASTNode;
import eu.hatsproject.absplugin.util.UtilityFunctions;
import eu.hatsproject.absplugin.wizards.WizardUtil;

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
								if (m.getName().equals(Constants.ABS_STDLIB_ID)) {
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

	@SuppressWarnings("unchecked")
	@Override
	public Object getParent(Object element) {
        if (element instanceof IResource) {
			return ((IResource) element).getParent();
		}else if (element instanceof InternalASTNode && InternalASTNode.hasASTNodeOfType((InternalASTNode<?>) element, ModuleDecl.class)){
			return WizardUtil.getProjectOfModuleDecl((InternalASTNode<ModuleDecl>) element);
		}
        return null;
	}

	@Override
	public boolean hasChildren(Object element) {
		return getChildren(element).length > 0;
	}

}
