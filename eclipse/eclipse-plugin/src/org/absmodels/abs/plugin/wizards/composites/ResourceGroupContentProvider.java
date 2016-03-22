/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.absmodels.abs.plugin.wizards.composites;

import static org.absmodels.abs.plugin.util.Constants.EMPTY_OBJECT_ARRAY;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import org.absmodels.abs.plugin.util.Constants;
import org.absmodels.abs.plugin.util.UtilityFunctions;
import org.eclipse.core.resources.IContainer;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IWorkspace;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.jface.viewers.ITreeContentProvider;
import org.eclipse.jface.viewers.Viewer;


/**
 * ContentProvider for the Resource group composite. Lists all projects of the workspace and 
 * their member files.
 * @author cseise
 *
 */
public class ResourceGroupContentProvider implements ITreeContentProvider {
	
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
			return getChildrenOf((IWorkspace) element);
		} else if (element instanceof IContainer) {	
			return getChildenOf((IContainer) element);
		}

		return EMPTY_OBJECT_ARRAY;
	}

	private static Object[] getChildenOf(IContainer container) {
		if (container.isAccessible()) {
			List<Object> children = new ArrayList<Object>();
			try {
				for (IResource res : container.members()) {
					if (res.isAccessible()) {
						if (res instanceof IFile){
							IFile file = (IFile) res;
							if (UtilityFunctions.hasABSFileExtension(file)){
								children.add(res);
							}
						}else{
							children.add(res);
						}
					}
				}
			} catch (CoreException ce) {

			}
			return children.toArray();
		}
		
		return EMPTY_OBJECT_ARRAY;
	}

	private static Object[] getChildrenOf(IWorkspace workspace) {
		List<IProject> projects = new ArrayList<IProject>();
		Collections.addAll(projects, workspace.getRoot().getProjects());

		List<IProject> projectsToRemove = new ArrayList<IProject>();

		// filter out all projects that do not have an ABSNature

		for (IProject p : projects) {
			try {
				if (p.isAccessible() && !p.hasNature(Constants.NATURE_ID)) {
					projectsToRemove.add(p);
				}
			} catch (CoreException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
		}
		
		projects.removeAll(projectsToRemove);

		return projects.toArray();
	}

	@Override
	public Object getParent(Object element) {
        if (element instanceof IResource) {
			return ((IResource) element).getParent();
		}
        return null;
	}

	@Override
	public boolean hasChildren(Object element) {
		return getChildren(element).length > 0;
	}

}
