/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.absmodels.abs.plugin.decorators;

import static org.absmodels.abs.plugin.util.Constants.MODULE_DECORATOR_ID;
import static org.absmodels.abs.plugin.util.Images.FOLDER_MARKER;

import java.util.ArrayList;

import org.absmodels.abs.plugin.Activator;
import org.absmodels.abs.plugin.util.UtilityFunctions;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.viewers.DecorationOverlayIcon;
import org.eclipse.jface.viewers.IDecoration;
import org.eclipse.jface.viewers.ILightweightLabelDecorator;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.jface.viewers.LabelProviderChangedEvent;
import org.eclipse.ui.PlatformUI;


/**
 * This Decorator is used for decorating folders with the ABS project icon. Only
 * those folders containing ABS files or contain folders with ABS files are
 * decorated
 * 
 * @author cseise
 * 
 */
public class FolderDecorator extends LabelProvider implements ILightweightLabelDecorator {

	private final static ImageDescriptor MARKER_DESCRIPTOR = DecorationOverlayIcon.createFromImage(FOLDER_MARKER);
	
	
	@Override
	public void decorate(Object element, IDecoration decoration) {
		if (element instanceof IFolder){
			if (hasABSFiles((IFolder)element)){
				decoration.addOverlay(MARKER_DESCRIPTOR);
			}
		} else if (element instanceof IFile){
		   if (UtilityFunctions.isABSPackage((IFile)element)) {
		      //if (checkFile((IResource)element)){
		      decoration.addOverlay(MARKER_DESCRIPTOR);				
		      //}
		   }
		}		
	}
	
	private boolean hasABSFiles(IFolder folder) {
		if (folder.isAccessible()) {
			try {
				IResource[] members = folder.members();
				for (IResource member : getNonFolders(members)) {
					if(checkFile(member)){
						return true;
					}
				}

				for (IResource member : getFolders(members)) {

					if (hasABSFiles((IFolder) member)) {
						return true;
					}
				}
			} catch (CoreException e) {
				Activator.logException(e);
				return false;
			}
		}
		return false;
	}

	private static boolean checkFile(IResource member) {
		return UtilityFunctions.isABSSourceFile(member);
	}

	private static ArrayList<IFolder> getFolders(IResource[] members) {
		ArrayList<IFolder> folders = new ArrayList<IFolder>();
		
		for (IResource member : members){
			if (member instanceof IFolder){
				folders.add((IFolder)member);
			}
		}
		
		return folders;
	}
	
	private static ArrayList<IResource> getNonFolders(IResource[] members) {
		ArrayList<IResource> res = new ArrayList<IResource>();
		
		for (IResource member : members){
			if (!(member instanceof IFolder)){
				res.add((IResource)member);
			}
		}
		
		return res;
	}	

	/**
	 * Fires a LabelProviderChangeEvent to force re-decoration.
	 * This is a workaround for circumventing decorator update problems.
	 */
	public void refresh(){
		boolean isEnabled = PlatformUI.getWorkbench().getDecoratorManager().getEnabled(MODULE_DECORATOR_ID);
		
		if (isEnabled){
			this.fireLabelProviderChanged(new LabelProviderChangedEvent(this));
		}
	}	
}
