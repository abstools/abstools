/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.absmodels.abs.plugin.actions;

import static org.absmodels.abs.plugin.util.Constants.EDITOR_ID;
import static org.absmodels.abs.plugin.util.Constants.RUNCONFIG_PROJECT_NAME_ATTRIBUTE;
import static org.absmodels.abs.plugin.util.UtilityFunctions.saveEditors;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.debug.core.ILaunchConfiguration;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.swt.widgets.Display;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IPageLayout;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.navigator.CommonNavigator;

public class ActionUtils {
	
	public static void saveDirtyEditors(final IProject project) {
		Display.getDefault().syncExec(new Runnable() { 
			@Override
			public void run() { 
				saveEditors(project, true); 
			}
		});
	}
	
	/**
	 * Return the project of the currently opened file. If no file is opened, then returns the project of the resource selected
	 * in the project explorer. Returns null if active page is null, no editor available or no resource selected.
	 * @param window
	 * @param editorPart
	 * @return current project
	 * @throws PartInitException
	 */
	public static IProject getCurrentProject(IWorkbenchWindow window, IEditorPart editorPart) throws PartInitException{
		
		String activePage;

		try{
			activePage = window.getActivePage().getActivePart().getSite().getId();
		} catch (NullPointerException e) {
			return null;
		}
		
		if(activePage.equals(EDITOR_ID)){
			IFile f = getActiveFile(editorPart);
			if (f != null)
			   return f.getProject();
		} else if(activePage.equals(IPageLayout.ID_PROJECT_EXPLORER)){
			IResource r = getSelectedResourceOrNull(window);
			if(r != null){
				return r.getProject();
			}
		} else{
			if(editorPart != null) {
				return getActiveFile(editorPart).getProject();
			} else {
				IResource r = getSelectedResourceOrNull(window);
				return r.getProject();
			}
		}
		
      IFile f = getOpenFileOrNull(window);
      if (f != null)
         return f.getProject();
		
		//all other cases (e.g. no open editor and nothing is selected in projectExplorer)
		return null;
	}

   private static IFile getOpenFileOrNull(IWorkbenchWindow window) {
      for (IWorkbenchPage p : window.getPages()) {
         IEditorPart editor = p.getActiveEditor();
         if (editor != null && editor.getSite().getId().equals(EDITOR_ID)) {
            return getActiveFile(editor);
         }
      }
      return null;
   }	
	
	/**
	 * Return the currently opened file. If no file is opened return the file selected
	 * in the project explorer. If no file is selected in the project explorer or active page is null, return null.
	 * @param window
	 * @param editorPart
	 * @return current file
	 * @throws PartInitException
	 */
	public static IFile getCurrentFile(IWorkbenchWindow window, IEditorPart editorPart) throws PartInitException{

		String activePage;
		try{
			activePage = window.getActivePage().getActivePart().getSite().getId();
		} catch (NullPointerException e) {
			return null;
		}
		
		if(activePage.equals(EDITOR_ID)){
			return getActiveFile(editorPart);
		} else if(activePage.equals(IPageLayout.ID_PROJECT_EXPLORER)){
			IResource r = getSelectedResourceOrNull(window);
			try {
				//if a file was selected
				return (IFile) r;
			} catch (ClassCastException e){
				return null;
			}
		} else{
			if(editorPart != null) {
				//get file and project from editor
				return getActiveFile(editorPart);
			} else {
				//get file and project from project explorer
				IResource r = getSelectedResourceOrNull(window);
				try {
					//if a file was selected
					return (IFile) r;
				} catch (ClassCastException e){
					return null;
				}
			}
		}
	}
	
	/**
	 * returns the active file (IFile) in the given editor
	 * @see org.eclipse.core.runtime.IAdaptable#getAdapter(Class)
	 * @param editorPart
	 * @return file active in the editor
	 */
	public static IFile getActiveFile(IEditorPart editorPart){
		return ((IFile) editorPart.getEditorInput().getAdapter(IFile.class));
	}
	
	/**
	 * returns the first element in the given selection 
	 * or null if this element is not an instance of IFile
	 * 
	 * @param selection
	 * @return selected file or <b>null</b> if no file selected
	 */
	public static IFile getSelectedFile(ISelection selection) {
		if (selection instanceof IStructuredSelection) {
        	Object firstElement = ((IStructuredSelection)selection).getFirstElement();
            if (firstElement instanceof IFile) {
            	return (IFile) firstElement;
            } 
        }
		return null;
	}
	
	/**
	 * returns the first element in the given selection 
	 * or null if this element is not an instance of IResource
	 * 
	 * @param selection
	 * @return selected resource or <b>null</b> if no resource selected
	 */
	public static IResource getSelectedResource(ISelection selection) {
		if (selection instanceof IStructuredSelection) {
        	Object firstElement = ((IStructuredSelection)selection).getFirstElement();
            if (firstElement instanceof IResource) {
            	return (IResource) firstElement;
            } 
        }
		return null;
	}
	
	private static IResource getSelectedResourceOrNull(IWorkbenchWindow window) throws PartInitException{
		CommonNavigator explorer = (CommonNavigator)window.getActivePage().showView(IPageLayout.ID_PROJECT_EXPLORER);
		Object e = ((StructuredSelection)explorer.getCommonViewer().getSelection()).getFirstElement();
		if (e instanceof IResource)
		   return (IResource) e;
		return null;
	}
	
	/**
	 * Convenience method showing an error dialog with the given error message.
	 */
	public static void showErrorMessage(final String errorMessage, IStatus status){
		ErrorDialog.openError(Display.getCurrent().getActiveShell(), "Error", errorMessage, status);
	}

	/**
	 * Get project from launch configuration.
	 * The stored project can be null or not an ABS project.
	 * 
	 * @param configuration
	 * @return the project from the launch configuration
	 * @throws CoreException
	 */
	public static IProject getProject(ILaunchConfiguration configuration) throws CoreException {
		IProject project;
		String projectName = configuration.getAttribute(RUNCONFIG_PROJECT_NAME_ATTRIBUTE, "undefinded");
		project = ResourcesPlugin.getWorkspace().getRoot().getProject(projectName);
		
		return project;
	}
	
	
}