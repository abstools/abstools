/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package eu.hatsproject.absplugin.actions;

import static eu.hatsproject.absplugin.util.UtilityFunctions.showErrorMessage;
import static eu.hatsproject.absplugin.util.UtilityFunctions.saveEditors;
import static eu.hatsproject.absplugin.util.UtilityFunctions.standardExceptionHandling;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.IWorkbenchWindowActionDelegate;
import org.eclipse.ui.PartInitException;



public class JavaAction implements IWorkbenchWindowActionDelegate {

	private IWorkbenchWindow window;

	@Override
	public void run(IAction action) {
		try{
			// get project and file
			IEditorPart editorPart = window.getActivePage().getActiveEditor();
			IProject project = null;
			IFile file = null;
			try {
				project = ActionUtils.getCurrentProject(window, editorPart);
				file = ActionUtils.getCurrentFile(window, editorPart);
			} catch (PartInitException e) {
				standardExceptionHandling(e);
				//just to be sure:
			} catch (NullPointerException e) {
				standardExceptionHandling(e);
			} catch (ClassCastException e) {
				standardExceptionHandling(e);
			}
			
			if (project == null) {
				showErrorMessage("No ABS project found.");
				return;
			}
			
			saveEditors(project, true);
			// starts a new job
			JavaJob job = new JavaJob("ABS Java Code Generation", action, project, file);
			job.schedule();
		} catch (Exception e) {
			//do not kill the plug-in, if something goes wrong
			standardExceptionHandling(e);
			showErrorMessage("Fatal Error!");
		}

	}

	@Override
	public void selectionChanged(IAction action, ISelection selection) {
		// nothing
	}

	@Override
	public void dispose() {
		// nothing
	}

	@Override
	public void init(IWorkbenchWindow window) {
		this.window = window;
	}

}
