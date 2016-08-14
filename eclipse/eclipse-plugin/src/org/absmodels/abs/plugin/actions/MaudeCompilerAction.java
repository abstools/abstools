/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.absmodels.abs.plugin.actions;

import static org.absmodels.abs.plugin.actions.ActionUtils.getCurrentProject;
import static org.absmodels.abs.plugin.util.Constants.ACTION_EXEC_ID;
import static org.absmodels.abs.plugin.util.UtilityFunctions.saveEditors;
import static org.absmodels.abs.plugin.util.UtilityFunctions.showErrorMessage;

import java.io.File;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.IWorkbenchWindowActionDelegate;
import org.eclipse.ui.PartInitException;

public class MaudeCompilerAction implements IWorkbenchWindowActionDelegate{

	IWorkbenchWindow window;
	IProject project = null;
	File destFolder;
	
	@Override
	public void run(IAction action){
		boolean realTime = false;
		
		IEditorPart editorPart = window.getActivePage().getActiveEditor();
		try {
			project = getCurrentProject(window, editorPart);
		} catch (PartInitException e) {
		} 
		
		//If no files or projects have been selected, show an error and abort
		if(project == null || !project.exists()){
			showErrorMessage("No project has been selected. Please open a file or select one in the project explorer.");
			return;
		}

		//Create a new MaudeJob which will generate Maude code and (if run button was pressed) execute it
		saveEditors(project, true);
		final Job maudeJob = new MaudeJob(project, realTime, action.getId().equals(ACTION_EXEC_ID));
		maudeJob.addJobChangeListener(new MaudeJobChangeListener(project));
		maudeJob.schedule();
	}

	@Override
	public void selectionChanged(IAction action, ISelection selection) {}

	@Override
	public void dispose() {}

	@Override
	public void init(IWorkbenchWindow window) {
		this.window = window;
	}
}
