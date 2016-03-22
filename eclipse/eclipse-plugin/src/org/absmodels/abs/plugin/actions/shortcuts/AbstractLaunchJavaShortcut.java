/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.absmodels.abs.plugin.actions.shortcuts;

import static org.absmodels.abs.plugin.actions.ActionUtils.getActiveFile;
import static org.absmodels.abs.plugin.actions.ActionUtils.getSelectedResource;

import org.absmodels.abs.plugin.actions.JavaJob;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.ui.IEditorPart;


public abstract class AbstractLaunchJavaShortcut {
	
	private void startJavaJob(IResource resource, String actionId, boolean withoutArgs) {
		IProject project = resource.getProject();
		IAction action = new Action() {}; 
		action.setId(actionId);
		IFile file = null;
		
		if (resource instanceof IFile){
		   file = (IFile) resource;
		}
		
      JavaJob job = new JavaJob(JavaJob.RUN_JOB, action, project, file);	

      if(withoutArgs){
			overwriteDefaultArgs(job);
		}
		job.schedule();
	}
	
	private void overwriteDefaultArgs(JavaJob job) {
		job.setDebuggerArgsOther("");
		job.setDebuggerArgsSystemObserver("");
		job.setDebuggerArgsTotalScheduler("");
	}
	
	/**
	 * Starts a JavaJob using the default arguments/settings with the given action id 
	 * and the selected resource from the given selection
	 * 
	 * @see #launchSelectedFile(ISelection, String, boolean)
	 * @param selection
	 * @param actionId
	 */
	protected void launchSelectedFile(ISelection selection, String actionId) {
		launchSelectedFile(selection, actionId, false);
	}

	/**
	 * Starts a JavaJob using the default arguments/settings with the given action id 
	 * and the active file in the given editor.
	 * 
	 * @see #launchActiveFile(IEditorPart, String, boolean)
	 * @param editor
	 * @param actionId
	 */
	protected void launchActiveFile(IEditorPart editor, String actionId) {
		launchActiveFile(editor, actionId, false);
	}
	
	/**
	 * Starts a JavaJob with the given action id and the selected resource from the given selection.
	 * Overwrites the default debugger arguments with an empty string, if withoutArgs is true.
	 * 
	 * @param selection
	 * @param actionId
	 * @param withoutArgs
	 */
	protected void launchSelectedFile(ISelection selection, String actionId, boolean withoutArgs) {
		IResource resource = getSelectedResource(selection); 
		if (resource != null){
			startJavaJob(resource, actionId, withoutArgs);
		}
	}

	/**
	 * Starts a JavaJob with the given action id and the active file in the given editor.
	 * Overwrites the default debugger arguments with an empty string, if withoutArgs is true.
	 * 
	 * @param editor
	 * @param actionId
	 * @param withoutArgs
	 */
	protected void launchActiveFile(IEditorPart editor, String actionId, boolean withoutArgs) {
		IFile file = getActiveFile(editor);
		if (file != null){
			startJavaJob(file, actionId, withoutArgs);
		}
	}
}
