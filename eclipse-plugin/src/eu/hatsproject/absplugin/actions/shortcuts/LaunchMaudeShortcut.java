/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package eu.hatsproject.absplugin.actions.shortcuts;

import static eu.hatsproject.absplugin.actions.ActionUtils.getActiveFile;
import static eu.hatsproject.absplugin.actions.ActionUtils.getSelectedResource;
import static eu.hatsproject.absplugin.util.UtilityFunctions.saveEditors;

import org.eclipse.core.resources.IResource;
import org.eclipse.debug.ui.ILaunchShortcut;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.ui.IEditorPart;

import eu.hatsproject.absplugin.actions.MaudeJob;
import eu.hatsproject.absplugin.actions.MaudeJobChangeListener;

public class LaunchMaudeShortcut implements ILaunchShortcut {

	@Override
	public void launch(ISelection selection, String mode) {
		launch(getSelectedResource(selection));
	}

	@Override
	public void launch(IEditorPart editor, String mode) {
		launch(getActiveFile(editor));	
	}
	
	private void launch(IResource resource){
		if(resource != null){
			saveEditors(resource.getProject(), true);
			MaudeJob maudeJob = new MaudeJob(resource.getProject(), false, true);
			maudeJob.addJobChangeListener(new MaudeJobChangeListener(resource.getProject()));
			maudeJob.schedule();
		}
	}
}
