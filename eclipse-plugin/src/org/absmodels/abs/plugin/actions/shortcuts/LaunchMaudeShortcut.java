/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.absmodels.abs.plugin.actions.shortcuts;

import static org.absmodels.abs.plugin.actions.ActionUtils.getActiveFile;
import static org.absmodels.abs.plugin.actions.ActionUtils.getSelectedResource;
import static org.absmodels.abs.plugin.util.UtilityFunctions.saveEditors;

import org.absmodels.abs.plugin.actions.MaudeJob;
import org.absmodels.abs.plugin.actions.MaudeJobChangeListener;
import org.eclipse.core.resources.IResource;
import org.eclipse.debug.ui.ILaunchShortcut;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.ui.IEditorPart;


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
