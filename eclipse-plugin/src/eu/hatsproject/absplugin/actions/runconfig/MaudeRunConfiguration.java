/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package eu.hatsproject.absplugin.actions.runconfig;

import static eu.hatsproject.absplugin.actions.ActionUtils.getProject;
import static eu.hatsproject.absplugin.actions.ActionUtils.saveDirtyEditors;
import static eu.hatsproject.absplugin.util.Constants.RUNCONFIG_MAUDE_EXECUTE;
import static eu.hatsproject.absplugin.util.Constants.RUNCONFIG_MAUDE_PARTIAL_EXEC;
import static eu.hatsproject.absplugin.util.Constants.RUNCONFIG_MAUDE_STEPS;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.debug.core.ILaunch;
import org.eclipse.debug.core.ILaunchConfiguration;
import org.eclipse.debug.core.model.ILaunchConfigurationDelegate;

import eu.hatsproject.absplugin.actions.MaudeJob;
import eu.hatsproject.absplugin.actions.MaudeJobChangeListener;
public class MaudeRunConfiguration implements ILaunchConfigurationDelegate {

	private IProject project;
	
	@Override
	public void launch(ILaunchConfiguration configuration, String mode,	ILaunch launch, IProgressMonitor monitor) throws CoreException {
		project = getProject(configuration);
		saveDirtyEditors(project);	
		boolean realTime = false;
		MaudeJob maudeJob;
		
		if(configuration.getAttribute(RUNCONFIG_MAUDE_PARTIAL_EXEC, false)){
			maudeJob = new MaudeJob(project, realTime, true, configuration.getAttribute(RUNCONFIG_MAUDE_STEPS, 0));
		} else{
			maudeJob = new MaudeJob(project, realTime, configuration.getAttribute(RUNCONFIG_MAUDE_EXECUTE, false));
		}
		maudeJob.addJobChangeListener(new MaudeJobChangeListener(project));
		maudeJob.schedule();
	}

	
}
