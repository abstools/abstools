/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.absmodels.abs.plugin.actions.runconfig;

import static org.absmodels.abs.plugin.actions.ActionUtils.getProject;
import static org.absmodels.abs.plugin.actions.ActionUtils.saveDirtyEditors;
import static org.absmodels.abs.plugin.util.Constants.RUNCONFIG_MAUDE_EXECUTE;
import static org.absmodels.abs.plugin.util.Constants.RUNCONFIG_MAUDE_MAINBLOCK;
import static org.absmodels.abs.plugin.util.Constants.RUNCONFIG_MAUDE_PARTIAL_EXEC;
import static org.absmodels.abs.plugin.util.Constants.RUNCONFIG_MAUDE_REALTIME;
import static org.absmodels.abs.plugin.util.Constants.RUNCONFIG_MAUDE_STEPS;
import static org.absmodels.abs.plugin.util.Constants.RUNCONFIG_PRODUCT_NAME_ATTRIBUTE;
import static org.absmodels.abs.plugin.util.Constants.RUNCONFIG_TEST_EXECUTION;

import org.absmodels.abs.plugin.actions.ABSUnitTestExecutionJob;
import org.absmodels.abs.plugin.actions.ABSUnitTestExecutionJobListener;
import org.absmodels.abs.plugin.actions.ABSUnitTestMaudeExecutionJob;
import org.absmodels.abs.plugin.actions.MaudeJob;
import org.absmodels.abs.plugin.actions.MaudeJobChangeListener;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.debug.core.ILaunch;
import org.eclipse.debug.core.ILaunchConfiguration;
import org.eclipse.debug.core.model.ILaunchConfigurationDelegate;


public class MaudeRunConfiguration implements ILaunchConfigurationDelegate {

	@Override
	public void launch(ILaunchConfiguration configuration, String mode,	ILaunch launch, IProgressMonitor monitor) throws CoreException {
		IProject project = getProject(configuration);
		saveDirtyEditors(project);	
		boolean realTime = configuration.getAttribute(RUNCONFIG_MAUDE_REALTIME, false);
		String mainBlock = configuration.getAttribute(RUNCONFIG_MAUDE_MAINBLOCK, (String)null);
		boolean partialExec = configuration.getAttribute(RUNCONFIG_MAUDE_PARTIAL_EXEC, false);
		int execStep = configuration.getAttribute(RUNCONFIG_MAUDE_STEPS, 0);
		boolean execute = configuration.getAttribute(RUNCONFIG_MAUDE_EXECUTE, false);
		String productName = configuration.getAttribute(RUNCONFIG_PRODUCT_NAME_ATTRIBUTE, (String)null);
		
		if (configuration.getAttribute(RUNCONFIG_TEST_EXECUTION, false)) {
			ABSUnitTestExecutionJob testJob = new ABSUnitTestMaudeExecutionJob(
					project,realTime,mainBlock,partialExec,execute,execStep,productName);
			testJob.addJobChangeListener(new ABSUnitTestExecutionJobListener(project));
			testJob.schedule();
		} else {
			MaudeJob maudeJob;
			
			if(partialExec){
				maudeJob = new MaudeJob(project, realTime, true, execStep);
			} else{
				maudeJob = new MaudeJob(project, realTime, execute);
			}
			
			if (mainBlock != null)
				maudeJob.setMainBlock(mainBlock);
			
			if (productName != null)
				maudeJob.setProduct(productName);
			
			maudeJob.addJobChangeListener(new MaudeJobChangeListener(project));
			maudeJob.schedule();
		}
	}

	
}
