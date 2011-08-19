/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package eu.hatsproject.absplugin.actions.runconfig;

import static eu.hatsproject.absplugin.util.Constants.*;
import static eu.hatsproject.absplugin.actions.ActionUtils.getProject;
import static eu.hatsproject.absplugin.actions.runconfig.RunConfigEnums.*;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.debug.core.ILaunch;
import org.eclipse.debug.core.ILaunchConfiguration;
import org.eclipse.debug.core.model.ILaunchConfigurationDelegate;
import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.IAction;

import abs.common.WrongProgramArgumentException;
import abs.frontend.ast.Model;

import eu.hatsproject.absplugin.actions.ABSUnitTestExecutionJob;
import eu.hatsproject.absplugin.actions.ABSUnitTestJavaExecutionJob;
import eu.hatsproject.absplugin.actions.ActionUtils;
import eu.hatsproject.absplugin.actions.JavaJob;
import eu.hatsproject.absplugin.exceptions.AbsJobException;
import eu.hatsproject.absplugin.util.Constants;


public class JavaRunConfiguration implements ILaunchConfigurationDelegate {

	@Override
	public void launch(ILaunchConfiguration configuration, String mode,
			ILaunch launch, IProgressMonitor monitor) throws CoreException {
		
		IAction action = createNewAction(configuration);
		IProject project = getProject(configuration);
		IFile file = null;
	
		ActionUtils.saveDirtyEditors(project);
		
		if (configuration.getAttribute(RUNCONFIG_TEST_EXECUTION, false)) {
			Job testJob = getABSUnitTestExecutionJob(project,action,file,configuration);
			testJob.schedule();
		} else {
			JavaJob job = new JavaJob(JavaJob.RUN_JOB,action, project, file);
			String product = configuration.getAttribute(RUNCONFIG_PRODUCT_NAME_ATTRIBUTE, (String)null);
			if (product != null) {
				try {
					Model model = JavaJob.getModelFromProject(project);
					job.setProduct(model.findProduct(product));
				} catch (WrongProgramArgumentException e) {
					throw new CoreException(new Status(IStatus.ERROR, Constants.PLUGIN_ID, "Launch failed", e));
				} catch (AbsJobException e) {
					throw new CoreException(new Status(IStatus.ERROR, Constants.PLUGIN_ID, "Launch failed", e));
				}
			}
			modifyDebuggerArguments(configuration, job);
			job.schedule();
		}
	}
	
	private Job getABSUnitTestExecutionJob(IProject project, 
			IAction action, IFile file, ILaunchConfiguration configuration) throws CoreException {
		return new ABSUnitTestJavaExecutionJob(
			project,
			configuration.getAttribute(RUNCONFIG_PRODUCT_NAME_ATTRIBUTE, (String)null),
			action,
			file,
			configuration.getAttribute(RUNCONFIG_DEBUGGER_OTHER_ARGS_ATTRIBUTE, DEBUGGER_ARGS_OTHER_DEFAULT),
			configuration.getAttribute(RUNCONFIG_DEBUGGER_RANDOMSEED, ""),
			configuration.getAttribute(RUNCONFIG_DEBUGGER_COMPILE_BEFORE, DEBUGGER_COMPILE_BEFORE_DEFAULT),
			configuration.getAttribute(RUNCONFIG_DEBUGGER_OBSERVER_LIST, new ArrayList<String>()),
			configuration.getAttribute(RUNCONFIG_DEBUGGER_CLASSPATH_LIST, new ArrayList<String>()),
			configuration.getAttribute(RUNCONFIG_DEBUGGER_SCHEDULER_ATTRIBUTE, DebuggerScheduler.getDefaultScheduler().toString()),
			configuration.getAttribute(RUNCONFIG_DEBUGGER_USE_EXTERNAL, RUNCONFIG_DEBUGGER_USE_EXTERNAL_DEFAULT),
			configuration.getAttribute(RUNCONFIG_DEBUGGER_DEBUG_MODE, RUNCONFIG_DEBUGGER_DEBUG_MODE_DEFAULT)
		);
	}

	private void modifyDebuggerArguments(ILaunchConfiguration configuration,
			JavaJob job) throws CoreException {
		
		job.setDebuggerArgsOther(configuration.getAttribute(RUNCONFIG_DEBUGGER_OTHER_ARGS_ATTRIBUTE, DEBUGGER_ARGS_OTHER_DEFAULT));
		job.setArgsDebuggerRandomSeed(configuration.getAttribute(RUNCONFIG_DEBUGGER_RANDOMSEED, ""));
		
		boolean compile = configuration.getAttribute(RUNCONFIG_DEBUGGER_COMPILE_BEFORE, DEBUGGER_COMPILE_BEFORE_DEFAULT);
		job.setDebuggerCompileFirst(compile);
		
		String observerArgs = "";
		@SuppressWarnings("unchecked") //setAttribute(Attribute, List) only accepts List<String>
		List<String> observerStrings = configuration.getAttribute(RUNCONFIG_DEBUGGER_OBSERVER_LIST, new ArrayList<String>());
			for (String observerClassName : observerStrings) {
				if(!observerClassName.isEmpty()){
					if (observerArgs.equals("")){
						observerArgs = DebuggerObserver.getEmptyCommand()+observerClassName;
					} else {
						observerArgs += ","+observerClassName;
					}
				}
			}
		job.setDebuggerArgsSystemObserver(observerArgs);
		
		job.setExtraClassPaths(configuration.getAttribute(RUNCONFIG_DEBUGGER_CLASSPATH_LIST, new ArrayList<String>()));
		
		String schedulerString = configuration.getAttribute(RUNCONFIG_DEBUGGER_SCHEDULER_ATTRIBUTE, DebuggerScheduler.getDefaultScheduler().toString());
		job.setDebuggerArgsTotalScheduler(DebuggerScheduler.valueOf(schedulerString).getCommand());
		
		job.setInternalDebugger(!configuration.getAttribute(RUNCONFIG_DEBUGGER_USE_EXTERNAL, RUNCONFIG_DEBUGGER_USE_EXTERNAL_DEFAULT));
		
		job.setDebuggerDebugMode(configuration.getAttribute(RUNCONFIG_DEBUGGER_DEBUG_MODE, RUNCONFIG_DEBUGGER_DEBUG_MODE_DEFAULT));
	}

	private IAction createNewAction(ILaunchConfiguration configuration)
			throws CoreException {
		IAction action = new Action() {
			//nothing
		}; 
		
		boolean startSDE = false;
		@SuppressWarnings("unchecked")
		List<String> observerStrings = configuration.getAttribute(RUNCONFIG_DEBUGGER_OBSERVER_LIST, new ArrayList<String>());
		for (String observerClassName : observerStrings) {
			DebuggerObserver observer = DebuggerObserver.valueOfClassName(observerClassName);
			if(observer!=null){
				startSDE = startSDE || observer.needsSDEdit();
			}
		}
		if(startSDE){
			action.setId(ACTION_START_SDE);
		} else {
			action.setId(ACTION_DEBUG_ID);
		}
		
		return action;
	}

}
