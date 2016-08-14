/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved.
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.absmodels.abs.plugin.actions.runconfig;

import static org.absmodels.abs.plugin.actions.runconfig.RunConfigEnums.*;
import static org.absmodels.abs.plugin.util.Constants.*;

import java.util.List;

import org.absmodels.abs.plugin.actions.ABSUnitTestJavaExecutionJob;
import org.absmodels.abs.plugin.actions.ActionUtils;
import org.absmodels.abs.plugin.actions.JavaJob;
import org.absmodels.abs.plugin.actions.runconfig.java.JavaLaunchConfig;
import org.absmodels.abs.plugin.exceptions.AbsJobException;
import org.absmodels.abs.plugin.util.Constants;
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

import abs.backend.java.absunit.ABSTestObserver;
import abs.common.WrongProgramArgumentException;
import abs.frontend.ast.Model;


public class JavaRunConfiguration implements ILaunchConfigurationDelegate {

	@Override
	public void launch(ILaunchConfiguration config, String mode,
			ILaunch launch, IProgressMonitor monitor) throws CoreException {

	    JavaLaunchConfig launchConfig = new JavaLaunchConfig(config);

		IAction action = createNewAction(launchConfig);
		IProject project = ActionUtils.getProject(config);
		IFile file = null;

		ActionUtils.saveDirtyEditors(project);

		JavaJob job = new JavaJob(JavaJob.RUN_JOB,action, project, file);
		String product = launchConfig.getProductName();
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

		if (launchConfig.getTestExecution()) {
			List<String> obs = launchConfig.getDebuggerObserverList();
			obs.add(ABSTestObserver.class.getName());
		}

		modifyDebuggerArguments(launchConfig, job);

		if (launchConfig.getTestExecution()) {
		    // execution of unit tests
			Job testJob = new ABSUnitTestJavaExecutionJob(project, product, job);
			testJob.schedule();
		} else {
		    // normal execution
		    job.schedule();
		}
	}


	private void modifyDebuggerArguments(JavaLaunchConfig launchConfig,
			JavaJob job) throws CoreException {

		job.setDebuggerArgsOther(launchConfig.getOtherArgs());
		job.setArgsDebuggerRandomSeed(launchConfig.getRandomSeedString());

		boolean compile = launchConfig.getCompileBefore();
		job.setDebuggerCompileFirst(compile);

		String observerArgs = "";
		List<String> observerStrings = launchConfig.getDebuggerObserverList();
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

		job.setExtraClassPaths(launchConfig.getDebuggerClassPathList());

		String schedulerString = launchConfig.getDebuggerScheduler();
		job.setDebuggerArgsTotalScheduler(DebuggerScheduler.valueOf(schedulerString).getCommand());

		job.setInternalDebugger(!launchConfig.getUseExternal());

		job.setDebuggerDebugMode(launchConfig.getDebugMode());

		job.setRunTarget(launchConfig.getRunTarget());
        job.setScheduler(launchConfig.getScheduler());
        job.setRunAutomatically(launchConfig.getRunAutomatically());
        job.setHistoryFile(launchConfig.getHistoryFile());
        job.setFLIClassPath(launchConfig.getDebuggerClassPathList());
        job.setIgnoreMissingFLIClasses(launchConfig.getIgnoreMissingFLIClasses());
        job.setUseFifoSemantics(launchConfig.getUseFifoSemantics());
	}

	private IAction createNewAction(JavaLaunchConfig launchConfig)
			throws CoreException {
		IAction action = new Action() {
			//nothing
		};

		boolean startSDE = false;
		List<String> observerStrings = launchConfig .getDebuggerObserverList();
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
