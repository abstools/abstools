package org.absmodels.abs.plugin.actions;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IncrementalProjectBuilder;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;

import abs.backend.tests.ABSTestRunnerGenerator;

public class ABSUnitTestJavaExecutionJob extends ABSUnitTestExecutionJob {

    private final JavaJob runJob;

	/**
	 * will generate an ABS module to execute all unit tests in the given project+prduct
	 * after this module is generated, the JavaJob 'runJob' will execute the tests
	 */
    public ABSUnitTestJavaExecutionJob(IProject project,
			String productName,
			JavaJob runJob) {
		super(project, productName);
		this.runJob = runJob;
	}

	protected IStatus executeTest(IProgressMonitor monitor) throws CoreException {
	    runJob.setRunTarget(ABSTestRunnerGenerator.RUNNER_MAIN);
	    runJob.setTerminateOnException(false);
		return runJob.runJob(monitor);
	}
	

}
