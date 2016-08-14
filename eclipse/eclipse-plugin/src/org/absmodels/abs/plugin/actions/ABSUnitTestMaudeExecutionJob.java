package org.absmodels.abs.plugin.actions;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;


public class ABSUnitTestMaudeExecutionJob extends ABSUnitTestExecutionJob {

	private final boolean realTime;
	private final String mainBlock;
	private final boolean partialExec;
	private final boolean execute;
	private final int executeStep;
	
	public ABSUnitTestMaudeExecutionJob(IProject project,
			boolean realTime, String mainBlock, boolean partialExec,
			boolean execute, int executeStep, String productName) {
		super(project, productName);
		this.realTime = realTime;
		this.mainBlock = mainBlock;
		this.partialExec = partialExec;
		this.execute = execute;
		this.executeStep = executeStep;
	}
	
	protected IStatus executeTest(IProgressMonitor monitor) {
		final MaudeJob maudeJob;
		if(partialExec){
			maudeJob = new MaudeJob(project, realTime, true, executeStep);
		} else{
			maudeJob = new MaudeJob(project, realTime, execute);
		}
		
		if (mainBlock != null)
			maudeJob.setMainBlock(mainBlock);

		if (productName != null)
			maudeJob.setProduct(productName);
		
		IStatus status = maudeJob.runJob(monitor);
		monitor.worked(100);
		return status;
	}

}
