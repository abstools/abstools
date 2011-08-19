package eu.hatsproject.absplugin.actions;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;


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
	
	protected void executeTest(IProgressMonitor monitor) throws CoreException {
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
		
		maudeJob.runJob(monitor);
		monitor.worked(100);
	}

}
