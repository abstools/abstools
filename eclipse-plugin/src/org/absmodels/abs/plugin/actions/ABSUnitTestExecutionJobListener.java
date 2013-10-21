package org.absmodels.abs.plugin.actions;

import static org.absmodels.abs.plugin.actions.ActionUtils.showErrorMessage;
import static org.absmodels.abs.plugin.util.Constants.ABSUNIT_TEST_ERROR;
import static org.absmodels.abs.plugin.util.Constants.ABSUNIT_TEST_OK;
import static org.absmodels.abs.plugin.util.Constants.ABSUNIT_TEST_USER_ABORT;
import static org.absmodels.abs.plugin.util.UtilityFunctions.getAbsNature;
import static org.absmodels.abs.plugin.util.UtilityFunctions.showErrorMessage;

import org.absmodels.abs.plugin.console.MsgConsole;
import org.absmodels.abs.plugin.console.ConsoleManager.MessageType;
import org.absmodels.abs.plugin.exceptions.ParseException;
import org.absmodels.abs.plugin.exceptions.TypeCheckerException;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.jobs.IJobChangeEvent;
import org.eclipse.core.runtime.jobs.IJobChangeListener;
import org.eclipse.swt.widgets.Display;


public class ABSUnitTestExecutionJobListener implements IJobChangeListener {

	private IProject project;
	private MsgConsole console;
	
	public ABSUnitTestExecutionJobListener(IProject project){
		this.project = project;
	}
	
	@Override
	public void sleeping(IJobChangeEvent event) {}
	@Override
	public void scheduled(IJobChangeEvent event) {}
	@Override
	public void running(IJobChangeEvent event) {}
	@Override
	public void done(IJobChangeEvent event) {
	//When the Job is done get the IStatus and print messages on console
		final IStatus status = event.getResult();
		switch(status.getCode()){
		case ABSUNIT_TEST_OK:
			initializeConsole();
			console.println(status.getMessage(), MessageType.MESSAGE_INFO); break;
		case ABSUNIT_TEST_ERROR: 
			//encountered an exception during execution
			Display.getDefault().asyncExec(new Runnable() {
				@Override
				public void run() {
					if(status.getException() instanceof ParseException || status.getException() instanceof TypeCheckerException){
						showErrorMessage(status.getMessage(), status);
					} else{
						showErrorMessage(status.getMessage());
					}
				}
			});	break;
		case ABSUNIT_TEST_USER_ABORT: 
			initializeConsole();
			console.println("Aborted Maude Job.", MessageType.MESSAGE_ERROR); break;
		default:
			//This should never happen... just in case...
			showErrorMessage("reached an unspecified status");
		}
	}
	private void initializeConsole() {
		console = getAbsNature(project).getABSUnitTestExecutionConsole();
		console.clearConsole();
	}
	
	@Override
	public void awake(IJobChangeEvent event) {}
	@Override
	public void aboutToRun(IJobChangeEvent event) {}

}
