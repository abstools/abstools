package eu.hatsproject.absplugin.actions;

import static eu.hatsproject.absplugin.actions.ActionUtils.showErrorMessage;
import static eu.hatsproject.absplugin.util.Constants.ABSUNIT_TEST_ERROR;
import static eu.hatsproject.absplugin.util.Constants.ABSUNIT_TEST_OK;
import static eu.hatsproject.absplugin.util.Constants.ABSUNIT_TEST_USER_ABORT;
import static eu.hatsproject.absplugin.util.UtilityFunctions.getAbsNature;
import static eu.hatsproject.absplugin.util.UtilityFunctions.showErrorMessage;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.jobs.IJobChangeEvent;
import org.eclipse.core.runtime.jobs.IJobChangeListener;
import org.eclipse.swt.widgets.Display;

import eu.hatsproject.absplugin.console.ConsoleManager.MessageType;
import eu.hatsproject.absplugin.console.MsgConsole;
import eu.hatsproject.absplugin.exceptions.ParseException;
import eu.hatsproject.absplugin.exceptions.TypeCheckerException;

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
