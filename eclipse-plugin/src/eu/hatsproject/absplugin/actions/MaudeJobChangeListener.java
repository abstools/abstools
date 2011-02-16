package eu.hatsproject.absplugin.actions;

import static eu.hatsproject.absplugin.util.Constants.MAUDE_ERROR;
import static eu.hatsproject.absplugin.util.Constants.MAUDE_ERROR_MAUDE;
import static eu.hatsproject.absplugin.util.Constants.MAUDE_ERROR_MAUDE_PATH;
import static eu.hatsproject.absplugin.util.Constants.MAUDE_INFO;
import static eu.hatsproject.absplugin.util.Constants.MAUDE_OK;
import static eu.hatsproject.absplugin.util.Constants.MAUDE_USER_ABORT;
import static eu.hatsproject.absplugin.util.Constants.MAUDE_WARNING;
import static eu.hatsproject.absplugin.util.UtilityFunctions.getAbsNature;
import static eu.hatsproject.absplugin.util.UtilityFunctions.showErrorMessage;
import static eu.hatsproject.absplugin.actions.ActionUtils.showErrorMessage;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.jobs.IJobChangeEvent;
import org.eclipse.core.runtime.jobs.IJobChangeListener;
import org.eclipse.swt.widgets.Display;
import org.eclipse.ui.dialogs.PreferencesUtil;

import eu.hatsproject.absplugin.console.ConsoleManager.MessageType;
import eu.hatsproject.absplugin.console.MsgConsole;
import eu.hatsproject.absplugin.exceptions.ParseException;
import eu.hatsproject.absplugin.exceptions.TypeCheckerException;

public class MaudeJobChangeListener implements IJobChangeListener{
	
	IProject project;
	MsgConsole console;
	
	public MaudeJobChangeListener(IProject project){
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
		case MAUDE_OK: break; //Indicates a successful Job without Maude execution
		case MAUDE_INFO:    
			//Execution of Maude was successful
			initializeConsole();
			console.println(status.getMessage(), MessageType.MESSAGE_INFO); break;
		case MAUDE_WARNING: //This message type is not used
			initializeConsole();
			console.println(status.getMessage(), MessageType.MESSAGE_WARNING); break;
		case MAUDE_ERROR_MAUDE:
			//Maude encountered an error during execution
			initializeConsole();
			console.println(status.getMessage(), MessageType.MESSAGE_ERROR); break;
		case MAUDE_ERROR_MAUDE_PATH:
			//missconfigured Maude path
			Display.getDefault().asyncExec(new Runnable() {
				@Override
				public void run() {
					showErrorMessage("Encountered IOException while executing Maude (probably misconfigured location of maude executable): " + status.getMessage());
					PreferencesUtil.createPreferenceDialogOn(Display.getDefault().getActiveShell(), "eu.hatsproject.absplugin.preferences.ABSMaudePreferences", null, null).open();
				}
			});	break;
		case MAUDE_ERROR: 
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
		case MAUDE_USER_ABORT: 
			initializeConsole();
			console.println("Aborted Maude Job.", MessageType.MESSAGE_ERROR); break;
		default:
			//This should never happen... just in case...
			showErrorMessage("reached an unspecified status");
		}
	}
	private void initializeConsole() {
		console = getAbsNature(project).getMaudeConsole();
		console.clearConsole();
	}
	
	@Override
	public void awake(IJobChangeEvent event) {}
	@Override
	public void aboutToRun(IJobChangeEvent event) {}
}