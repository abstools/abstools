package apet.handlers;


import java.io.File;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.dialogs.ErrorDialog;

import org.eclipse.swt.widgets.Shell;

import org.eclipse.ui.handlers.HandlerUtil;

import apet.console.ConsoleHandler;
import apet.dialogs.OptionsDialog;
import apet.utils.SourceUtils;
import eu.hatsproject.absplugin.costabslink.CostabsLink;

import apet.console.ApetShellCommand;
import apet.structures.ResultTracker;

/**
 * Our sample handler extends AbstractHandler, an IHandler base class.
 * @see org.eclipse.core.commands.IHandler
 * @see org.eclipse.core.commands.AbstractHandler
 */
public class apetHandler extends AbstractHandler {

	public static ResultTracker STORAGE_COSTABS = new ResultTracker();
	
	/**
	 * The constructor.
	 */
	public apetHandler() {
	}

	/**
	 * the command has been executed, so extract the needed information
	 * from the application context.
	 */
	public Object execute(ExecutionEvent event) throws ExecutionException {
		final Shell shellEclipse= HandlerUtil.getActiveWorkbenchWindowChecked(event).getShell();
		
		ApetShellCommand shell = new ApetShellCommand();

		try {
			ConsoleHandler.defaultConsole = ConsoleHandler.findCostabsConsole();

			String absFile = SourceUtils.extractResource(SourceUtils.obtainActiveEditor()).getLocation().toString();

			// Do costabs directory
			File f = new File("//tmp//costabs//absPL");
			f.mkdirs();
			
			if (CostabsLink.SELECTED_ITEMS.size() <= 0) {
				Status status = new Status(IStatus.ERROR, "costabs", 0,
			            "No functions or methods selected in the outline view.", null);
				ErrorDialog.openError(shellEclipse, "Costabs Error", "Costabs can not analyze.", status);
				
			}
			else {
				OptionsDialog mDialog = new OptionsDialog (shellEclipse);
				mDialog.open();
				
				if (mDialog.getReturnCode() == OptionsDialog.CANCEL) {
					ConsoleHandler.write("Don't do anything, cancelled by the user");
				} 
				else {
					// If analyze, get preferences and run
					shell.generateProlog(absFile, true);
					shell.analyze(absFile, CostabsLink.SELECTED_ITEMS);
				}
				
				// Execute shell commands
				ConsoleHandler.write(shell.getResult());
			}
		}
		catch (Exception e) {
			ConsoleHandler.write(shell.getError());
		}
			
		return null;
	}
}
