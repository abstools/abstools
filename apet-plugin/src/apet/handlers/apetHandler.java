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
import abs.backend.prolog.PrologBackend;
import abs.frontend.ast.Model;

import apet.console.ApetShellCommand;

/**
 * Our sample handler extends AbstractHandler, an IHandler base class.
 * @see org.eclipse.core.commands.IHandler
 * @see org.eclipse.core.commands.AbstractHandler
 */
public class apetHandler extends AbstractHandler {

	
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

			// Creating the costabs tmp directory
			File f = new File("//tmp//costabs//absPL");
			f.mkdirs();
			
			if (CostabsLink.ENTRIES_STRINGS.size() <= 0) {
				Status status = new Status(IStatus.ERROR, "costabs", 0,
			            "At least one function or method must be selected in the outline view.", null);
				ErrorDialog.openError(shellEclipse, "Costa Error", "Costa cannot analyze.", status);		
			} else {
				OptionsDialog mDialog = new OptionsDialog (shellEclipse);
				mDialog.open();
				
				if (mDialog.getReturnCode() == OptionsDialog.CANCEL) {
					ConsoleHandler.write("Don't do anything, cancelled by the user");
				} else {
					// If analyze, get preferences and run
					callPrologBackend(absFile);
					shell.callAPet(CostabsLink.ENTRIES_STRINGS);
				}	
				// Execute shell commands
				ConsoleHandler.write(shell.getResult());
			}
		} catch (Exception e) {
			ConsoleHandler.write(shell.getError());
		}

		return null;
	}
	
	private void callPrologBackend(String filename) throws Exception {
    	/*
		int numArgs = 3;
		String[] args = new String[numArgs];
		int i = 0;
		args[i++] = "-d";
		args[i++] = "/tmp/costabs/absPL";
		args[i++] = filename;
		PrologBackend.runFromShell(args);
		*/
		
		Model model = CostabsLink.ABS_NATURE.getCompleteModel(); //getCurrentABSModel();
		PrologBackend.runFromPlugin(model,"/tmp/costabs/absPL","abs.pl",CostabsLink.ENTRIES_NODES);
	}
}
