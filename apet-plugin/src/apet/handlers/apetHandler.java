package apet.handlers;


import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;

import org.eclipse.swt.widgets.Shell;

import org.eclipse.ui.handlers.HandlerUtil;


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

			
		return null;
	}
	
	


}
