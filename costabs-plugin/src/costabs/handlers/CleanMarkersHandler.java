package costabs.handlers;


import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.handlers.HandlerUtil;

import costabs.dialogs.DialogPrinter;
import costabs.exceptions.CostabsException;
import costabs.trackers.OutputManager;
import costabs.utils.SourceUtils;


/**
 * Our sample handler extends AbstractHandler, an IHandler base class.
 * @see org.eclipse.core.commands.IHandler
 * @see org.eclipse.core.commands.AbstractHandler
 */
public class CleanMarkersHandler extends AbstractHandler {

	/**
	 * the command has been executed, so extract the needed information
	 * from the application context.
	 */
	public Object execute(ExecutionEvent event) throws ExecutionException {

		final Shell shellEclipse= HandlerUtil.getActiveWorkbenchWindowChecked(event).getShell();
		try {
			
			OutputManager.getInstance().cleanAll(SourceUtils.getActiveFile());
		} catch (CostabsException e) {
			DialogPrinter.printError(shellEclipse, e, e.getMessage());
		}
		return null;
	}

}
