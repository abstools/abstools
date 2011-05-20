package costabs.handlers;

import java.io.File;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.handlers.HandlerUtil;

import costabs.console.ConsoleHandler;
import costabs.console.CostabsShellCommand;
import costabs.dialogs.OptionsDialog;
import costabs.utils.SourceUtils;

/**
 * Our sample handler extends AbstractHandler, an IHandler base class.
 * @see org.eclipse.core.commands.IHandler
 * @see org.eclipse.core.commands.AbstractHandler
 */
public class costabsHandler extends AbstractHandler {

	/**
	 * The constructor.
	 */
	public costabsHandler() {
	}

	/**
	 * the command has been executed, so extract the needed information
	 * from the application context.
	 */
	public Object execute(ExecutionEvent event) throws ExecutionException {
		final Shell shellEclipse= HandlerUtil.getActiveWorkbenchWindowChecked(event).getShell();
		try {
			ConsoleHandler.defaultConsole = ConsoleHandler.findCostabsConsole();

			String absFile = SourceUtils.extractResource(SourceUtils.obtainActiveEditor()).getLocation().toString();

			// Do costabs directory
			File f = new File("//tmp//costabs//absPL");
			f.mkdirs();
			
			CostabsShellCommand shell = new CostabsShellCommand();
			
			OptionsDialog mDialog = new OptionsDialog (shellEclipse);

			mDialog.open();
			
			if (mDialog.getReturnCode() == OptionsDialog.CANCEL) {
				ConsoleHandler.write("Don't do anything, cancelled by the user");
			}else{
				// If analyze, get preferences and run
				shell.generateProlog(absFile, false);
				shell.analyze(absFile, CostabsContainer.SELECTED_ITEMS);
			}
			// Execute shell commands
			
			ConsoleHandler.write(shell.getResult());
		}
		catch (Exception e) {

		}

		return null;
	}


}
