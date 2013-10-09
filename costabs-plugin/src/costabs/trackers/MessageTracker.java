package costabs.trackers;

import java.util.ArrayList;
import java.util.Collection;

import org.eclipse.core.commands.ExecutionException;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.handlers.HandlerUtil;

import costabs.beans.Command;
import costabs.console.ConsoleHandler;
import costabs.dialogs.DialogPrinter;
import costabs.structures.CostabsConstants;

public class MessageTracker extends CommandTracker{

	public MessageTracker(Command command) {
		super(command);
	}

	@Override
	public void track() {
		
		if (CostabsConstants.LEVEL_WARN.equals(getLevel())) {
			PlatformUI.getWorkbench().getDisplay().asyncExec(new Runnable() {
			    public void run() {
				    Shell activeShell = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell();
				    DialogPrinter.printWarning(activeShell, getText());
				}
			});
		}
		else {
			PlatformUI.getWorkbench().getDisplay().asyncExec(new Runnable() {
			    public void run() {
				    Shell activeShell = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell();
				    DialogPrinter.printMessage(activeShell, getText());
				}
			});
		}
//		if (CostabsConstants.LEVEL_WARN.equals(getLevel())) {
//			DialogPrinter.printWarning(null, getText());
//		}
//		else {
//			DialogPrinter.printMessage(null, getText());
//		}
	}

	@Override
	public void clean() {
	}

	public static Collection<String> getMarkers() {
		return new ArrayList<String>();
	}

}
