package costabs.dialogs;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IWorkbenchWindow;

import costabs.Activator;

public class DialogPrinter {

	public static void printError (Shell shell, Exception e, String error) {
		Status status = new Status(IStatus.ERROR, "SACO", 0,
				e.getMessage(), null);
		ErrorDialog.openError(shell, "SACO Error", error, status);		
		
//		MessageDialog.openInformation(
//				window.getShell(),
//				"Problem",
//				"An error has ocurred: " + e.getMessage());
	}
	
	public static void logError (Exception e) {
		Status status = new Status(IStatus.ERROR, "SACO", 0,
				e.getMessage(), e);
		Activator.getDefault().getLog().log(status);
	}
	
	public static void printMessage (Shell shell, String message) {
		MessageDialog.openInformation(shell, "SACO message", message);		
	}

	public static void printWarning (Shell shell, String message) {
		MessageDialog.openWarning(shell, "SACO  message",message);		
	}

	
}
