package costabs.handlers;


import java.io.File;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.handlers.HandlerUtil;
import org.eclipse.ui.part.FileEditorInput;

import costabs.console.ConsoleHandler;
import costabs.console.CostabsShellCommand;
import costabs.dialogs.OptionsDialog;
import costabs.markers.UBMarker;
import costabs.structures.ResultTracker;
import costabs.utils.SourceUtils;
import eu.hatsproject.absplugin.costabslink.CostabsLink;


/**
 * Our sample handler extends AbstractHandler, an IHandler base class.
 * @see org.eclipse.core.commands.IHandler
 * @see org.eclipse.core.commands.AbstractHandler
 */
public class costabsHandler extends AbstractHandler {

	public static ResultTracker STORAGE_COSTABS = new ResultTracker();
	
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
		
		CostabsShellCommand shell = new CostabsShellCommand();
		
		try {
			ConsoleHandler.defaultConsole = ConsoleHandler.findCostabsConsole();

			String absFile = SourceUtils.extractResource(SourceUtils.obtainActiveEditor()).getLocation().toString();

			// Do costabs directory
			File f = new File("//tmp//costabs//absPL");
			f.mkdirs();
			
			if (CostabsLink.SELECTED_ITEMS.size() <= 0) {
				Status status = new Status(IStatus.ERROR, "costabs", 0,
			            "At least one function or method must be selected in the outline view.", null);
				ErrorDialog.openError(shellEclipse, "Costa Error", "Costa cannot analyze.", status);
				
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
					updateUpperBounds();
					updateMarkers();
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
		
	private void updateUpperBounds() {
		
		// First, get the results from costabs
		STORAGE_COSTABS.addXMLResults();
		
		// and then add the line numbers from ABS Outline View
		STORAGE_COSTABS.addLineNumbers(CostabsLink.SELECTED_ITEMS, CostabsLink.LINE_ITEMS);
	}
	
	private void updateMarkers() {
		
		// Get the IFile from editor
		IWorkbench iworkbench = PlatformUI.getWorkbench();
		IWorkbenchWindow iworkbenchwindow = iworkbench.getActiveWorkbenchWindow();
		IWorkbenchPage iworkbenchpage = iworkbenchwindow.getActivePage();
		IEditorPart ieditorpart = iworkbenchpage.getActiveEditor();
		IEditorInput input = ieditorpart.getEditorInput();
		IFile file = ((FileEditorInput)input).getFile();
		
		// Clean the actual markers
		UBMarker ub = new UBMarker();
		ub.removeAllMarkers(file);
		
		// Update the editor filling with the actual markers
		STORAGE_COSTABS.fillMarkers(file);
		
	}


}
