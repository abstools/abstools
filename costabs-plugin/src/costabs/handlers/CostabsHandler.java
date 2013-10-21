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

import abs.backend.prolog.PrologBackend;
import abs.frontend.ast.Model;
import costabs.console.ConsoleHandler;
import costabs.console.CostabsShellCommand;
import costabs.dialogs.OptionsDialog;
import costabs.markers.UBMarker;
import costabs.structures.ResultTracker;
import costabs.utils.SourceUtils;
import org.absmodels.abs.plugin.costabslink.CostabsLink;


/**
 * Our sample handler extends AbstractHandler, an IHandler base class.
 * @see org.eclipse.core.commands.IHandler
 * @see org.eclipse.core.commands.AbstractHandler
 */
public class CostabsHandler extends AbstractHandler {

	public static ResultTracker STORAGE_COSTABS = new ResultTracker();
	boolean filterUnreachable = true; 
	// The default is true. In that case the whole project is compiled and the frontend 
	// calls Antonio's reachability filtering. Otherwise, there's no filtering and it would 
	// only work with a single ABS file
	
	/**
	 * The constructor.
	 */
	public CostabsHandler() {
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
					shell.analyze(absFile, CostabsLink.ENTRIES_STRINGS);
					updateUpperBounds();
					updateMarkers();
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
    	
		if (!filterUnreachable){
			int numArgs = 3;
			String[] args = new String[numArgs];
			int i = 0;
			args[i++] = "-d";
			args[i++] = "/tmp/costabs/absPL";
			args[i++] = filename;
			PrologBackend.runFromShell(args);
		} else {		
			Model model = CostabsLink.ABS_NATURE.getCompleteModel(); //getCurrentABSModel();
			PrologBackend.runFromPlugin(model,"/tmp/costabs/absPL","abs.pl",CostabsLink.ENTRIES_NODES);
		}
	}
	
	/*
	private Model getCurrentABSModel() throws Exception{
		IWorkbench iworkbench = PlatformUI.getWorkbench();
		IWorkbenchWindow window = iworkbench.getActiveWorkbenchWindow();
		IEditorPart editorPart = window.getActivePage().getActiveEditor();
		IProject project = ActionUtils.getCurrentProject(window, editorPart);
		return JavaJob.getModelFromProject(project);
	}*/
		
	private void updateUpperBounds() {
		
		// First, get the results from costabs
		STORAGE_COSTABS.addXMLResults();
		
		// and then add the line numbers from ABS Outline View
		STORAGE_COSTABS.addLineNumbers(CostabsLink.ENTRIES_STRINGS, CostabsLink.LINE_ITEMS);
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
