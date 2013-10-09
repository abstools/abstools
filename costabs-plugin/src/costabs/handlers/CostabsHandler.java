package costabs.handlers;


import java.io.File;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.resources.IFile;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.handlers.HandlerUtil;

import abs.backend.prolog.PrologBackend;
import abs.frontend.ast.Model;
import costabs.console.ConsoleHandler;
import costabs.console.CostabsShellCommand;
import costabs.dialogs.DialogPrinter;
import costabs.dialogs.OptionsDialog;
import costabs.exceptions.CostabsException;
import costabs.structures.CostabsConstants;
import costabs.structures.CostabsXMLFrontend;
import costabs.structures.ResultTracker;
import costabs.trackers.OutputManager;
import costabs.trackers.OutputTracker;
import costabs.utils.SourceUtils;
import eu.hatsproject.absplugin.costabslink.CostabsLink;


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
			IEditorPart editor = SourceUtils.obtainActiveEditor();
			String absFile = SourceUtils.extractResource(SourceUtils.obtainActiveEditor()).getLocation().toString();

			// Creating the costabs tmp directory
			File f = new File(CostabsConstants.TMP_DIR);
			f.mkdirs();

			if (CostabsLink.ENTRIES_STRINGS.size() <= 0) {
				DialogPrinter.printError(shellEclipse, 
						new Exception ("At least one function or method must be selected in the outline view"), 
						"SACO cannot analyze");
				return null;
			}
			
			OptionsDialog mDialog = new OptionsDialog(shellEclipse);
			mDialog.open();

			if (mDialog.getReturnCode() == OptionsDialog.CANCEL) {
				return null;
			} 

			if (mDialog.getAnalysisSelected() == null) {
				DialogPrinter.printError(shellEclipse, 
						new Exception ("At least one analysis must be selected in the dialog"), 
						"SACO cannot analyze");
				return null;
			}

			// Global boolean to use a given output.xml instead of executing a command
			if (!CostabsConstants.DEBUG_OUTPUT) {
				CostabsXMLFrontend.cleanPrevResults();
				callPrologBackend(absFile);
				shell.analyze(absFile, CostabsLink.ENTRIES_STRINGS, mDialog.getAnalysisSelected());
			}
			
			processOutput();
			
			editor.setFocus();
			
			OutputManager.getInstance().updateView(SourceUtils.getActiveFile());
				
		} catch (CostabsException e){
			e.printStackTrace();
			DialogPrinter.printError(shellEclipse, e, "An error has ocurred during the analysis");

		} catch (Exception e) {
			e.printStackTrace();
			DialogPrinter.printError(shellEclipse, 
						new Exception("Unkwon error: " + e.getMessage()), 
						"An unkown error has ocurred during the analysis");
		}

		return null;
	}

	private void callPrologBackend(String filename) throws Exception {

		if (!filterUnreachable){
			int numArgs = 3;
			String[] args = new String[numArgs];
			int i = 0;
			args[i++] = "-d";
			args[i++] = CostabsConstants.TMP_DIR;
			args[i++] = filename;
			PrologBackend.runFromShell(args);
		} else {		
			Model model = CostabsLink.ABS_NATURE.getCompleteModel(); 
			PrologBackend.runFromPlugin(model,CostabsConstants.TMP_DIR,CostabsConstants.ABS_PL_FILE,CostabsLink.ENTRIES_NODES);
		}
	}


	private void processOutput () throws Exception {

		IFile file = SourceUtils.getActiveFile();

		try {
			OutputTracker tracker = OutputManager.getInstance().getOutputTracker(file);
			
			if (tracker!= null) {
				tracker.cleanAllInfo();
			}
			
			OutputManager.getInstance().loadResults(file);
			
			OutputTracker tracker2 = OutputManager.getInstance().getOutputTracker(file);
			tracker2.trackResults();
			
		}
		catch (Exception e) {
			throw e;
		}

	}

}
