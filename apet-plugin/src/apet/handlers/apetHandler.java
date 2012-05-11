package apet.handlers;


import java.io.File;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.jface.preference.IPreferenceNode;
import org.eclipse.jface.preference.IPreferencePage;
import org.eclipse.jface.preference.PreferenceDialog;
import org.eclipse.jface.preference.PreferenceManager;
import org.eclipse.jface.preference.PreferenceNode;

import org.eclipse.swt.widgets.Shell;

import org.eclipse.ui.handlers.HandlerUtil;

import apet.absunit.ABSUnitTestCaseTranslator;
import apet.console.ConsoleHandler;
//import apet.dialogs.OptionsDialog;
import apet.preferences.ApetPreferences;
import apet.testCases.ApetTestSuite;
import apet.testCases.XMLParser;
import apet.utils.SourceUtils;
import eu.hatsproject.absplugin.costabslink.CostabsLink;
import abs.backend.prolog.PrologBackend;
import abs.frontend.ast.Model;
import abs.frontend.parser.Main;

import apet.console.ApetShellCommand;

/**
 * Our sample handler extends AbstractHandler, an IHandler base class.
 * @see org.eclipse.core.commands.IHandler
 * @see org.eclipse.core.commands.AbstractHandler
 */
public class apetHandler extends AbstractHandler {

	/**
	 * This flag dictates if test cases are to be translated into ABSUnit
	 * Currently only modify manually during development
	 */
	private boolean translate = true;
	
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
				ErrorDialog.openError(shellEclipse, "aPET Error", "aPET cannot be run.", status);		
			} else {
				/*OptionsDialog mDialog = new OptionsDialog (shellEclipse);
				mDialog.open();				
				if (mDialog.getReturnCode() == OptionsDialog.CANCEL) {
					ConsoleHandler.write("Don't do anything, cancelled by the user");*/
				
				IPreferencePage page = new ApetPreferences();
				PreferenceManager mgr = new PreferenceManager();
				IPreferenceNode node = new PreferenceNode("1", page);
				mgr.addToRoot(node);
				PreferenceDialog dialog = new PreferenceDialog(shellEclipse, mgr);
				dialog.create();
				dialog.setMessage("aPET preferences");
				dialog.open();
		
				if (dialog.getReturnCode() == PreferenceDialog.CANCEL) {
					return null;
				} else {
					callPrologBackend(absFile);
					shell.callAPet(CostabsLink.ENTRIES_STRINGS);
					ApetTestSuite suite = callXMLParser();
					if (translate) {
						Model m = getABSModel(absFile);
						generateABSUnitTests(m,suite);
					}
				}	
				ConsoleHandler.write(shell.getResult());
			}
		} catch (Exception e) {
			ConsoleHandler.write(shell.getError());
		}
		return null;
	}
	
	/**
	 * Connection to ABSUnit translator -- WORK IN PROGRESS
	 * @param model
	 * @param suite
	 */
	private void generateABSUnitTests(Model model, ApetTestSuite suite) {
		
		if (suite == null) {
			System.out.println("aPET error: Error generating ABSUnit test suite");
			return;
		}
		
		File g = new File("//tmp//absunit");
		g.mkdirs();
		ABSUnitTestCaseTranslator generator = new ABSUnitTestCaseTranslator(model, g); 
		
		if (! generator.hasABSUnit()) {
			System.out.println("aPET error: cannot find ABSUnit packages");
			return;
		}
		
		generator.generateABSUnitTests(suite);
	}
	
	private Model getABSModel(String filename) throws Exception {
		int numArgs = 2;
		String[] args = new String[numArgs];
		int i = 0;
		args[i++] = "-v";
		args[i++] = filename;
		
		Main main = new Main();
		return main.parse(args);
	}
	
	private void callPrologBackend(String filename) throws Exception {
    	
		int numArgs = 3;
		String[] args = new String[numArgs];
		int i = 0;
		args[i++] = "-d";
		args[i++] = "/tmp/costabs/absPL";
		args[i++] = filename;
		PrologBackend.runFromShell(args);
				
		/*Model model = CostabsLink.ABS_NATURE.getCompleteModel(); //getCurrentABSModel();
		PrologBackend.runFromPlugin(model,"/tmp/costabs/absPL","abs.pl",CostabsLink.ENTRIES_NODES);
		*/
	}
	
	private ApetTestSuite callXMLParser(){
		// TODO The xml filename should be a constant
		XMLParser parser = new XMLParser(ApetShellCommand.XML_FILE_PATH);
		ApetTestSuite suite = null;
		try {
			suite = parser.read();
			System.out.println("Test cases parsed from the xml file and stored in the APetTestSuite");
		} catch (Exception e) {
			System.out.println("aPET error: Error parsing the XML file");
		}
		return suite;
	}
}
