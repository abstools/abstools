package apet.handlers;


import java.io.File;
import java.io.IOException;
import java.io.PrintStream;

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

import abs.backend.prolog.PrologBackend;
import abs.frontend.ast.Model;
import abs.frontend.parser.Main;
import apet.absunit.ABSUnitTestCaseTranslator;
import apet.console.ApetShellCommand;
import apet.console.ConsoleHandler;
import apet.preferences.ApetPreferences;
import apet.testCases.ApetTestSuite;
import apet.testCases.XMLParser;
import apet.utils.SourceUtils;
import org.absmodels.abs.plugin.costabslink.CostabsLink;

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
	 * This flag dictates if the generated test cases should be type checked
	 * against the SUT.
	 */
	private boolean validate = false;
	
	/**
	 * The file name to store ABSUnit test cases 
	 */
	private final String absUnitOutputFile = "absunit-testcase.abs";
	
	/**
	 * The default file location to store ABSUnit test cases
	 */
	private final String defaultAbsUnitOutputFile = "/tmp/" + absUnitOutputFile;
	
	/**
	 * The constructor
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
					printError(shell);
					ApetTestSuite suite = callXMLParser();
					if (translate) {
						Model m = getABSModel(absFile);
						generateABSUnitTests(m,suite, 
							new File(new File(absFile).getParentFile(), absUnitOutputFile));
					}
				}	
				ConsoleHandler.write(shell.getResult());
			}
		} catch (Exception e) {
			ConsoleHandler.write(shell.getError());
			e.printStackTrace(new PrintStream(
				ConsoleHandler.getDefault().newMessageStream()));
		}
		return null;
	}
	
	void printError(ApetShellCommand shell) {
		String error = shell.getError();
		if (error != null && ! error.isEmpty()) {
			ConsoleHandler.write(error);
		}
	}
	
	/**
	 * Connection to ABSUnit translator
	 * @param model
	 * @param suite
	 * @param outputFile 
	 * @throws IOException 
	 */
	private void generateABSUnitTests(Model model, ApetTestSuite suite, 
			File outputFile) throws IOException {
		
		if (suite == null) {
			ConsoleHandler.write("aPET error: Error generating ABSUnit test suite");
			return;
		}
		
		if (outputFile.isDirectory()) {
			ConsoleHandler.write("aPET error: cannot create ABSUnit test cases to "+outputFile);
			return;
		}

		if (outputFile.isFile() && outputFile.exists()) {
			outputFile.delete();
		} else {
			outputFile.createNewFile();
		}
		
		ABSUnitTestCaseTranslator generator = 
		        new ABSUnitTestCaseTranslator(model, outputFile, true); 
		
		if (! generator.hasABSUnit()) {
			ConsoleHandler.write("aPET error: cannot find ABSUnit packages");
			return;
		}
		
		generator.generateABSUnitTests(suite, validate);
	}
	
	private Model getABSModel(String filename) throws Exception {
		int numArgs = 3;
		String[] args = new String[numArgs];
		int i = 0;
		args[i++] = "-v";
		args[i++] = "-notypecheck";
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
