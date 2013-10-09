package costabs.console;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import org.eclipse.jface.preference.IPreferenceStore;

import costabs.Activator;
import costabs.beans.Analyses;
import costabs.beans.Analysis;
import costabs.beans.Option;
import costabs.exceptions.CostabsException;
import costabs.preferences.PreferenceConstants;
import costabs.preferences.PreferencesManager;
import costabs.structures.CostabsConstants;

public class CostabsShellCommand {

	public static String COSTABS_EXECUTABLE_PATH = "";

	/**
	 * Result of the last run.
	 */
	private String result = "";
	private String error = "";

	/**
	 * Create the communicator with costabs.
	 */
	public CostabsShellCommand() {
	}

	/**
	 * Get the error message from the last execution of costabs.
	 * @return
	 */
	public String getError() {
		return error;
	}

	/**
	 * Get the result from the last execution of costabs.
	 * @return
	 */
	public String getResult() {
		return result;
	}

	/**
	 * Call to costabs to execute with the actual preferences setup.
	 * @param file ABS to be passed to costabs.
	 * @param entries The names of methods / functions to use in costabs.
	 */
	public void analyze(String file, ArrayList<String> entries, String analysis) throws CostabsException {
		executeCommand(buildCommand(file,entries, analysis));
	}

	
	public static final String PARAMS_ID = "$PARAMS";
	public static final String FILE_ID = "$FILE";
	public static final String ENTRIES_ID = "$ENTRIES";
	
	
	/**
	 * Auxiliar method, just to build the shell command with the entries and
	 * preferences setup.
	 * @param entries The entries to be used in costabs.
	 * @return The string that has the shell command to use costabs.
	 */
	private String buildCommand(String file, ArrayList<String> entries, String idAnalysis) throws CostabsException {

		Analysis analysis = PreferencesManager.getInstance().getAnalysis(idAnalysis);
		String aCommand = analysis.getCommand();
		
//		if ((new File(COSTABS_EXECUTABLE_PATH)).exists())
//			command2.append(COSTABS_EXECUTABLE_PATH);
//		else // In case the executable is not there we try with the costabs command	
//			command2.append(aCommand);

		// Build entries
		StringBuffer entriesBuf = new StringBuffer ();
		for (int i = 0; i < entries.size(); i++) {
			entriesBuf.append("'"+entries.get(i)+"' ");
		}

		// Build options checking preferences
		String options = buildOptions(idAnalysis);
		

		String c2 = aCommand.replace(PARAMS_ID, options);
		String c3 = c2.replace(FILE_ID, file);
		String cFinal = c3.replace(ENTRIES_ID, entriesBuf.toString());

		ConsoleHandler.write(cFinal.toString());
		
		
		return cFinal;
	}

	/**
	 * Auxiliar method to add to a string the options checked in preferences.
	 * @param command The String with the shell command to ABS.
	 */
	private String buildOptions(String idAnalysis) throws CostabsException {
		StringBuffer command = new StringBuffer();

		//		command.append("-cost_model " + store.getString(PreferenceConstants.PCOST_MODEL) + " ");
		//		command.append("-cost_centers " + store.getString(PreferenceConstants.PCOST_CENTER) + " ");
		//		command.append("-norm " + store.getString(PreferenceConstants.PSIZE_ABST) + " ");
		//		if (store.getString(PreferenceConstants.PASYMPTOTIC) == "yes") command.append("-a ");
		//		if (store.getString(PreferenceConstants.PDEBUG_MODE) == "yes") command.append("-debug ");
		//		command.append("-verbosity " + store.getString(PreferenceConstants.PVERBOSITY) + " ");


		IPreferenceStore store = Activator.getDefault().getPreferenceStore();
		Analysis analysis = PreferencesManager.getInstance().getAnalysis(idAnalysis);
		for(Option op: analysis.getOptions().getOptions()) {
			String optId = PreferencesManager.getInstance().getOptionId(analysis.getAnalysisId(), op.getOptname());
			if (PreferencesManager.getInstance().isBooleanOption(optId)) {
				if (store.getBoolean(optId)) {
					command.append(" -" + op.getOptname() + " ");
				}
			}
			else {
				command.append(" -" + op.getOptname() + " " + store.getString(optId));
			}
		}
		return command.toString();
	}

	/**
	 * Create a process to execute the command given by argument in a shell.
	 * @param command The shell command to be executed.
	 * @return The state of finalization of the process.
	 */
	public boolean executeCommand(String command) {
		StreamReaderThread outputThread;
		StreamReaderThread errorThread;
		try {
			// Execute the command using bash
			String[] commands = new String[] {"sh", "-c", command};

			// Preparing the commands
			ProcessBuilder processBuilder = new ProcessBuilder(commands);

			// Starting the analysis
			Process proc = processBuilder.start();
			outputThread=new StreamReaderThread(proc.getInputStream());
			errorThread=new StreamReaderThread(proc.getErrorStream());

			errorThread.start();
			outputThread.start();

			try {
				errorThread.join();
			} catch (InterruptedException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
			try {
				outputThread.join();
			} catch (InterruptedException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
			proc.waitFor();
			result=outputThread.getContent();
			error=errorThread.getContent();
		}
		catch (IOException e) {
			System.out.println("Error to execute the command : "+e);
			return false;
		} catch (InterruptedException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		return true;
	}




}
