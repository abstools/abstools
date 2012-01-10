package costabs.console;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;

import org.eclipse.jface.preference.IPreferenceStore;

import costabs.Activator;
import costabs.preferences.PreferenceConstants;

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
	public void analyze(String file, ArrayList<String> entries) {

		executeCommand(buildCommand(entries));
	}

	/**
	 * Auxiliar method, just to build the shell command with the entries and
	 * preferences setup.
	 * @param entries The entries to be used in costabs.
	 * @return The string that has the shell command to use costabs.
	 */
	private String buildCommand(ArrayList<String> entries) {

		StringBuffer command2 = new StringBuffer();
		
		if ((new File(COSTABS_EXECUTABLE_PATH)).exists())
			command2.append(COSTABS_EXECUTABLE_PATH);
		else // In case the executable is not there we try with the costabs command	
			command2.append("costabs");
		
		command2.append(" -mode analyze ");
				
		// Build entries
		command2.append("-entries ");
		for (int i = 0; i < entries.size(); i++) {
			command2.append("'"+entries.get(i)+"' ");
		}

		// Build options checking preferences
		buildOptions(command2);

		ConsoleHandler.write(command2.toString());

		return command2.toString();
	}

	/**
	 * Auxiliar method to add to a string the options checked in preferences.
	 * @param command The String with the shell command to ABS.
	 */
	private void buildOptions(StringBuffer command) {

		IPreferenceStore store = Activator.getDefault().getPreferenceStore();
		
		command.append("-cost_model " + store.getString(PreferenceConstants.PCOST_MODEL) + " ");
		command.append("-cost_centers " + store.getString(PreferenceConstants.PCOST_CENTER) + " ");
		command.append("-norm " + store.getString(PreferenceConstants.PSIZE_ABST) + " ");
		if (store.getString(PreferenceConstants.PASYMPTOTIC) == "yes") command.append("-a ");
		if (store.getString(PreferenceConstants.PDEBUG_MODE) == "yes") command.append("-debug ");
		command.append("-verbosity " + store.getString(PreferenceConstants.PVERBOSITY) + " ");

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
