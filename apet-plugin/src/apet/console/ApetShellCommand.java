package apet.console;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;

import org.eclipse.jface.preference.IPreferenceStore;

import apet.Activator;
import apet.preferences.PreferenceConstants;

public class ApetShellCommand {

	public static String APET_EXECUTABLE_PATH = "";
	public static String XML_FILE_PATH = "/tmp/pet/abs_testcases.xml";

	/**
	 * Result of the last run.
	 */
	private String result = "";
	private String error = "";

	/**
	 * Create the communicator with costabs.
	 */
	public ApetShellCommand() {
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
	 * This builds the apet comand and executes it
	 * @param entries The names of methods/functions to generate test-cases for.
	 */
	public void callAPet(ArrayList<String> entries) {
		executeCommand(buildCommand(entries));
	}

	/**
	 * Auxiliary method to build the shell command.
	 * @param entries The names of methods/functions to generate test-cases for.
	 * @return The string with the actual shell command.
	 */
	private String buildCommand(ArrayList<String> entries) {

		StringBuffer command2 = new StringBuffer();

		if (!(new File(APET_EXECUTABLE_PATH)).exists() || System.getProperty("os.name","generic").toLowerCase().contains("mac"))
				command2.append("apet");
		else
			command2.append(APET_EXECUTABLE_PATH);
		
		// Build entries
		command2.append(" -entries ");
		for (int i = 0; i < entries.size(); i++)
			command2.append("'"+entries.get(i)+"' ");

		// Build options checking preferences
		buildOptions(command2);
		
		ConsoleHandler.write(command2.toString());

		return command2.toString();
	}

	/**
	 * Auxiliary method to add to a string the options checked in preferences.
	 * @param command The String with the shell command to ABS.
	 */
	private void buildOptions(StringBuffer command) {
		IPreferenceStore store = Activator.getDefault().getPreferenceStore();
		if (store.getString(PreferenceConstants.NUM_OR_CONST) == "constraint") command.append("-constr ");
		else {
			command.append(" -dom " + store.getString(PreferenceConstants.DOM_MIN) + " " + 
						   store.getString(PreferenceConstants.DOM_MAX) + " ");
		}
		command.append("-kl " + store.getString(PreferenceConstants.KL) + " ");
		command.append("-ks " + store.getString(PreferenceConstants.KS) + " ");
		command.append("-kstr " + store.getString(PreferenceConstants.KSTR) + " ");
		if (store.getString(PreferenceConstants.SEL_CRIT) == "alp") command.append("-alp ");
		command.append("-mp " + store.getString(PreferenceConstants.MAX_PRIOR) + " ");
		command.append("-sc " + store.getString(PreferenceConstants.SCHED_POLICY) + " ");
		command.append("-pr " + store.getString(PreferenceConstants.PRUNING) + " ");
		command.append("-ql " + store.getString(PreferenceConstants.MAXQL) + " ");
		if (store.getBoolean(PreferenceConstants.TRACING) == true) command.append("-tr ");
		command.append("-verbosity " + store.getString(PreferenceConstants.VERBOSITY) + " ");
		command.append("-xmlpath " + XML_FILE_PATH);	
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
/*
			try {
				errorThread.join();
			} catch (InterruptedException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
*/			try {
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
