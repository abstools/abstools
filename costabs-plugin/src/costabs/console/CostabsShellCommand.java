package costabs.console;


import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;

import org.eclipse.jface.preference.IPreferenceStore;

import costabs.Activator;
import costabs.preferences.PreferenceConstants;
import abs.backend.prolog.*;

public class CostabsShellCommand {


	/**
	 * Result of the last run.
	 */
	private  String result = "";
	private  String error = "";

	public CostabsShellCommand() {
	}

	public String getError() {
		return error;
	}

	public String getResult() {
		return result;
	}

	public void generateProlog(String file, boolean stdlib) throws Exception {

		int numArgs;
		if (!stdlib) 
			numArgs = 4;
		else numArgs = 3;
		String[] args = new String[numArgs];
		int i = 0;
		args[i++] = "-d";
		args[i++] = "/tmp/costabs/absPL";
		if (!stdlib) args[i++] = "-nostdlib";
		args[i++] = file;

		try {
			PrologBackend.main(args); 
		
		} catch (Exception e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
			throw new Exception();
		}
	}

	public void analyze(String file, ArrayList<String> entries) {

		executeCommand(buildCommand(entries));
	}

	private String buildCommand(ArrayList<String> entries) {

		StringBuffer command2 = new StringBuffer();

		command2.append("costabs -mode analyze ");

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

	private void buildOptions(StringBuffer command) {

		IPreferenceStore store = Activator.getDefault().getPreferenceStore();
		
		command.append("-cost_model " + store.getString(PreferenceConstants.PCOST_MODEL) + " ");
		command.append("-cost_centers " + store.getString(PreferenceConstants.PCOST_CENTER) + " ");
		command.append("-size_abst " + store.getString(PreferenceConstants.PSIZE_ABST) + " ");
		command.append("-verbosity " + store.getString(PreferenceConstants.PVERBOSITY) + " ");

	}

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
