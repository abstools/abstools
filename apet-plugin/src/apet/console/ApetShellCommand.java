package apet.console;

import java.io.IOException;
import java.util.ArrayList;

import org.eclipse.jface.preference.IPreferenceStore;
import apet.preferences.PreferenceConstants;

public class ApetShellCommand {

	public static String COSTABS_EXECUTABLE_PATH = "";

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

		// byMZ: Here I have to take the path of the apet binary executable instead of apet
		command2.append("apet ");
		
		// Build entries
		command2.append("-entries ");
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
		
		command.append("-xmlpath /tmp/pet/abs_testcases.xml");
/*		IPreferenceStore store = Activator.getDefault().getPreferenceStore();

		// -c CovCrit K: CovCrit:{dpk,bck} depth-k o block-k
		command.append(" -c "
				+store.getString(PreferenceConstants.PCOVERAGE_CRITERION)+" "
				+store.getInt(PreferenceConstants.PCOVERAGE_CRITERION_NUM)+" ");
		// -td NumOrC: NumOrC:{num,constraint}
		command.append(" -td "
				+store.getString(PreferenceConstants.PNUMERIC)+" ");
		// -d none 
		// -d Min Max: Rango de integers
	
		if(store.getString(PreferenceConstants.PNUMERIC).equals("num")){
			command.append(" -d "
					+store.getInt(PreferenceConstants.PRANGEMIN)+" "
					+store.getInt(PreferenceConstants.PRANGEMAX)+" ");
		}else{
			command.append(" -d none ");
		}
		
					
		// -g: obtiene el generador de casos de prueba en un archivo
		// -g no: para deshabilitar la creación del generador
		if(store.getBoolean(PreferenceConstants.PTEST_CASE_GENERATION))
			command.append(" -g ");
		// -al: No sé que significa esta opción pero funciona como la anterior
		// -al no: para deshabilitarlo
		// if (AlYes) command.append("-al ");
		if(store.getBoolean(PreferenceConstants.PREFERENCES_ALIASING))
			command.append(" -al ");
		// -l Labeling: donde Labeling: {ff,leftmost,min,max}
		// Es el orden de etiquetado de las variables en las restricciones que usa pet
		// if (Labeling == ff) command.append("-l ff ");
		// else if (Labeling == leftmost) command.append("-l leftmost ");
		// else if (Labeling == min) command.append("-l min ");
		// else command.append("-l max ");
		command.append(" -l "
				+store.getString(PreferenceConstants.PLABELING)+" ");
		// -v: Verbosity level (1 a 3)
		// command.append("-v "+VerbosityLevel);
		command.append(" -v "
				+store.getString(PreferenceConstants.PVERBOSITY)+" ");
		// -w: Obtiene el CLP decompilado
		// if (WYes) command.append("-w ");
		if(store.getBoolean(PreferenceConstants.PCLP))
			command.append(" -w ");
		// -tr Tracings: Tracings puede ser {statements,blocks}
		// tenemos que incluir la opción de none
		// if (Tracings == statements) command.append("-tr statements ");
		// else if (Tracings == blocks) command.append("-tr blocks ");
		if(!store.getString(PreferenceConstants.PTRACING).equals("none"))
			command.append(" -tr "+store.getString(PreferenceConstants.PTRACING)+" ");
 	*/
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
