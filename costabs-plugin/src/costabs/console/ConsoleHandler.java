package costabs.console;

import java.io.OutputStream;

import org.eclipse.ui.console.ConsolePlugin;
import org.eclipse.ui.console.IConsole;
import org.eclipse.ui.console.IConsoleManager;
import org.eclipse.ui.console.MessageConsole;
import org.eclipse.ui.console.MessageConsoleStream;


public class ConsoleHandler {

	public static MessageConsole defaultConsole = null;

		public static final String ID_CONSOLE_VIEW = "CostabsConsole";

		public static MessageConsole findCostabsConsole() {
		      ConsolePlugin plugin = ConsolePlugin.getDefault();
		      IConsoleManager conMan = plugin.getConsoleManager();
		      IConsole[] existing = conMan.getConsoles();
		      for (int i = 0; i < existing.length; i++)
		         if (ID_CONSOLE_VIEW.equals(existing[i].getName()))
		            return (MessageConsole) existing[i];
		      //no console found, so create a new one
		      MessageConsole myConsole = new MessageConsole(ID_CONSOLE_VIEW, null);
		      conMan.addConsoles(new IConsole[]{myConsole});
		      return myConsole;
		   }

		/**
		 * Returns the Costabs Console Output Stream to write to the console.
		 * @return Costabs Console Output Stream to write to the console.
		 */
		public static OutputStream getCostaConsoleStream () {
			MessageConsole myConsole = findCostabsConsole();
			MessageConsoleStream out = myConsole.newMessageStream();
			out.setActivateOnWrite(true);
			return out;
		}

		public static void write(String text) {
			if (defaultConsole != null) {
				MessageConsoleStream out = defaultConsole.newMessageStream();
				out.setActivateOnWrite(true);
				out.println(text);
			}
		}
	
		public static void clearConsole() {

			if (defaultConsole != null) 
				defaultConsole.clearConsole();
		}
		
		
		/**
		 * Gives the default console with an empty title
		 * @return Default Message Console
		 */
		public static MessageConsole getDefault() {
			if (defaultConsole == null){
				defaultConsole = new MessageConsole("Default",null);
				addConsole(defaultConsole);
			}
			return defaultConsole;
		}
		
		/**
		 * Adds consoles to the console View
		 * @param console
		 */
		public static void addConsole(IConsole console){
			ConsolePlugin.getDefault().getConsoleManager().addConsoles(new IConsole[] {console});
		}
}
