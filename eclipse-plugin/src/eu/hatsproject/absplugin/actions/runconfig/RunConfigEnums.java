package eu.hatsproject.absplugin.actions.runconfig;

import eu.hatsproject.absplugin.debug.model.Debugger;
import eu.hatsproject.absplugin.debug.scheduling.SchedulingStrategy;
import abs.backend.java.debugging.GraphicalDebugger;
import abs.backend.java.scheduling.InteractiveScheduler;
import abs.backend.java.scheduling.RandomSchedulingStrategy;
import abs.backend.java.visualization.UMLSequenceChart;

public class RunConfigEnums {

	/**
	 * Contains all possible observer.
	 * The user can choose one of them in the run configuration.
	 */
	public enum DebuggerObserver {
		ECLIPSE(Debugger.class.getName(),"Eclipse observer", true),
		GRAPHICAL(GraphicalDebugger.class.getName(), "Graphical observer", false),
		UML (UMLSequenceChart.class.getName(), "UML observer", false);
		
		private String className;
		private String userReadableName;
		private boolean isSelectedDefault;
		private DebuggerObserver(String className, String userReadableName, boolean isSelectedDefault) {
			this.className = className;
			this.userReadableName = userReadableName;
			this.isSelectedDefault = isSelectedDefault;
		}
		
		public String getUserReadableName(){
			return userReadableName;
		}
		
		public String getClassName(){
			return className;
		}
		
		public String getCommand(){
			return getEmptyCommand()+this.getClassName();
		}
		
		public static String getEmptyCommand(){
			return "-Dabs.systemobserver=";
		}
		
		public boolean getDefaultSelection(){
			return isSelectedDefault;
		}
		
		/**
		 * To show UML diagrams, we have to start SDEdit
		 */
		public boolean needsSDEdit() {
			return this.equals(UML);
		}
		
		/**
		 * Returns Observer with the given userReadableName
		 * or a default value, if no observer was found.
		 * Default is DebuggerObserver.ECLIPSE
		 * 
		 * @return DebuggerObserver
		 */
		public static DebuggerObserver valueOfUserReadableName(String userReadableName){
			DebuggerObserver[] observer = DebuggerObserver.values();
			for (int i = 0; i < observer.length; i++) {
				if(userReadableName.equals(observer[i].getUserReadableName())){
					return observer[i];
				}
			}
			
			System.err.println("Observer not found: "+userReadableName);
			return ECLIPSE; //return a default value
		}
		
		/**
		 * Returns Observer with the given class name 
		 * or null, if no observer was found.
		 * 
		 * @return DebuggerObserver
		 */
		public static DebuggerObserver valueOfClassName(String className){
			DebuggerObserver[] observer = DebuggerObserver.values();
			for (int i = 0; i < observer.length; i++) {
				if(className.equals(observer[i].getClassName())){
					return observer[i];
				}
			}
			
			return null;
		}
	}
	
	/**
	 * Contains all possible scheduler.
	 * The user can choose one of them in the run configuration.
	 */
	public enum DebuggerScheduler {
		ECLIPSE(SchedulingStrategy.class.getName(),"Eclipse scheduler"),
		INTERACTIVE (InteractiveScheduler.class.getName(), "Interactive scheduler"),
		RANDOM (RandomSchedulingStrategy.class.getName(), "Random scheduler"),
		DEFAULT("", "No scheduler");
		
		
		private String className;
		private String userReadableName;
		private DebuggerScheduler(String className, String userReadableName){
			this.className = className;
			this.userReadableName = userReadableName;
		}
		
		public String getUserReadableName(){
			return userReadableName;
		}
		
		public String getClassName(){
			return className;
		}
		
		public String getCommand(){
			if (this.className != null && !this.className.isEmpty())
				return "-Dabs.totalscheduler="+this.getClassName();
			else
				return "";
		}
		
		/**
		 * Returns Scheduler with the given userReadableName
		 * or a default value, if no scheduler was found.
		 * The default value equals DebuggerScheduler.getDefaultScheduler()
		 * 
		 * @return DebuggerObserver
		 */
		public static DebuggerScheduler valueOfUserReadableName(String userReadableName){
			DebuggerScheduler[] scheduler = DebuggerScheduler.values();
			for (int i = 0; i < scheduler.length; i++) {
				if(userReadableName.equals(scheduler[i].getUserReadableName())){
					return scheduler[i];
				}
			}
			
			System.err.println("Observer not found: "+userReadableName);
			return DebuggerScheduler.getDefaultScheduler();
		}
		
		public static DebuggerScheduler getDefaultScheduler(){
			return DEFAULT;
		}
	}
	
}
