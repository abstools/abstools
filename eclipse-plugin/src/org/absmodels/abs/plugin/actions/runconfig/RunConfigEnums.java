/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.absmodels.abs.plugin.actions.runconfig;

import org.absmodels.abs.plugin.debug.model.Debugger;
import org.absmodels.abs.plugin.debug.scheduling.SchedulingStrategy;

import abs.backend.java.absunit.ABSTestObserver;
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
		ECLIPSE(Debugger.class.getName(),"Eclipse Debugger", true),
		GRAPHICAL(GraphicalDebugger.class.getName(), "Graphical Debugger", false),
		UML (UMLSequenceChart.class.getName(), "UML Sequence Diagram", false),
		ABSUnit (ABSTestObserver.class.getName(), "ABS Unit Testrunner", false);
        
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
		ECLIPSE(SchedulingStrategy.class.getName(),"Eclipse Debugger"),
		INTERACTIVE (InteractiveScheduler.class.getName(), "Interactive Debugger"),
		RANDOM (RandomSchedulingStrategy.class.getName(), "Random Scheduler"),
		DEFAULT("", "Default Scheduler");
		
		
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
