/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.absmodels.abs.plugin.console;

import java.io.IOException;
import java.io.PrintStream;

import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.console.ConsolePlugin;
import org.eclipse.ui.console.IConsole;
import org.eclipse.ui.console.IConsoleConstants;
import org.eclipse.ui.console.IOConsoleInputStream;

/**
 * Class for handling the Console conveniently
 * @author cseise
 *
 */
public class ConsoleManager {
	/**
	 * The default console singleton used by getDefault()
	 */
	private static MsgConsole     defaultConsole = null;
	
	/**
	 * Type of the Console Message (Info,Warning or Error)
	 *
	 */
	public enum MessageType{
		MESSAGE_INFO,
		MESSAGE_WARNING,
		MESSAGE_ERROR
	}
	
	private ConsoleManager(){
	}
	
	/**
	 * Gives the default console with an empty title
	 * @return Default Message Console
	 */
	public static MsgConsole getDefault() {
		if (defaultConsole == null){
			defaultConsole = newConsole("Default");
			//addConsole(defaultConsole);
		}
		return defaultConsole;
	}
		
	/**
	 * Removes a console from the console View
	 * @param console
	 */
	public static void removeConsole(IConsole console){
		ConsolePlugin.getDefault().getConsoleManager().removeConsoles(new IConsole[] {console});
	}

	/**
	 * Adds consoles to the console View
	 * @param consoles
	 */
	public static void addConsole(IConsole[] consoles){
		ConsolePlugin.getDefault().getConsoleManager().addConsoles(consoles);
	}
	
	/**
	 * Adds consoles to the console View
	 * @param console
	 */
	public static void addConsole(IConsole console){
		ConsolePlugin.getDefault().getConsoleManager().addConsoles(new IConsole[] {console});
	}
	
	/**
	 * Create a new Console with the specified title and adds it to the console view
	 * @param title The desired title of the console
	 * @return The new Message Console
	 */
	public static MsgConsole newConsole(String title){
		MsgConsole mc = new MsgConsole(title,null);
		addConsole((IConsole)mc); 
		
		System.setOut(new PrintStream(mc.getOutputStream(MessageType.MESSAGE_INFO)));
//		try {
			IOConsoleInputStream str = mc.getInputStream();
			//str.reset();
			System.setIn(str);
//		} catch (IOException e) {
//			// TODO Auto-generated catch block
//			e.printStackTrace();
//		}
		
		//defaultConsole = mc;
		return mc;
	}
	
	
	/**
	 * Method to show the ConsoleView.
	 * @throws PartInitException if the ConsoleView cannot be displayed
	 */
	public static void displayConsoleView() throws PartInitException{
		
		IWorkbenchWindow activeWorkbenchWindow = PlatformUI.getWorkbench().getActiveWorkbenchWindow();
		
		
		
		if (activeWorkbenchWindow != null){
			IWorkbenchPage activePage = activeWorkbenchWindow.getActivePage();
			if (activePage != null){
				activePage.showView(IConsoleConstants.ID_CONSOLE_VIEW,null,IWorkbenchPage.VIEW_VISIBLE);
			}
		}	
	}


}
