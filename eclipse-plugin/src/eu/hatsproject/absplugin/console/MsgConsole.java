/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package eu.hatsproject.absplugin.console;

import java.io.PrintStream;

import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.text.IDocument;
import org.eclipse.swt.SWTException;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.widgets.Display;
import org.eclipse.ui.console.MessageConsole;
import org.eclipse.ui.console.MessageConsoleStream;

import eu.hatsproject.absplugin.console.ConsoleManager.MessageType;

public class MsgConsole extends MessageConsole {

	/**
	 * Creates a new Message Console with the given name and an (optional)
	 * imageDescriptor that is used as the MessageConsole's icon
	 * 
	 * @param name
	 *            The name of the MessageConsole
	 * @param imageDescriptor
	 *            Describes an optional icon for the MessageConsole
	 */
	public MsgConsole(String name, ImageDescriptor imageDescriptor) {
		super(name, imageDescriptor);
	}

	
	public static Color getColor (MessageType type){
		RGB color;
		switch (type){
			case MESSAGE_INFO:
				color = new RGB(0,0,0);
				break;
			case MESSAGE_WARNING:
				color = new RGB(171,171,0);
				break;
			case MESSAGE_ERROR:
				color = new RGB(255,0,0);
				break;
			default:
				color = new RGB(0,0,0);
		}
		
		return new Color(Display.getDefault(),color);
	}	
	
	/**
	 * Clears the console. Does nothing if the consoles IDocument could not be
	 * found.
	 * 
	 * @throws SWTException
	 *             if the default display is already disposed
	 * @see Display#getDefault()
	 */
	public void clear(){
		final IDocument document = this.getDocument();
		if (document != null) {
			Display.getDefault().asyncExec(new Runnable() {

				@Override
				public void run() {
					document.set("");					
				}
				
			});
		}
	}
	
	private MessageConsoleStream getNewMessageConsoleStream(final MessageType type){
		final MessageConsoleStream newMessageStream = this.newMessageStream();
		newMessageStream.setActivateOnWrite(true);
		Display.getDefault().asyncExec(new Runnable() {
			
			@Override
			public void run() {
				newMessageStream.setColor(getColor(type));
				
			}
		});
		
		return newMessageStream;
	}
	
	public void print(String message, MessageType type){
		MessageConsoleStream newMessageStream = getNewMessageConsoleStream(type);
		newMessageStream.print(message);
	}
	
	public void println(String message, MessageType type){
		MessageConsoleStream newMessageStream = getNewMessageConsoleStream(type);
		newMessageStream.println(message);
	}
	
	public MessageConsoleStream getOutputStream(MessageType type){
		return getNewMessageConsoleStream(type);	
	}
	
	public PrintStream getPrintStream(MessageType type){
		return new PrintStream(getOutputStream(type));
	}

}
