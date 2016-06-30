/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.absmodels.abs.plugin.console;


import org.absmodels.abs.plugin.console.ConsoleManager;
import org.absmodels.abs.plugin.console.MsgConsole;
import org.eclipse.ui.console.ConsolePlugin;
import org.eclipse.ui.console.IConsole;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import static org.mockito.Mockito.*;

import static org.junit.Assert.*;

public class ConsoleManagerTest {

	@Before
	public void setUp() throws Exception {
	}

	@After
	public void tearDown() throws Exception {
	}
	
	@Test
	public void testDefaultConsole() {
		MsgConsole defaultConsole = ConsoleManager.getDefault();
		assertNotNull(defaultConsole);
		assertTrue(containsConsole(defaultConsole));
		assertEquals("Default", defaultConsole.getName());
	}
	
	@Test
	public void testAddConsole(){
		IConsole console = mock(IConsole.class);
		ConsoleManager.addConsole(console);
		assertTrue(containsConsole(console));
	}
	
	@Test
	public void testRemoveConsole(){
		IConsole console = mock(IConsole.class);
		ConsoleManager.addConsole(console);
		ConsoleManager.removeConsole(console);
		assertFalse(containsConsole(console));
	}
	
	
	@Test
	public void testNewConsole() {
		MsgConsole newConsole = ConsoleManager.newConsole("test console");
		assertNotNull(newConsole);
		assertTrue(containsConsole(newConsole));
		assertEquals("test console", newConsole.getName());
	}
	
	private boolean containsConsole(IConsole c){
		for (IConsole console : ConsolePlugin.getDefault().getConsoleManager().getConsoles()) {
			if(console.equals(c)){
				return true;
			}
		}
		return false;
	}
}
