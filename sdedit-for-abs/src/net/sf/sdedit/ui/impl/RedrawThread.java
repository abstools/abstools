// Copyright (c) 2006 - 2008, Markus Strauch.
// All rights reserved.
// 
// Redistribution and use in source and binary forms, with or without
// modification, are permitted provided that the following conditions are met:
// 
// * Redistributions of source code must retain the above copyright notice, 
// this list of conditions and the following disclaimer.
// * Redistributions in binary form must reproduce the above copyright notice, 
// this list of conditions and the following disclaimer in the documentation 
// and/or other materials provided with the distribution.
// 
// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" 
// AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE 
// IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE 
// ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE 
// LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR 
// CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF 
// SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS 
// INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN 
// CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) 
// ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF 
// THE POSSIBILITY OF SUCH DAMAGE.

package net.sf.sdedit.ui.impl;

import javax.swing.SwingUtilities;

import net.sf.sdedit.config.ConfigurationManager;
import net.sf.sdedit.ui.components.DelayedActivity;

/**
 * A RedrawThread is associated to a text area. It updates the diagram displayed
 * when text has been entered and a certain amount of time has passed since the
 * last key stroke.
 * 
 * @author Markus Strauch
 * 
 */
public class RedrawThread extends DelayedActivity {
	
	private UserInterfaceImpl ui;

	/**
	 * Creates a new RedrawThread.
	 * 
	 * @param ui
	 */
	public RedrawThread(UserInterfaceImpl ui) {
		this.ui = ui;
	}

	protected int getDelay() {
		return ConfigurationManager.getGlobalConfiguration().getAutodrawLatency() * 20;
	}
	
	private Runnable fire = new Runnable () {
		public void run() {
			ui.fireCodeChanged(!ConfigurationManager.getGlobalConfiguration()
					.isAutoUpdate());
			ui.enableComponents();
		}
	};

	protected void perform() {
		SwingUtilities.invokeLater(fire);
	}
}
