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

package net.sf.sdedit.ui.components.buttons;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

import javax.swing.AbstractButton;
import javax.swing.JComponent;
import javax.swing.Timer;

import net.sf.sdedit.ui.components.configuration.ConfigurationAction;

public final class EnableComponents implements ActionListener {
	
	private Map<JComponent, Activator> activatorMap;
	
	private List<ConfigurationAction<?>> configurationActions;
	
	private Timer timer;
	
	private boolean timeToEnable;
	
	public EnableComponents () {
		activatorMap = new HashMap<JComponent, Activator>();
		configurationActions = new LinkedList<ConfigurationAction<?>>();
		timer = new Timer(250, this);
		timeToEnable = false;
		timer.start();
	}
	
	private void _activateComponents () {
		for (Map.Entry<JComponent,Activator> entry : activatorMap.entrySet()) {
			entry.getKey().setEnabled(entry.getValue().isEnabled());
		}
		for (ConfigurationAction<?> action : configurationActions) {
			action.update();
		}
	}
	
	public synchronized void enableComponents () {
		timeToEnable = true;
		timer.restart();
	}
	
	public synchronized void actionPerformed (ActionEvent e) {
		if (timeToEnable) {
			_activateComponents();
			timeToEnable = false;
		}
	}
	
	public void registerButton (JComponent comp, Activator activator) {
		activatorMap.put(comp,activator);
	}
	
	public void registerConfigurationAction (ConfigurationAction<?> action,
			AbstractButton button) {
		action.setButton(button);
		configurationActions.add(action);
	}
}
