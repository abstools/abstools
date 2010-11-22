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

package net.sf.sdedit.ui.components.configuration.configurators;

import java.awt.BorderLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.beans.PropertyDescriptor;
import java.util.Set;

import javax.swing.JComboBox;

import net.sf.sdedit.ui.components.configuration.Bean;
import net.sf.sdedit.ui.components.configuration.DataObject;

public class StringSelectionConfigurator<C extends DataObject> extends StringConfigurator<C> implements
		ActionListener {

	private JComboBox comboBox;

	public StringSelectionConfigurator(Bean<C> bean,
			PropertyDescriptor property) {
		super(bean, property);
		initialize();
	}

	private void initialize() {
		Set<String> choices = getBean().getStringsForProperty(getProperty());
		comboBox = new JComboBox();
		comboBox.setEditable(false);
		for (String choice : choices) {
			comboBox.addItem(choice);
		}
		comboBox.setSelectedItem(getValue());
		comboBox.addActionListener(this);
		getBottomPanel().setLayout(new BorderLayout());
		getBottomPanel().add(comboBox, BorderLayout.CENTER);
	}

	@Override
	protected void refresh(String value) {
		comboBox.setSelectedItem(value);
	}

	protected void _actionPerformed(ActionEvent e) {
		String value = (String) comboBox.getSelectedItem();
		applyValue(value);
	}

	protected void _setEnabled(boolean enabled) {
		comboBox.setEnabled(enabled);
	}
}
