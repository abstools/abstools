/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.absmodels.abs.plugin.preferences;

import org.eclipse.jface.preference.FieldEditorPreferencePage;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPreferencePage;

import static org.absmodels.abs.plugin.util.UtilityFunctions.*;

/**
 * Preference page for global ABS preferences. This page contains no entries.
 * @author tfischer
 */
public class ABSPreferencePage extends FieldEditorPreferencePage implements IWorkbenchPreferencePage{
	
	@Override
	public void init(IWorkbench workbench) {
		setDescription("Please select a subentry to modify global preferences");
		setPreferenceStore(getDefaultPreferenceStore());
		noDefaultAndApplyButton();
	}

	@Override
	protected void createFieldEditors() {
		//reserved for future use
	}
}
