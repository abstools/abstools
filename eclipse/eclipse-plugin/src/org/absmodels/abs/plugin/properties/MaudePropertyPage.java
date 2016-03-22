/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.absmodels.abs.plugin.properties;

import static org.absmodels.abs.plugin.util.Constants.MAUDE_PATH;
import static org.absmodels.abs.plugin.util.UtilityFunctions.getAbsNature;
import static org.absmodels.abs.plugin.util.UtilityFunctions.syncPreferenceStore;

import org.eclipse.core.resources.IProject;
import org.eclipse.jface.preference.DirectoryFieldEditor;
import org.eclipse.jface.preference.IPersistentPreferenceStore;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.ui.dialogs.PropertyPage;

/**
 * Property page for project specific ABS properties concerning Maude execution.
 * @author tfischer
 */
public class MaudePropertyPage extends PropertyPage {

	private DirectoryFieldEditor maudeSourcePath;
	private IProject project;
	private IPersistentPreferenceStore prefstore = null;
	
	@Override
	protected Control createContents(Composite parent) {
		project = (IProject)getElement();
		
		GridLayout gridLayout = new GridLayout(1, false);
		parent.setLayout(gridLayout);
		
		Composite maudeSourcePathContainer = new Composite(parent, SWT.NONE);
		GridData gridData = new GridData(GridData.FILL_HORIZONTAL);
		maudeSourcePathContainer.setLayoutData(gridData);
		
		prefstore = getAbsNature(project).getProjectPreferenceStore();

		maudeSourcePath = new DirectoryFieldEditor(MAUDE_PATH, "Maude Source Path", maudeSourcePathContainer);
		maudeSourcePath.setPreferenceStore(prefstore);
		maudeSourcePath.load();
		
		return parent;
	}
	
	@Override
	protected void performDefaults() {
		maudeSourcePath.loadDefault();		
		super.performDefaults();
	}
	
	@Override
	public boolean performOk() {
		maudeSourcePath.store();
		syncPreferenceStore(prefstore);
		return super.performOk();
	}
}
