/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.absmodels.abs.plugin.properties;

import static org.absmodels.abs.plugin.util.Constants.ALWAYS_COMPILE;
import static org.absmodels.abs.plugin.util.Constants.JAVA_SOURCE_PATH;
import static org.absmodels.abs.plugin.util.Constants.NO_WARNINGS;
import static org.absmodels.abs.plugin.util.Constants.SOURCE_ONLY;
import static org.absmodels.abs.plugin.util.UtilityFunctions.getAbsNature;
import static org.absmodels.abs.plugin.util.UtilityFunctions.syncPreferenceStore;

import org.eclipse.core.resources.IProject;
import org.eclipse.jface.preference.BooleanFieldEditor;
import org.eclipse.jface.preference.DirectoryFieldEditor;
import org.eclipse.jface.preference.IPersistentPreferenceStore;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.ui.dialogs.PropertyPage;

/**
 * Property page for project specific ABS properties concerning Java execution and debugging.
 * @author tfischer
 */
public class JavaPropertyPage extends PropertyPage {
	
	//(check) buttons
	private BooleanFieldEditor sourceOnly;
	private BooleanFieldEditor noWarnings;
	private BooleanFieldEditor alwaysCompile;
	private DirectoryFieldEditor javaSourcePath;
	private IPersistentPreferenceStore prefstore = null;

	private IProject project;

	
	@Override
	protected Control createContents(Composite parent) {
		project = (IProject)getElement();
		
		GridLayout gridLayout = new GridLayout(1, false);
		parent.setLayout(gridLayout);
		
		Composite javaSourcePathContainer = new Composite(parent, SWT.NONE);
		GridData gridData = new GridData(GridData.FILL_HORIZONTAL);
		javaSourcePathContainer.setLayoutData(gridData);
		
		Composite sourceOnlyContainer    = new Composite(parent, SWT.NONE);
		sourceOnlyContainer.setLayoutData(gridData);
		
		Composite alwaysCompileContainer = new Composite(parent, SWT.NONE);
		alwaysCompileContainer.setLayoutData(gridData);
		
		Composite noWarningsContainer    = new Composite(parent, SWT.NONE);
		noWarningsContainer.setLayoutData(gridData);
		
		javaSourcePath = new DirectoryFieldEditor(JAVA_SOURCE_PATH, "Java source path", javaSourcePathContainer);
		sourceOnly     = new BooleanFieldEditor(SOURCE_ONLY, " Create Java source code only", sourceOnlyContainer);
		alwaysCompile  = new BooleanFieldEditor(ALWAYS_COMPILE, " Always compile before debugging", alwaysCompileContainer);
		noWarnings     = new BooleanFieldEditor(NO_WARNINGS, " Supress Java compilation warnings", noWarningsContainer);
		
		setPreferenceStore();	
		loadValues();
		
		return parent;
	}

	private void setPreferenceStore(){
		prefstore = getAbsNature(project).getProjectPreferenceStore();
		sourceOnly.setPreferenceStore(prefstore);
		alwaysCompile.setPreferenceStore(prefstore);
		noWarnings.setPreferenceStore(prefstore);
		javaSourcePath.setPreferenceStore(prefstore);
	}
	
	private void loadValues() {
		sourceOnly.load();
		noWarnings.load();
		alwaysCompile.load();
		javaSourcePath.load();
	}
	
	@Override
	protected void performDefaults() {
		sourceOnly.loadDefault();
		noWarnings.loadDefault();
		alwaysCompile.loadDefault();
		javaSourcePath.loadDefault();
		
		super.performDefaults();
	}
	
	@Override
	public boolean performOk() {
		saveValues();
		return super.performOk();
	}

	private void saveValues(){
		sourceOnly.store();
		noWarnings.store();
		alwaysCompile.store();
		javaSourcePath.store();
		syncPreferenceStore(prefstore);
	}
}
