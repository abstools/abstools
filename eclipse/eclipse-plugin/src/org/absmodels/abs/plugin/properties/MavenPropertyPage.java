/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.absmodels.abs.plugin.properties;

import static org.absmodels.abs.plugin.util.Constants.MAVEN_IGNORE_TARGET_FOLDER;
import static org.absmodels.abs.plugin.util.UtilityFunctions.getAbsNature;
import static org.absmodels.abs.plugin.util.UtilityFunctions.syncPreferenceStore;

import org.eclipse.core.resources.IProject;
import org.eclipse.jface.preference.BooleanFieldEditor;
import org.eclipse.jface.preference.IPersistentPreferenceStore;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.ui.dialogs.PropertyPage;

/**
 * A Maven property page
 * @author pwong
 *
 */
public class MavenPropertyPage extends PropertyPage {

	private BooleanFieldEditor ignoreMavenTargerFolder;
	private IPersistentPreferenceStore prefstore;
	private IProject project;

	protected Control createContents(Composite parent) {
		project = (IProject) getElement();

		GridLayout gridLayout = new GridLayout(1, false);
		parent.setLayout(gridLayout);

		prefstore = getAbsNature(project).getProjectPreferenceStore();

		ignoreMavenTargerFolder = new BooleanFieldEditor(
				MAVEN_IGNORE_TARGET_FOLDER, "Ignore target folder for type checking",
				createContainer(parent));

		ignoreMavenTargerFolder.setPreferenceStore(prefstore);
		ignoreMavenTargerFolder.load();
		
		return parent;
	}

	private Composite createContainer(Composite parent) {
		Composite cont = new Composite(parent, SWT.NONE);
		GridData gridData = new GridData(GridData.FILL_HORIZONTAL);
		cont.setLayoutData(gridData);
		return cont;
	}
	
	@Override
	protected void performDefaults() {
		ignoreMavenTargerFolder.loadDefault();
		super.performDefaults();
	}
	
	@Override
	public boolean performOk() {
		ignoreMavenTargerFolder.store();
		syncPreferenceStore(prefstore);
		return super.performOk();
	}

}
