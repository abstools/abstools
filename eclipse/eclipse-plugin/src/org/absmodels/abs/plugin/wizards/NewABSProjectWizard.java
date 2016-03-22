/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.absmodels.abs.plugin.wizards;

import static org.absmodels.abs.plugin.util.Constants.ABSPERSPECTIVE_ID;
import static org.absmodels.abs.plugin.util.Constants.NATURE_ID;

import java.io.File;

import org.absmodels.abs.plugin.util.UtilityFunctions;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IProjectDescription;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.wizard.Wizard;
import org.eclipse.ui.INewWizard;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.dialogs.WizardNewProjectCreationPage;


/**
 * Wizard for creating a new ABS project
 * @author cseise, mweber
 *
 */
public class NewABSProjectWizard extends Wizard implements INewWizard {

	private IWorkbench workbench;
	private WizardNewProjectCreationPage mainPage;

	public NewABSProjectWizard() {
	}
	
	private final String WIZARD_TITLE = "New ABS project"; 
	private final String WIZARD_DESCRIPTION = "Create a new ABS project";
	
	private final String OVERWRITE_TITLE = "Overwriting project";
	private final String OVERWRITE_TEXT  = "A .project file exist in the given location" +
			" (maybe an existing project). Should the project be overwritten?";

	@Override
	public void init(IWorkbench workbench, IStructuredSelection selection) {
		this.workbench = workbench;
		setWindowTitle("New ABS project");
	}
	
	@Override
	public void addPages() {
		mainPage = new WizardNewProjectCreationPage(WIZARD_TITLE);
		mainPage.setDescription(WIZARD_DESCRIPTION);
		mainPage.setTitle(WIZARD_TITLE);
	    addPage(mainPage);
	}

	@Override
	public boolean performFinish() {
		try{
			IProject project = mainPage.getProjectHandle();
			IProjectDescription desc;
			File projectFile;
			if(!mainPage.useDefaults()){
				projectFile = new File(mainPage.getLocationPath().toFile(), ".project");
				
				desc = ResourcesPlugin.getWorkspace().newProjectDescription(mainPage.getProjectName());
				desc.setLocation(mainPage.getLocationPath());
			} else {
				File projectDir = new File(mainPage.getLocationPath().toFile(), mainPage.getProjectName());
				projectFile = new File(projectDir, ".project");
				desc = ResourcesPlugin.getWorkspace().newProjectDescription(mainPage.getProjectName());
			}
			if(projectFile.exists()){
				boolean overwrite = MessageDialog.
					openQuestion(getShell(), OVERWRITE_TITLE,OVERWRITE_TEXT);
				if(overwrite)
					projectFile.delete();
				else
					return false;
			}
			project.create(desc, null);
			project.open(null);
			desc.setNatureIds(new String[]{NATURE_ID}); //, JavaCore.NATURE_ID});
			project.setDescription(desc, null);
			
			workbench.showPerspective(ABSPERSPECTIVE_ID, workbench.getActiveWorkbenchWindow());
			return true;
		}catch(CoreException ex){
			ex.printStackTrace();
			UtilityFunctions.showErrorMessage("Could not create new Project: \n"+ex.getLocalizedMessage());
		}
		return false;
	}

}
