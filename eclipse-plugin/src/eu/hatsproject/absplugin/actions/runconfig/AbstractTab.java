/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package eu.hatsproject.absplugin.actions.runconfig;

import static eu.hatsproject.absplugin.util.Constants.RUNCONFIG_PROJECT_NAME_ATTRIBUTE;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.debug.core.ILaunchConfiguration;
import org.eclipse.debug.ui.AbstractLaunchConfigurationTab;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Listener;

import eu.hatsproject.absplugin.util.Constants;

public abstract class AbstractTab extends AbstractLaunchConfigurationTab {
	protected Label projectLabel;	
	protected Combo projectDropDown;

	/**
	 * A Listener updating the LaunchConfigurationDialog of the given tab
	 * when an event occurs.
	 */
	class TabListener implements Listener {
		AbstractTab tab;

		public TabListener(AbstractTab tab){
			this.tab=tab;
		}
		
		@Override
		public void handleEvent(Event event) {
			tab.updateLaunchConfigurationDialog();
		}
	}
	
	//---create methods------------------------
	
	protected void createProjectDropDownMenu(TabListener myListener,
			Composite comp) {
		Group group = createGroup(comp, "ABS Project", 1, 1, GridData.FILL_HORIZONTAL);
		
	    projectDropDown = new Combo(group, SWT.DROP_DOWN | SWT.BORDER | SWT.READ_ONLY);
	    GridData gridData = new GridData();
		gridData.widthHint = 200;
		projectDropDown.setLayoutData(gridData);
		
	    projectDropDown.addListener(SWT.Selection, myListener);
	}
	
	
	protected static Group createGroup(Composite parent, String text, int columns, int hspan, int fill) {
    	Group g = new Group(parent, SWT.NONE);
    	g.setLayout(new GridLayout(columns, false));
    	g.setText(text);
    	g.setFont(parent.getFont());
    	GridData gd = new GridData(fill);
		gd.horizontalSpan = hspan;
    	g.setLayoutData(gd);
    	return g;
	}


	//---set methods----------------------------------
	
	protected void initProject(ILaunchConfiguration configuration) throws CoreException {
		String name = configuration.getAttribute(RUNCONFIG_PROJECT_NAME_ATTRIBUTE, getDefaultProjectName());
		fillProjectDropDownMenue(name);
	}
	
	protected void fillProjectDropDownMenue(String name) {
		List<String> projectNames = getProjectNames();
		int selectedProjectName = 0;
		projectDropDown.removeAll();
		for (int i = 0; i < projectNames.size() ; i++) {
			String projectName = projectNames.get(i);
			if(projectName.equals(name)){
				selectedProjectName = i;
			}
			projectDropDown.add(projectName);
		}
		projectDropDown.select(selectedProjectName);
	}
	
	
	//---get and help functions----------------------------------
	
	protected String getDefaultProjectName() {
		List<String> names = getProjectNames();
		if (names!=null){
			return names.get(0);
		} else {
			return (String)null;
		}
	}

	protected ArrayList<String> getProjectNames() {
		IProject[] projects = ResourcesPlugin.getWorkspace().getRoot().getProjects();
		ArrayList<String> projectNames = new ArrayList<String>();
		for (int i = 0; i < projects.length; i++) {
			try {
				if(projects[i].isAccessible() && projects[i].hasNature(Constants.NATURE_ID)){
					projectNames.add(projects[i].getName());
				}
			} catch (CoreException e) {
				//just skip the project, if an error occurs (see hasNature for error description)
			}
		}
		return projectNames;
	}
	
	protected String getSelectedProjectName() {
		return projectDropDown.getItem(projectDropDown.getSelectionIndex());
	}	

}
