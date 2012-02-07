/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package eu.hatsproject.absplugin.actions.runconfig;

import static eu.hatsproject.absplugin.util.Constants.*;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IncrementalProjectBuilder;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.debug.core.ILaunchConfiguration;
import org.eclipse.debug.ui.AbstractLaunchConfigurationTab;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.*;

import abs.common.WrongProgramArgumentException;
import abs.frontend.analyser.SemanticErrorList;
import abs.frontend.ast.Model;
import abs.frontend.ast.Product;
import abs.frontend.delta.exceptions.ASTNodeNotFoundException;

import eu.hatsproject.absplugin.builder.AbsNature;
import eu.hatsproject.absplugin.exceptions.AbsJobException;
import eu.hatsproject.absplugin.exceptions.TypeCheckerException;
import eu.hatsproject.absplugin.util.Constants;
import eu.hatsproject.absplugin.util.UtilityFunctions;

public abstract class AbstractTab extends AbstractLaunchConfigurationTab {
	protected Label projectLabel;	
	protected Combo projectDropDown;
	protected Combo productDropDown;

	/**
	 * A Listener updating the LaunchConfigurationDialog of the given tab
	 * when an event occurs.
	 */
	static class TabListener implements Listener {
		final private AbstractTab tab;

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
		/* Refresh products everytime the project is changed */
		projectDropDown.addListener(SWT.Selection, new Listener() {
			@Override
			public void handleEvent(Event event) {
				fillProductDropDownMenue(null);
				updateErrors();
			}
		});
	}

	/**
	 * Don't recommend to start projects which have errors.
	 * TODO: manipulating the error message here does not cooperate well with isValid().
	 */
	protected boolean updateErrors() {
	        boolean res = true;
		String projectName = getSelectedProjectName();
		if (projectName != null) {
			IProject project = ResourcesPlugin.getWorkspace().getRoot().getProject(projectName);
			try {
				AbsNature nat = UtilityFunctions.getAbsNature(project);
				assert nat != null;
				synchronized (nat.modelLock) {
					Model model = nat.getCompleteModel();
					/* E.g. errors in the project */
					if (model == null)
						return false;
					/* Check product if any */
					String prod = getSelectedProductName();
					if (prod != null) {
						model.flattenForProduct(prod);
						/* Type check again */
						model.flushCache(); // #335, see IncrementalModelBuilder#flushAll()
						SemanticErrorList errs = model.typeCheck();
						// #332: trigger fresh build
						project.build(IncrementalProjectBuilder.FULL_BUILD, null);
						if (errs != null && !errs.isEmpty())
							throw new AbsJobException(new TypeCheckerException(errs));
					}
				}
				setErrorMessage(null);
			} catch (AbsJobException e) {
				setErrorMessage(e.getMessage());
				res = false;
			} catch (WrongProgramArgumentException e) {
				setErrorMessage(e.getMessage());
				res = false;
			} catch (ASTNodeNotFoundException e) {
				setErrorMessage(e.getMessage());
				res = false;
			} catch (CoreException e) {
				setErrorMessage(e.getMessage());
				res = false;
			}
			getLaunchConfigurationDialog().updateMessage();
		}
		return res;
	}

	@Override
	public boolean isValid(ILaunchConfiguration launchConfig) {
	    boolean res = super.isValid(launchConfig);
	    res &= updateErrors();
	    return res;
	}

	/**
	 * Must be called after createProjectDropDownMenu, since we need to figure out the selected project.
	 * @author stolz
	 */
	protected void createProductDropDownMenu(TabListener myListener,
			Composite comp) {
		Group group = createGroup(comp, "ABS Product", 1, 1, GridData.FILL_HORIZONTAL);

		productDropDown = new Combo(group, SWT.DROP_DOWN | SWT.BORDER | SWT.READ_ONLY);
		GridData gridData = new GridData();
		gridData.widthHint = 200;
		productDropDown.setLayoutData(gridData);

		productDropDown.addListener(SWT.Selection, myListener);
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
		name = configuration.getAttribute(RUNCONFIG_PRODUCT_NAME_ATTRIBUTE, (String)null);
		fillProductDropDownMenue(name);
		updateErrors();
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

	/**
	 * Lists all products in the current project's module, incl. a "<base>" product if
	 * no particular product is selected.
	 * @param preSelected The product that should be preselected, or null for <base>.
	 */
	protected void fillProductDropDownMenue(String preSelected) {
		productDropDown.removeAll();
		final int idx = projectDropDown.getSelectionIndex();
		if (idx == -1)
			return;
		String projN = getProjectNames().get(idx);
		/* We use this entry at index 0 to indicate that no particular product is selected */
		productDropDown.add("<base>");
		IProject proj = ResourcesPlugin.getWorkspace().getRoot().getProject(projN);
		assert proj != null;		
		AbsNature n = UtilityFunctions.getAbsNature(proj);
		Model m = n.getCompleteModel();
		if (m == null)
			return;
		Collection<Product> prods = m.getProducts();
		if (prods == null)
			return;
		int i = 1; /* base comes first */
		int selected = 0;
		for (Product p : prods) {
			final String name = p.qualifiedName();
			productDropDown.add(name);
			if (name.equals(preSelected)) {
				selected = i;
			}
			i++;
		}
		productDropDown.select(selected);
	}

	//---get and help functions----------------------------------

	protected String getDefaultProjectName() {
		List<String> names = getProjectNames();
		if (names!=null && !names.isEmpty()){
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

	/**
	 * @return the product name, or null if the base product is selected.
	 */
	protected String getSelectedProductName() {
		final int index = productDropDown.getSelectionIndex();
		if (index <= 0) /* base is at index 0 */
			return null;
		return productDropDown.getItem(index);
	}	

}
