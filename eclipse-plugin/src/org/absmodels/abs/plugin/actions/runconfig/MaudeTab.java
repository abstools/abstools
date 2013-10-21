/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.absmodels.abs.plugin.actions.runconfig;

import static org.absmodels.abs.plugin.util.Constants.*;
import static org.absmodels.abs.plugin.util.UtilityFunctions.showErrorMessage;
import static org.absmodels.abs.plugin.util.UtilityFunctions.standardExceptionHandling;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.debug.core.ILaunchConfiguration;
import org.eclipse.debug.core.ILaunchConfigurationWorkingCopy;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Spinner;
import org.eclipse.swt.widgets.Text;

import abs.backend.tests.ABSTestRunnerGenerator;

public class MaudeTab extends AbstractTab {

	Button testExecution;
	Button exec;
	Button partialExec;
	Spinner steps;
	Button realtime;
	Text mainBlock;
	
	@Override
	public void createControl(Composite parent) {
		TabListener myListener = new TabListener(this);
		Composite comp = new Composite(parent, SWT.NONE);
		setCompositeLayout(comp, 1);
		
		createProjectDropDownMenu(myListener, comp);
		createProductDropDownMenu(myListener, comp);
		
		Group group = createGroup(comp, "Options", 1, 1, GridData.FILL_HORIZONTAL);
		createTestRunnerGenerationButton(group);
		createExecutionButton(group);
	    createPartialExecButton(group);
	    createRealtimeButton(group);
	    createMainBlockChooser(group);
	    
		Composite stepContainer = new Composite(group, SWT.NONE);
		setCompositeLayout(stepContainer, 2);
		createStepSpinner(stepContainer);
	    
		setControl(comp);
	}

	@Override
	public void setDefaults(ILaunchConfigurationWorkingCopy configuration) {
		configuration.setAttribute(RUNCONFIG_PROJECT_NAME_ATTRIBUTE, getDefaultProjectName());
		configuration.setAttribute(RUNCONFIG_PRODUCT_NAME_ATTRIBUTE, (String)null);
		configuration.setAttribute(RUNCONFIG_TEST_EXECUTION, false);
		configuration.setAttribute(RUNCONFIG_MAUDE_EXECUTE, true);
		configuration.setAttribute(RUNCONFIG_MAUDE_PARTIAL_EXEC, false);
		configuration.setAttribute(RUNCONFIG_MAUDE_STEPS, 0);
		configuration.setAttribute(RUNCONFIG_MAUDE_REALTIME, false);
		configuration.setAttribute(RUNCONFIG_MAUDE_MAINBLOCK, "");
	}

	@Override
	public void initializeFrom(ILaunchConfiguration configuration) {
		try {
			initProject(configuration);
			testExecution.setSelection(configuration.getAttribute(RUNCONFIG_TEST_EXECUTION, false));
			exec.setSelection(configuration.getAttribute(RUNCONFIG_MAUDE_EXECUTE, true));
			
			if(!exec.getSelection()){
				partialExec.setEnabled(false);
			}
			
			partialExec.setSelection(configuration.getAttribute(RUNCONFIG_MAUDE_PARTIAL_EXEC, false));
			
			if(!partialExec.getSelection()){
				steps.setEnabled(false);
			}
			
			realtime.setSelection(configuration.getAttribute(RUNCONFIG_MAUDE_REALTIME, false));
			steps.setSelection(configuration.getAttribute(RUNCONFIG_MAUDE_STEPS, 0));
			mainBlock.setText(configuration.getAttribute(RUNCONFIG_MAUDE_MAINBLOCK, ""));
			
			scheduleUpdateJob();
		} catch (CoreException e) {
			showErrorMessage("Fatal Error while opening Maude Tab");
			standardExceptionHandling(e);
		}
	}

	@Override
	public void performApply(ILaunchConfigurationWorkingCopy configuration) {
		configuration.setAttribute(RUNCONFIG_PROJECT_NAME_ATTRIBUTE, getSelectedProjectName());
		configuration.setAttribute(RUNCONFIG_PRODUCT_NAME_ATTRIBUTE, getSelectedProductName());
		configuration.setAttribute(RUNCONFIG_TEST_EXECUTION, testExecution.getSelection());
		configuration.setAttribute(RUNCONFIG_MAUDE_EXECUTE, exec.getSelection());
		configuration.setAttribute(RUNCONFIG_MAUDE_PARTIAL_EXEC, partialExec.getSelection());
		configuration.setAttribute(RUNCONFIG_MAUDE_STEPS, steps.getSelection());
		configuration.setAttribute(RUNCONFIG_MAUDE_REALTIME, realtime.getSelection());
		configuration.setAttribute(RUNCONFIG_MAUDE_MAINBLOCK, mainBlock.getText());
	}

	@Override
	public String getName() {
		return "ABS Maude Backend";
	}
	
	private void createTestRunnerGenerationButton(Composite comp) {
		testExecution = new Button(comp, SWT.CHECK);
		testExecution.setText("Execute ABSUnit tests");
		testExecution.addSelectionListener(new SelectionListener() {
	    	
	    	@Override
			public void widgetSelected(SelectionEvent event) {
	    		updateLaunchConfigurationDialog();
	    		if(testExecution.getSelection()){
					mainBlock.setText(ABSTestRunnerGenerator.RUNNER_MAIN);
					mainBlock.setEditable(false);
				} else {
					mainBlock.setText("");
					mainBlock.setEditable(true);
				}
			}
			
			@Override
			public void widgetDefaultSelected(SelectionEvent event) {
				widgetSelected(event);
			}
	    });
	}
	
	private void createExecutionButton(Composite comp) {
		exec = new Button(comp, SWT.CHECK);
	    exec.setText("Execute generated .maude file");
	    exec.addSelectionListener(new SelectionListener() {
	    	
	    	@Override
			public void widgetSelected(SelectionEvent event) {
	    		updateLaunchConfigurationDialog();
	    		if(exec.getSelection()){
					partialExec.setEnabled(true);
					steps.setEnabled(partialExec.getSelection());
				} else{
					partialExec.setEnabled(false);
					steps.setEnabled(false);
				}
			}
			
			@Override
			public void widgetDefaultSelected(SelectionEvent event) {
				widgetSelected(event);
			}
	    });
	}

	private void createPartialExecButton(Composite comp) {
		partialExec = new Button(comp, SWT.CHECK);
	    partialExec.setText("Partial Run");
	    partialExec.addSelectionListener(new SelectionListener() {
			
			@Override
			public void widgetSelected(SelectionEvent event) {
				updateLaunchConfigurationDialog();
				if(exec.getSelection() && partialExec.getSelection()){
					steps.setEnabled(true);
				} else{
					steps.setEnabled(false);
				}
			}
			
			@Override
			public void widgetDefaultSelected(SelectionEvent event) {
				widgetSelected(event);
			}
		});
	}

	private void createRealtimeButton(Composite comp) {
		realtime = new Button(comp, SWT.CHECK);
		realtime.setText("Realtime");
		realtime.addSelectionListener(new SelectionListener() {

			@Override
			public void widgetSelected(SelectionEvent e) {
				updateLaunchConfigurationDialog();				
			}

			@Override
			public void widgetDefaultSelected(SelectionEvent e) {
				widgetSelected(e);
			}
		});
	}

	private void createMainBlockChooser(Composite comp) {
		Label label = new Label(comp, SWT.NULL);
		label.setText("Main block: ");
		mainBlock = new Text(comp, SWT.SINGLE);
		mainBlock.addModifyListener(new ModifyListener() {
			@Override
			public void modifyText(ModifyEvent e) {
				updateLaunchConfigurationDialog();
			}
		});
	}

	private void createStepSpinner(Composite comp) {
		projectLabel = new Label(comp, SWT.NULL);
	    projectLabel.setText("Number of steps: ");
	    
	    steps = new Spinner(comp, SWT.NONE);
	    steps.setMinimum(0);
		steps.setMaximum(Integer.MAX_VALUE);
		steps.setIncrement(1);
		steps.setToolTipText("Number of steps in Maude execution");

		steps.addModifyListener(new ModifyListener() {
			@Override
			public void modifyText(ModifyEvent e) {
				updateLaunchConfigurationDialog();
			}
		});
	}
	
	private void setCompositeLayout(Composite comp, int columns) {
		GridLayout gridLayout = new GridLayout(columns, false);
	    gridLayout.verticalSpacing = 6;
	    comp.setLayout(gridLayout);
	}
}
