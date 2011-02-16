package eu.hatsproject.absplugin.actions.runconfig;

import static eu.hatsproject.absplugin.util.Constants.RUNCONFIG_MAUDE_EXECUTE;
import static eu.hatsproject.absplugin.util.Constants.RUNCONFIG_MAUDE_PARTIAL_EXEC;
import static eu.hatsproject.absplugin.util.Constants.RUNCONFIG_MAUDE_STEPS;
import static eu.hatsproject.absplugin.util.Constants.RUNCONFIG_PROJECT_NAME_ATTRIBUTE;
import static eu.hatsproject.absplugin.util.UtilityFunctions.standardExceptionHandling;
import static eu.hatsproject.absplugin.util.UtilityFunctions.showErrorMessage;

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

public class MaudeTab extends AbstractTab {

	Button exec;
	Button partialExec;
	Spinner steps;
	
	@Override
	public void createControl(Composite parent) {
		TabListener myListener = new TabListener(this);
		Composite comp = new Composite(parent, SWT.NONE);
		setCompositeLayout(comp, 1);
		
		createProjectDropDownMenu(myListener, comp);
		
		Group group = createGroup(comp, "Options", 1, 1, GridData.FILL_HORIZONTAL);
		createExecutionButton(group);
	    createPartialExecButton(group);
	    
		Composite stepContainer = new Composite(group, SWT.NONE);
		setCompositeLayout(stepContainer, 2);
		createStepSpinner(stepContainer);
	    
		setControl(comp);
	}

	@Override
	public void setDefaults(ILaunchConfigurationWorkingCopy configuration) {
		configuration.setAttribute(RUNCONFIG_PROJECT_NAME_ATTRIBUTE, getDefaultProjectName());
		configuration.setAttribute(RUNCONFIG_MAUDE_EXECUTE, true);
		configuration.setAttribute(RUNCONFIG_MAUDE_PARTIAL_EXEC, false);
		configuration.setAttribute(RUNCONFIG_MAUDE_STEPS, 0);
	}

	@Override
	public void initializeFrom(ILaunchConfiguration configuration) {
		try {
			initProject(configuration);
			exec.setSelection(configuration.getAttribute(RUNCONFIG_MAUDE_EXECUTE, true));
			
			if(!exec.getSelection()){
				partialExec.setEnabled(false);
			}
			
			partialExec.setSelection(configuration.getAttribute(RUNCONFIG_MAUDE_PARTIAL_EXEC, false));
			
			if(!partialExec.getSelection()){
				steps.setEnabled(false);
			}
			
			steps.setSelection(configuration.getAttribute(RUNCONFIG_MAUDE_STEPS, 0));
			
			scheduleUpdateJob();
		} catch (CoreException e) {
			showErrorMessage("Fatal Error while opening Maude Tab");
			standardExceptionHandling(e);
		}
	}

	@Override
	public void performApply(ILaunchConfigurationWorkingCopy configuration) {
		configuration.setAttribute(RUNCONFIG_PROJECT_NAME_ATTRIBUTE, getSelectedProjectName());
		configuration.setAttribute(RUNCONFIG_MAUDE_EXECUTE, true);
		configuration.setAttribute(RUNCONFIG_MAUDE_PARTIAL_EXEC, partialExec.getSelection());
		configuration.setAttribute(RUNCONFIG_MAUDE_STEPS, steps.getSelection());
	}

	@Override
	public String getName() {
		return "ABS Maude Backend";
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
