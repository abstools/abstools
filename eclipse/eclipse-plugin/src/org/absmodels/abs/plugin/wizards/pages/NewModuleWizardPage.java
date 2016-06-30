/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.absmodels.abs.plugin.wizards.pages;

import org.absmodels.abs.plugin.util.Constants;
import org.absmodels.abs.plugin.wizards.WizardUtil;
import org.absmodels.abs.plugin.wizards.WizardUtil.ErrorType;
import org.absmodels.abs.plugin.wizards.composites.ResourceGroup;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Listener;
import org.eclipse.swt.widgets.Text;


public class NewModuleWizardPage extends WizardPage{

	private Text classNameField;
	private ResourceGroup resourceGroup;
	private String   initialValue = "";
	private IResource initialFile = null;
	private IProject proj;
	
	private static int     DEFAULT_TEXT_FIELD_WIDTH = 250;

	private final Listener changeListener = new Listener() {
		
		@Override
		public void handleEvent(Event event) {
			boolean valid = validate();
			setPageComplete(valid);
			
		}
	};	
	
	public NewModuleWizardPage(String pageName,IProject proj) {
		super(pageName);
		setPageComplete(false);
		this.proj = proj;
	}
	
	public void setInitialValue(String value){
		initialValue = value;
	}

	@Override
	public void createControl(Composite parent) {
		Composite composite = new Composite(parent, SWT.NULL);
		
		initializeDialogUnits(parent);
		
        composite.setLayout(new GridLayout());
        composite.setLayoutData(new GridData(GridData.FILL_BOTH));
        
        createClassNameField(composite);
        
		resourceGroup = new ResourceGroup(composite, changeListener);
		if (initialFile == null){
		    if (proj != null) {
		        resourceGroup.setSelectedResource(proj);
		    }
		}else{
			resourceGroup.setSelectedResource(initialFile);
		}
                
        setPageComplete(validate());
		
		setControl(composite);
		Dialog.applyDialogFont(composite);
		

	}
	
	private void createClassNameField(Composite parent) {
		Composite classNameDialog = new Composite(parent, SWT.NONE);
        GridLayout layout = new GridLayout();
        layout.numColumns = 2;
        classNameDialog.setLayout(layout);
        classNameDialog.setLayoutData(new GridData(GridData.FILL_HORIZONTAL));
        
        Label classNameLabel = new Label(classNameDialog,SWT.NONE);
        classNameLabel.setText("Module Name:");
        classNameLabel.setFont(classNameDialog.getFont());
        
        classNameField = new Text(classNameDialog, SWT.NONE);
        GridData gridData = new GridData(GridData.FILL_HORIZONTAL);
        gridData.widthHint = DEFAULT_TEXT_FIELD_WIDTH;
        classNameField.setLayoutData(gridData);
        classNameField.setFont(classNameDialog.getFont());
        
        if (!"".equals(initialValue)){
        	classNameField.setText(initialValue);
        }
        
        classNameField.addListener(SWT.Modify, changeListener);
	}
	
	public String getResult(){
		return classNameField != null ? classNameField.getText() : null;
	}
	
	public IFile getResultFile(){
		IFile result = resourceGroup.getSelectedFile();
		return result;
		
	}
	
	public void setInitialFileResource(IResource resource){
		if (resource != null) {
			this.initialFile = resource;
		}
	}
	
	private boolean validate() {
		ErrorType errorMessage = WizardUtil.validateModule(classNameField.getText());
		if (errorMessage == ErrorType.NO_ERROR) {
			errorMessage = validateFile();
		}

		setErrorMessage(WizardUtil.getErrorDescription(errorMessage));
		return (errorMessage == ErrorType.NO_ERROR);
	}
	
	private ErrorType validateFile(){
		IFile result = resourceGroup.getSelectedFile();
		if (result != null) {
			if (result.getName().endsWith("." + Constants.ABS_FILE_EXTENSION)){
				return ErrorType.NO_ERROR;
			}
		}
		
		return ErrorType.ERROR_NO_VALID_FILE;
	}
}
