/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.absmodels.abs.plugin.wizards.pages;

import org.absmodels.abs.plugin.util.InternalASTNode;
import org.absmodels.abs.plugin.wizards.WizardUtil;
import org.absmodels.abs.plugin.wizards.WizardUtil.ErrorType;
import org.absmodels.abs.plugin.wizards.composites.ModuleGroup;
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

import abs.frontend.ast.ModuleDecl;

public class NewInterfaceInFileWizardPage extends WizardPage implements IABSClassInterfaceWizardPage{

	private Text classNameField;
	private ModuleGroup moduleGroup;
	private String initialValue = "";
	private InternalASTNode<ModuleDecl> initialDecl = null;
	
	private static String MODULE_GROUP_MESSAGE = "Select a module declaration";

	private static int DEFAULT_TEXT_FIELD_WIDTH = 250;

	private final Listener changeListener = new Listener() {

		@Override
		public void handleEvent(Event event) {
			boolean valid = validate();
			setPageComplete(valid);

		}
	};
		
	public NewInterfaceInFileWizardPage(String pageName) {
		super(pageName);
		setPageComplete(false);
	}

	@Override
	public void setInitialName(String value) {
		initialValue = value;
	}

	@Override
	public void createControl(Composite parent) {
		Composite composite = new Composite(parent, SWT.NULL);

		initializeDialogUnits(parent);

		composite.setLayout(new GridLayout());
		composite.setLayoutData(new GridData(GridData.FILL_BOTH));

		createClassNameField(composite);

		moduleGroup = new ModuleGroup(composite, changeListener, MODULE_GROUP_MESSAGE);
		if (initialDecl == null) {
			moduleGroup.setSelectedResource(null);
		} else {
			moduleGroup.setSelectedResource(initialDecl);
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
        classNameLabel.setText("Interface Name:");
        classNameLabel.setFont(classNameDialog.getFont());
        
        classNameField = new Text(classNameDialog, SWT.BORDER);
        GridData gridData = new GridData(GridData.FILL_HORIZONTAL);
        gridData.widthHint = DEFAULT_TEXT_FIELD_WIDTH;
        classNameField.setLayoutData(gridData);
        classNameField.setFont(classNameDialog.getFont());
        
        if (!"".equals(initialValue)){
        	classNameField.setText(initialValue);
        }
        
        classNameField.addListener(SWT.Modify, changeListener);
	}
	
	@Override
	public String getNewName(){
		return classNameField != null ? classNameField.getText() : null;
	}
	
	@Override
	public InternalASTNode<ModuleDecl> getResultModule(){
		InternalASTNode<ModuleDecl> result = moduleGroup.getSelectedModuleDecl();
		return result;
		
	}
	
	@Override
	public void setInitialModule(InternalASTNode<ModuleDecl> decl){
		if (decl != null) {
			this.initialDecl = decl;
		}
	}
	
	private boolean validate() {
		ErrorType errorMessage = WizardUtil.validate(classNameField.getText());
		
		if (errorMessage == ErrorType.NO_ERROR){
			errorMessage = validateModule();
		}
		setErrorMessage(WizardUtil.getErrorDescription(errorMessage));
		return (errorMessage == ErrorType.NO_ERROR);
	}
	
	private ErrorType validateModule(){
		InternalASTNode<ModuleDecl> result = moduleGroup.getSelectedModuleDecl();
		if (result != null) {
			return ErrorType.NO_ERROR;
		}
		
		return ErrorType.ERROR_NO_VALID_MODULE;
	}
}
