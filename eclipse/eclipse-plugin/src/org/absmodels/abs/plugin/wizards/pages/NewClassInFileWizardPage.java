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
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Listener;
import org.eclipse.swt.widgets.Text;

import abs.frontend.ast.ModuleDecl;

public class NewClassInFileWizardPage extends WizardPage implements IABSClassInterfaceWizardPage{

	private Text classNameField;
	private ModuleGroup moduleGroup;
	private Combo classTypeField;
	private String initialValue = "";
	private InternalASTNode<ModuleDecl> initialDecl = null;

	private static int DEFAULT_TEXT_FIELD_WIDTH = 250;

	private final Listener changeListener = new Listener() {

		@Override
		public void handleEvent(Event event) {
			boolean valid = validate();
			setPageComplete(valid);

		}
	};
	
	public static enum ClassType {
		TYPE_UNDEFINED("Undefined",""),
		TYPE_COG("COG","[COG]"),
		TYPE_PLAIN("Plain","[Plain]");
		
		private final String name;
		private final String annotation;
		
		ClassType(String name, String annotation){
			this.name = name;
			this.annotation = annotation;
		}
		
		@Override
		public String toString(){
			return name;
		}
		
		public String getAnnotationString(){
			return annotation;
		}
		
		public int getAnnotationLength(){
			return annotation.length();
		}
	}
	
	public NewClassInFileWizardPage(String pageName) {
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

		//Combo to choose the class type
		createClassTypeField(composite);

		moduleGroup = new ModuleGroup(composite, changeListener, "Select a module declaration:");
		if (initialDecl == null) {
			moduleGroup.setSelectedResource(null);
		} else {
			moduleGroup.setSelectedResource(initialDecl);
		}
		setPageComplete(validate());

		setControl(composite);
		Dialog.applyDialogFont(composite);

	}
	
	private void createClassTypeField(Composite composite) {
		Composite classType = new Composite(composite, SWT.NULL);
        GridLayout layout = new GridLayout();
        layout.numColumns = 2;
        classType.setLayout(layout);
        classType.setLayoutData(new GridData(GridData.FILL_HORIZONTAL));
        
        Label classTypeLabel = new Label(classType,SWT.NONE);
        classTypeLabel.setText("Class Type:");
        classTypeLabel.setFont(classType.getFont());
        classTypeField = new Combo(classType, SWT.DROP_DOWN | SWT.READ_ONLY);
        
        for (ClassType type : ClassType.values()){
        	classTypeField.add(type.toString());
        }
        classTypeField.select(0);
	}
	
	public ClassType getResultType(){
		String selection = classTypeField.getItem(classTypeField.getSelectionIndex());
		for (ClassType type : ClassType.values()){
			if (type.toString().equals(selection)){
				return type;
			}
		}
		
		return ClassType.TYPE_UNDEFINED;
	}

	private void createClassNameField(Composite parent) {
		Composite classNameDialog = new Composite(parent, SWT.NONE);
        GridLayout layout = new GridLayout();
        layout.numColumns = 2;
        classNameDialog.setLayout(layout);
        classNameDialog.setLayoutData(new GridData(GridData.FILL_HORIZONTAL));
        
        Label classNameLabel = new Label(classNameDialog,SWT.NONE);
        classNameLabel.setText("Class Name:");
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
		return moduleGroup.getSelectedModuleDecl();
		
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
