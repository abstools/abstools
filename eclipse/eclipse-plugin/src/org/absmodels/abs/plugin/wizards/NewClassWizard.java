/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.absmodels.abs.plugin.wizards;

import org.absmodels.abs.plugin.Activator;
import org.absmodels.abs.plugin.wizards.WizardUtil.EditorData;
import org.absmodels.abs.plugin.wizards.WizardUtil.InsertType;
import org.absmodels.abs.plugin.wizards.pages.IABSClassInterfaceWizardPage;
import org.absmodels.abs.plugin.wizards.pages.NewClassInFileWizardPage;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.ui.INewWizard;
import org.eclipse.ui.IWorkbench;


/**
 * Wizard for creating a new class in an existing module
 * @author cseise
 *
 */
public class NewClassWizard extends NewClassInterfaceWizard implements INewWizard {

	private NewClassInFileWizardPage classInFilePage;

	private final String WIZARD_TITLE = "New ABS class"; 
	private final String WIZARD_DESCRIPTION = "Create a new ABS Class within an ABS module";

	public NewClassWizard() {		
		insertType = InsertType.INSERT_CLASS;
	}

	@Override
	public void init(IWorkbench workbench, IStructuredSelection selection) {
		super.init(workbench, selection);
		setWindowTitle("New ABS class");
	}

	@Override
	public void addPages(){
		classInFilePage = new NewClassInFileWizardPage(WIZARD_TITLE);
		page = classInFilePage;
		page.setInitialModule(mDecl);
		page.setDescription(WIZARD_DESCRIPTION);
		page.setTitle(WIZARD_TITLE);
		addPage(page);
	}

	@Override
	public boolean performFinish() {
		this.findModuleDecl();

		EditorData d = WizardUtil.getDocumentForModuleDecl(mDecl);
		if (d.editor == null || d.document == null)
			return false;
		IDocument document = d.document;
		try {
			int off = WizardUtil.getInsertionPosition(document,mDecl);
			final String insertString = insertType.getInsertionString(page.getNewName());

			switch (classInFilePage.getResultType()){
			case TYPE_UNDEFINED:
				document.replace(off, 0, insertString);
				break;
			default:
				document.replace(off, 0, "\n " + classInFilePage.getResultType().getAnnotationString() + insertString);
				break;
			}

			off += insertType.getInsertOffset(page.getNewName()) + classInFilePage.getResultType().getAnnotationLength();

			WizardUtil.saveEditorAndGotoOffset(d.editor, off);

			return true;
		} catch (BadLocationException e) {
			Activator.logException(e);
			MessageDialog.openError(getShell(), "Error", "Fatal Error in wizard: The module declaration could not be found!");
			return false;
		}
	}

}
