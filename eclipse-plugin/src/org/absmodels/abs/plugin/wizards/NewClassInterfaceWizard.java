/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.absmodels.abs.plugin.wizards;

import org.absmodels.abs.plugin.navigator.ModulePath;
import org.absmodels.abs.plugin.util.InternalASTNode;
import org.absmodels.abs.plugin.wizards.pages.IABSClassInterfaceWizardPage;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.ui.INewWizard;
import org.eclipse.ui.IWorkbench;

import abs.frontend.ast.ModuleDecl;

/**
 * Class for providing common functionality to the new interface and new class wizard
 * @author cseise
 *
 */
public abstract class NewClassInterfaceWizard extends ABSNewWizard implements INewWizard {

	protected InternalASTNode<ModuleDecl> mDecl;

	protected IABSClassInterfaceWizardPage page;

	@Override
	public void init(IWorkbench workbench, IStructuredSelection selection) {
		super.init(workbench, selection);
		prepareSelection(selection);

	}

	/**
	 * Extracts a module declaration out of a selection. And sets the internal
	 * state accordingly. If there if no module declaration, the internal state
	 * will not be changed.
	 * 
	 * @param selection
	 */
	@SuppressWarnings("unchecked")
	protected void prepareSelection(IStructuredSelection selection) {
		if (selection != null) {
			Object firstSelection = selection.getFirstElement();
			if (firstSelection instanceof ModulePath) {
				mDecl = ((ModulePath) firstSelection).getModuleDecl();
			} else if (firstSelection instanceof InternalASTNode<?>) {
				InternalASTNode<?> internalASTNode = (InternalASTNode<?>) firstSelection;

				if (internalASTNode.hasASTNodeOfType(ModuleDecl.class))
					//Suppress warnings as we have checked that the internal ASTNode
					//contains a module declaration
					mDecl = (InternalASTNode<ModuleDecl>) internalASTNode;
			}
		}
	}

	@Override
	public abstract void addPages();

	protected void findModuleDecl() {
	    if (mDecl == null || !mDecl.equals(page.getResultModule())){
			mDecl = page.getResultModule();
		}
	}

	@Override
	public abstract boolean performFinish();

}
