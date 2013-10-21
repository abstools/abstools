/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.absmodels.abs.plugin.wizards.pages;

import org.absmodels.abs.plugin.util.InternalASTNode;
import org.eclipse.jface.wizard.IWizardPage;

import abs.frontend.ast.ModuleDecl;

/**
 * Interface for new ABS class or new ABS interface wizards
 * 
 * @author cseise
 * 
 */
public interface IABSClassInterfaceWizardPage extends IWizardPage {

	/**
	 * Retrieves the module declaration the user has selected in the wizard
	 * page.
	 * 
	 * @return The module declaration the user has selected or null if the
	 *         selection is empty or is not an module declaration
	 */
	public InternalASTNode<ModuleDecl> getResultModule();

	/**
	 * @return The value that should be used for the new element
	 */
	public String getNewName();

	/**
	 * Sets the initial module declaration selection to m
	 * 
	 * @param m
	 *            ModuleDecl
	 */
	public void setInitialModule(InternalASTNode<ModuleDecl> m);

	/**
	 * Fills the name field of the page with the given name
	 * 
	 * @param name
	 *            String
	 */
	public void setInitialName(String name);

}
