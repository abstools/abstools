/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.absmodels.abs.plugin.wizards;

import org.absmodels.abs.plugin.wizards.WizardUtil.InsertType;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.wizard.Wizard;
import org.eclipse.ui.INewWizard;
import org.eclipse.ui.IWorkbench;


/**
 * Class for providing common functionality to ABS wizards.
 * @author cseise
 *
 */
public abstract class ABSNewWizard extends Wizard implements INewWizard {

	protected String INSERT_STRING1 = "";
	protected String INSERT_STRING2 = "";
	protected int INSERT_OFFSET = 0;
	
	protected IWorkbench workbench;
	
	protected InsertType insertType;
	
	
	@Override
	public void init(IWorkbench workbench, IStructuredSelection selection){
		this.workbench = workbench;
	}

	@Override
	public abstract boolean performFinish();

}
