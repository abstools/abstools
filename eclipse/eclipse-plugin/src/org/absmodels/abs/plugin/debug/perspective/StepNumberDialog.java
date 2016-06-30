/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.absmodels.abs.plugin.debug.perspective;

import org.absmodels.abs.plugin.debug.DebugUtils;
import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Spinner;


/**
 * Dialog for the executeNSteps functionality of the debugger.
 * @author tfischer
 */
public class StepNumberDialog extends Dialog{

	private Spinner steps;
	
	protected StepNumberDialog(Shell parentShell) {
		super(parentShell);
	}

	@Override
	protected Control createDialogArea(Composite parent) {
		GridLayout gridLayout = new GridLayout(1, false);
		Composite composite = (Composite)super.createDialogArea(parent);
	    composite.setLayout(gridLayout);
	    
	    Composite stepsContainer = new Composite(composite, SWT.NONE);
		stepsContainer.setLayout(gridLayout);
	    
	    steps = new Spinner(stepsContainer, SWT.NONE);
		steps.setMinimum(0);
		steps.setMaximum(Integer.MAX_VALUE);
		steps.setIncrement(1);
		steps.setSelection(0);
	     
	    return composite;
	}

	@Override
	protected void configureShell(Shell newShell) {
	     super.configureShell(newShell);
	     newShell.setText("Number of steps");
	}
	
	@Override
	protected void okPressed() {
		int n = steps.getSelection();
		DebugUtils.getSchedulerRef().doMultipleSteps(n);
		super.okPressed();
	}
}
