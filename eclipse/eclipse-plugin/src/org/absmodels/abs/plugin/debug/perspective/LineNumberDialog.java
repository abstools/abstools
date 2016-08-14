/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.absmodels.abs.plugin.debug.perspective;

import static org.absmodels.abs.plugin.actions.ActionUtils.getActiveFile;
import static org.absmodels.abs.plugin.debug.DebugUtils.getSchedulerRef;

import org.eclipse.core.resources.IFile;
import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Spinner;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.PlatformUI;

/**
 * Dialog for runToLine functionality of the debugger.
 * @author tfischer
 *
 */
public class LineNumberDialog extends Dialog{

	private Spinner line;
	
	protected LineNumberDialog(Shell parentShell) {
		super(parentShell);
	}

	@Override
	protected Control createDialogArea(Composite parent) {
		GridLayout gridLayout = new GridLayout(1, false);
		Composite composite = (Composite)super.createDialogArea(parent);
	    composite.setLayout(gridLayout);
	    
	    Composite stepsContainer = new Composite(composite, SWT.NONE);
		stepsContainer.setLayout(gridLayout);
	    
	    line = new Spinner(stepsContainer, SWT.NONE);
		line.setMinimum(0);
		line.setMaximum(Integer.MAX_VALUE);
		line.setIncrement(1);
		line.setSelection(0);
	     
	    return composite;
	}

	@Override
	protected void configureShell(Shell newShell) {
	     super.configureShell(newShell);
	     newShell.setText("Run to line");
	}
	
	@Override
	protected void okPressed() {
		int n = line.getSelection();
		IEditorPart activeEditor = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getActivePage().getActiveEditor();
		IFile activeFile = getActiveFile(activeEditor);
		getSchedulerRef().doRunToLine(activeFile.getLocation().toFile().getAbsolutePath(), n);
		super.okPressed();
	}
}
