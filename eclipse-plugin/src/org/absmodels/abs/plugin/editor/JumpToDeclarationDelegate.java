/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.absmodels.abs.plugin.editor;

import org.eclipse.jface.action.IAction;
import org.eclipse.jface.text.TextSelection;
import org.eclipse.jface.text.hyperlink.IHyperlink;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.ui.IEditorActionDelegate;
import org.eclipse.ui.IEditorPart;

public class JumpToDeclarationDelegate implements IEditorActionDelegate {
	private IEditorPart editor;
	
	@Override
	public void run(IAction action) {
	    if(!(editor instanceof ABSEditor)){
	        return;
	    }
	    ABSEditor abseditor = (ABSEditor) editor;
	    TextSelection sel = (TextSelection)abseditor.getSelectionProvider().getSelection();
	    IHyperlink[] links = AbsHyperlinkDetector.getHyperlinks(abseditor, sel.getOffset());
	    if (links != null && links.length > 0) {
	        links[0].open();
	    } else {
	        abseditor.openInformation("Error", "Could not find declaration for Element under cursor.");
	    }
	}

	@Override
	public void selectionChanged(IAction action, ISelection selection) {
		//nothing
	}

	@Override
	public void setActiveEditor(IAction action, IEditorPart targetEditor) {
		this.editor = targetEditor;
	}

}
