/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.absmodels.abs.plugin.navigator.actionProvider;

import org.eclipse.ui.IActionBars;
import org.eclipse.ui.navigator.CommonActionProvider;
import org.eclipse.ui.navigator.ICommonActionConstants;
import org.eclipse.ui.navigator.ICommonActionExtensionSite;

/**
 * Action provider for hooking double click support (ASTNodes and ModulePaths) to the PackageExplorer
 * @author cseise
 *
 */
public class DoubleClickActionProvider extends CommonActionProvider {

	private final DoubleClickAction doubleClickAction = new DoubleClickAction();
	private ICommonActionExtensionSite aSite;
	
	
	@Override
	public void init(ICommonActionExtensionSite aSite){
		super.init(aSite);
		this.aSite = aSite;
		
		aSite.getStructuredViewer().addSelectionChangedListener(doubleClickAction);
	}
	
	@Override
	public void fillActionBars(IActionBars actionBars){
		super.fillActionBars(actionBars);
		//forward doubleClick to appropriate ActionHandler
		actionBars.setGlobalActionHandler(ICommonActionConstants.OPEN, doubleClickAction);
	}
	
	@Override
	public void dispose(){
		aSite.getStructuredViewer().removeSelectionChangedListener(doubleClickAction);
	}
	

}
