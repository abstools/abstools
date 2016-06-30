/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.absmodels.abs.plugin.navigator.actionProvider;

import org.eclipse.jface.action.IMenuManager;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.navigator.CommonActionProvider;
import org.eclipse.ui.navigator.ICommonActionExtensionSite;

/**
 * A provider the {@link RefreshDependenciesAction} that provides menu
 * items on the context menu of the package container for refresh and update.
 * @author pwong
 *
 */
public class RefreshDependenciesProvider extends CommonActionProvider {
	
	private RefreshDependenciesAction refresh;
	private MavenAction maven;
	private ICommonActionExtensionSite aSite;
	private Shell shell;
	
	@Override
	public void init(ICommonActionExtensionSite aSite){
		super.init(aSite);
		this.aSite = aSite;
		this.shell = aSite.getViewSite().getShell();
		ISelection selection = aSite.getStructuredViewer().getSelection();
		refresh = new RefreshDependenciesAction(shell,selection);
		maven = new MavenAction(shell,selection);
		aSite.getStructuredViewer().addSelectionChangedListener(refresh);
		aSite.getStructuredViewer().addSelectionChangedListener(maven);
	}
	
	@Override
	public void fillContextMenu(IMenuManager menu) {
		super.fillContextMenu(menu);
		menu.add(refresh);
		menu.add(maven);
	}
	
	@Override
	public void dispose(){
		aSite.getStructuredViewer().removeSelectionChangedListener(refresh);
		aSite.getStructuredViewer().removeSelectionChangedListener(maven);
	}
	
}
