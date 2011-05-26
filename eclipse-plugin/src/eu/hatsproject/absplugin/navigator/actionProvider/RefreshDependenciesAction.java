/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package eu.hatsproject.absplugin.navigator.actionProvider;

import static eu.hatsproject.absplugin.navigator.NavigatorUtils.updateDependencies;
import static eu.hatsproject.absplugin.util.Constants.MODULE_DECORATOR_ID;

import org.eclipse.jface.action.Action;
import org.eclipse.jface.viewers.IBaseLabelProvider;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.TreeSelection;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.jface.window.IShellProvider;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.actions.RefreshAction;
import org.eclipse.ui.navigator.CommonViewer;

import eu.hatsproject.absplugin.Activator;
import eu.hatsproject.absplugin.decorators.ModuleDecorator;
import eu.hatsproject.absplugin.navigator.ABSNavigator;

/**
 * An action for refresh the package dependencies 
 * @author pwong
 *
 */
public class RefreshDependenciesAction extends Action implements ISelectionChangedListener {
	
	protected CommonViewer viewer;
	protected ISelection selection;
	protected final Shell shell;
	
	protected final IShellProvider provider = new IShellProvider() {
		public Shell getShell() {
			return shell;
		}
	};
	
	public RefreshDependenciesAction(Shell shell, ISelection iSelection) {
		this(shell,iSelection,"Refresh");
	}
	
	public RefreshDependenciesAction(Shell shell, ISelection iSelection, String name) {
		super(name);
		this.shell = shell;
		this.selection = iSelection;
	}
	
	public void selectionChanged(SelectionChangedEvent event) {
		viewer = (CommonViewer) event.getSource();
		selection = event.getSelection();
	}
	
	@Override
	public void run(){
		if (selection != null && selection instanceof TreeSelection){
			TreeSelection ts = (TreeSelection) selection;
			updateDependencies(ts);
			RefreshAction action = new RefreshAction(provider);
			action.refreshAll();
			refreshLabelProvider();
		}
	}
	
	private CommonViewer getCommonViewer() {
		return viewer;
	}
	
	/**
	 * Updates the Common Viewer in an asynchronous execution.
	 * XXX copy from {@link ABSNavigator}
	 */	
	private void refreshLabelProvider() {

		Display.getDefault().asyncExec(new Runnable() {
			@Override
			public void run() {
				CommonViewer viewer = getCommonViewer();
				if (viewer != null && !viewer.getControl().isDisposed()) {
					viewer.refresh();
				}
				
				final IBaseLabelProvider baseLabelProvider = 
					Activator.
					getDefault().
					getWorkbench().
					getDecoratorManager().
					getBaseLabelProvider(MODULE_DECORATOR_ID);

				if (baseLabelProvider instanceof ModuleDecorator) {
					((ModuleDecorator) baseLabelProvider).refresh();
				}
			}

		});
	}

}
