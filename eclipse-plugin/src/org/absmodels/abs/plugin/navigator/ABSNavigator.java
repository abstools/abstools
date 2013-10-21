/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.absmodels.abs.plugin.navigator;

import static org.absmodels.abs.plugin.util.Constants.MODULE_DECORATOR_ID;

import org.absmodels.abs.plugin.Activator;
import org.absmodels.abs.plugin.decorators.ModuleDecorator;
import org.absmodels.abs.plugin.util.CoreControlUnit;
import org.absmodels.abs.plugin.util.UtilityFunctions;
import org.absmodels.abs.plugin.util.CoreControlUnit.ResourceBuildListener;
import org.absmodels.abs.plugin.util.CoreControlUnit.ResourceBuiltEvent;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResourceChangeEvent;
import org.eclipse.core.resources.IResourceChangeListener;
import org.eclipse.core.resources.IResourceDelta;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.jface.viewers.DoubleClickEvent;
import org.eclipse.jface.viewers.IBaseLabelProvider;
import org.eclipse.jface.viewers.IDoubleClickListener;
import org.eclipse.jface.viewers.TreeSelection;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.ui.IMemento;
import org.eclipse.ui.IViewSite;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.navigator.CommonNavigator;
import org.eclipse.ui.navigator.IMementoAware;
import org.eclipse.ui.views.framelist.TreeFrame;


/**
 * Uses the Common Navigator Framework to provide an ABS navigator view
 * 
 * @author cseise
 * 
 */
public class ABSNavigator extends CommonNavigator implements IMementoAware{

	/**
	 * Resource Change Listener for updating the ABSNavigator
	 */
	ABSNavigatorResourceChangeListener resourceChangeListener = new ABSNavigatorResourceChangeListener();
	
	private IMemento memento;

	/**
	 * {@inheritDoc}
	 */
	@Override
	public void init(IViewSite site, IMemento memento) {
		try {
			super.init(site);
		} catch (PartInitException e) {
			UtilityFunctions.showErrorMessage("Fatal error during initialization of the ABS Navigator View:" + e.getLocalizedMessage());
		}
		this.memento = memento;
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public void createPartControl(Composite aParent) {
		super.createPartControl(aParent);

		this.getCommonViewer().addDoubleClickListener(new IDoubleClickListener() {

			@Override
			public void doubleClick(DoubleClickEvent event) {

				if (event.getSelection() instanceof TreeSelection) {
					TreeSelection ts = (TreeSelection) event.getSelection();

					try {
						NavigatorUtils.openEditor(ts);
					} catch (PartInitException e) {
						UtilityFunctions.showErrorMessage("Fatal error in ABS Navigator View:\n Could not open the editor connected to the selected tree element.");
					}
				}

			}
		});

		ResourcesPlugin.getWorkspace().addResourceChangeListener(resourceChangeListener,IResourceChangeEvent.POST_CHANGE);
		CoreControlUnit.addResourceBuildListener(resourceChangeListener);
		
		//Restore expanded projects
		if (memento != null){
			TreeFrame tf = new TreeFrame(this.getCommonViewer());
			tf.restoreState(memento);
			Object[] expandedElements = tf.getExpandedElements();
			if (expandedElements != null)
				this.getCommonViewer().setExpandedElements(expandedElements);			
		}

	}

	@Override
	public void dispose() {
		CoreControlUnit.removeResourceBuildListener(resourceChangeListener);
		ResourcesPlugin.getWorkspace().removeResourceChangeListener(resourceChangeListener);
		super.dispose();
	}

	/**
	 * Class responsible for updating the contents of the ABSNavigator
	 * 
	 * @author cseise
	 * 
	 */
	private class ABSNavigatorResourceChangeListener implements IResourceChangeListener, ResourceBuildListener {
		@Override
		public void resourceChanged(IResourceChangeEvent event) {
			if (event.getType() == IResourceChangeEvent.POST_CHANGE) {
				IResourceDelta[] delta = event.getDelta().getAffectedChildren();

				for (IResourceDelta d : delta) {
					if (d.getResource() instanceof IProject) {
						refreshLabelProvider();
					}
				}
			}
		}

		@Override
		public void resourceBuilt(ResourceBuiltEvent builtevent) {
			refreshLabelProvider();
		}
	}
	/**
	 * Updates the Common Viewer in an asynchronous execution.
	 */	
	private void refreshLabelProvider() {

		Display.getDefault().asyncExec(new Runnable() {
			@Override
			public void run() {
				if (!getCommonViewer().getControl().isDisposed()) {
					getCommonViewer().refresh();
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

	@Override
	public void saveState(IMemento aMemento){
		super.saveState(aMemento);
		TreeFrame tf = new TreeFrame(this.getCommonViewer());
		tf.setExpandedElements(this.getCommonViewer().getExpandedElements());
		tf.setSelection(this.getCommonViewer().getSelection());
		tf.saveState(aMemento);
	}

	@Override
	public void restoreState(IMemento aMemento) {
		// no-op
	}


}
