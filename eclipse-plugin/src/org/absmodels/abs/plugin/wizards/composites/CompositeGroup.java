/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.absmodels.abs.plugin.wizards.composites;

import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.jface.viewers.*;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Listener;
import org.eclipse.ui.part.DrillDownComposite;

public abstract class CompositeGroup extends Composite {
	/**
	 *  The listener to notify of events
	 */
	protected Listener listener;

	/**
	 * The internal tree viewer component
	 */
	protected TreeViewer treeViewer;

	/**
	 *  the message to display at the top of this dialog
	 */
	protected String DISPLAY_MSG = "Select a file:";

	/**
	 * Width sizing constant
	 */
	protected static final int SIZING_SELECTION_PANE_WIDTH = 320;

	/**
	 * Height sizing constant
	 */
	protected static final int SIZING_SELECTION_PANE_HEIGHT = 300;

	/**
	 * Creates a new instance of the widget.
	 * 
	 * @param parent
	 *            The parent widget of the group.
	 * @param listener
	 *            A listener to forward events to. Can be null if no listener is
	 *            required.
	 */
	public CompositeGroup(Composite parent, Listener listener) {
		this(parent, listener, null);
	}

	/**
	 * Creates a new instance of the widget.
	 * 
	 * @param parent
	 *            The parent widget of the group.
	 * @param listener
	 *            A listener to forward events to. Can be null if no listener is
	 *            required.
	 * @param message
	 *            The text to present to the user.
	 */
	public CompositeGroup(Composite parent, Listener listener,
			String message) {
		this(parent, listener, message,
				SIZING_SELECTION_PANE_HEIGHT,
				SIZING_SELECTION_PANE_WIDTH);
	}

	/**
	 * Creates a new instance of the widget.
	 * 
	 * @param parent
	 *            The parent widget of the group.
	 * @param listener
	 *            A listener to forward events to. Can be null if no listener is
	 *            required.
	 * @param message
	 *            The text to present to the user.
	 * @param heightHint
	 *            height hint for the drill down composite
	 * @param widthHint
	 *            width hint for the drill down composite
	 */
	public CompositeGroup(Composite parent, Listener listener,
			String message, int heightHint, int widthHint) {
		super(parent, SWT.NONE);
		this.listener = listener;
		if (message != null) {
			createContents(message, heightHint, widthHint);
		} else {
			createContents(DISPLAY_MSG, heightHint, widthHint);
		}
	}

	/**
	 * The resource selection has changed in the tree view. Update the
	 * container name field value and notify all listeners.
	 * 
	 * @param o
	 *            The container that changed
	 */
	protected abstract void resourceSelectionChanged(Object o);
	
	
	protected void fireSelectionChangedEvent(){
		// fire an event so the parent can update its controls
		if (listener != null) {
			Event changeEvent = new Event();
			changeEvent.type = SWT.Selection;
			changeEvent.widget = this;
			listener.handleEvent(changeEvent);
		}		
	}

	/**
	 * Creates the contents of the composite.
	 * 
	 * @param message
	 */
	protected final void createContents(String message) {
		createContents(message, SIZING_SELECTION_PANE_HEIGHT,
				SIZING_SELECTION_PANE_WIDTH);
	}

	/**
	 * Creates the contents of the composite.
	 * 
	 * @param message
	 * @param heightHint
	 * @param widthHint
	 */
	protected abstract void createContents(String message, int heightHint, int widthHint);

	protected DrillDownComposite createDrillDown(int heightHint){
		DrillDownComposite drillDown = new DrillDownComposite(this, SWT.BORDER);
		GridData spec = new GridData(SWT.FILL, SWT.FILL, true, true);
		spec.widthHint = SIZING_SELECTION_PANE_WIDTH;
		spec.heightHint = heightHint;
		drillDown.setLayoutData(spec);
		return drillDown;
	}
	
	protected void setUpTreeViewer(){
		treeViewer.setComparator(new ViewerComparator());
		treeViewer.setUseHashlookup(true);
		setUpTreeListeners();

		// This has to be done after the viewer has been laid out
		treeViewer.setInput(ResourcesPlugin.getWorkspace());		
	}

	/**
	 * 
	 */
	private void setUpTreeListeners() {
		treeViewer.addSelectionChangedListener(new ISelectionChangedListener() {
			@Override
			public void selectionChanged(SelectionChangedEvent event) {
				IStructuredSelection selection = (IStructuredSelection) event
						.getSelection();
				Object firstElement = selection.getFirstElement();
				resourceSelectionChanged(firstElement);

			}
		});
		treeViewer.addDoubleClickListener(new IDoubleClickListener() {
			@Override
			public void doubleClick(DoubleClickEvent event) {
				ISelection selection = event.getSelection();
				if (selection instanceof IStructuredSelection) {
					Object item = ((IStructuredSelection) selection)
							.getFirstElement();
					if (item == null) {
						return;
					}
					if (treeViewer.getExpandedState(item)) {
						treeViewer.collapseToLevel(item, 1);
					} else {
						treeViewer.expandToLevel(item, 1);
					}
				}
			}
		});
	}
	/**
	 * Returns a new drill down viewer for this dialog.
	 * 
	 * @param heightHint
	 *            height hint for the drill down composite
	 */
	protected abstract void createTreeViewer(int heightHint);
}
