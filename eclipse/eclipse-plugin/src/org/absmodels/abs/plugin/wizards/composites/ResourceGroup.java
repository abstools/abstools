/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.absmodels.abs.plugin.wizards.composites;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.resources.IContainer;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IResource;
import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Listener;
import org.eclipse.ui.model.WorkbenchLabelProvider;
import org.eclipse.ui.part.DrillDownComposite;

public class ResourceGroup extends CompositeGroup {
	
	/**
	 *  Last selection made by user
	 */
	private IResource selectedResource;
	/**
	 * Creates a new instance of the widget.
	 * 
	 * @param parent
	 *            The parent widget of the group.
	 * @param listener
	 *            A listener to forward events to. Can be null if no listener is
	 *            required.
	 */
	public ResourceGroup(Composite parent, Listener listener) {
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
	public ResourceGroup(Composite parent, Listener listener,
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
	public ResourceGroup(Composite parent, Listener listener,
			String message, int heightHint, int widthHint) {
		super(parent, listener, message, heightHint, widthHint);
	}

	/**
	 * The resource selection has changed in the tree view. Update the
	 * container name field value and notify all listeners.
	 * 
	 * @param o
	 *            The object that changed
	 */
	@Override
	public void resourceSelectionChanged(Object o) {		
		if (o instanceof IResource){
			selectedResource = (IResource)o;
			fireSelectionChangedEvent();
		}
	}

	/**
	 * Creates the contents of the composite.
	 * 
	 * @param message
	 * @param heightHint
	 * @param widthHint
	 */
	@Override
	protected void createContents(String message, int heightHint, int widthHint) {
		GridLayout layout = new GridLayout();
		layout.marginWidth = 0;
		setLayout(layout);
		setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));

		Label label = new Label(this, SWT.WRAP);
		label.setText(message);
		label.setFont(this.getFont());

		//filler
		new Label(this, SWT.NONE);

		createTreeViewer(heightHint);
		Dialog.applyDialogFont(this);
	}

	/**
	 * Returns a new drill down viewer for this dialog.
	 * 
	 * @param heightHint
	 *            height hint for the drill down composite
	 */
	@Override
	protected void createTreeViewer(int heightHint) {
		// Create drill down.
		DrillDownComposite drillDown = createDrillDown(heightHint);
		
		// Create tree viewer inside drill down.
		treeViewer = new TreeViewer(drillDown, SWT.NONE);
		drillDown.setChildTree(treeViewer);
		ResourceGroupContentProvider cp = new ResourceGroupContentProvider();
		treeViewer.setContentProvider(cp);
		treeViewer.setLabelProvider(WorkbenchLabelProvider
				.getDecoratingWorkbenchLabelProvider());
		
		setUpTreeViewer();
	}

	/**
	 * Retrieves the IFile selected in this wizard page, or null if the
	 * selection is not an instance of IFile
	 * 
	 * @return the selected file or <i>null</i> if no instance of IFile was selected
	 */
	public IFile getSelectedFile() {
		return (selectedResource instanceof IFile) ? (IFile) selectedResource : null;
	}

	/**
	 * Sets the selected existing container.
	 * 
	 * @param container
	 */
	public void setSelectedResource(IResource container) {
		selectedResource = container;

		// expand to and select the specified container
		List<IResource> itemsToExpand = new ArrayList<IResource>();
		IContainer parent = container.getParent();
		itemsToExpand.add(0,container);
		while (parent != null) {
			itemsToExpand.add(0, parent);
			parent = parent.getParent();
		}
		treeViewer.setExpandedElements(itemsToExpand.toArray());
		treeViewer.setSelection(new StructuredSelection(container), true);
	}
}
