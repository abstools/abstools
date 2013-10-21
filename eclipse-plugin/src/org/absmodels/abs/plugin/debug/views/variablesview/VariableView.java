/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.absmodels.abs.plugin.debug.views.variablesview;

import org.eclipse.debug.ui.AbstractDebugView;
import org.eclipse.jface.action.IMenuManager;
import org.eclipse.jface.action.IToolBarManager;
import org.eclipse.jface.viewers.DoubleClickEvent;
import org.eclipse.jface.viewers.IDoubleClickListener;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ControlAdapter;
import org.eclipse.swt.events.ControlEvent;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.TreeColumn;

/**
 * This is the ABS variable view intended to be used in the ABS debug perspective. It contains
 * a TreeViewer representing the contents of a abs.backend.java.observing.TaskStackFrameView
 * or an abs.backend.java.observing.ObjectView.
 * @author tfischer
 */
public class VariableView extends AbstractDebugView{

	@Override
	protected Viewer createViewer(final Composite parent) {
		final TreeViewer viewer = new TreeViewer(parent, SWT.SINGLE | SWT.FULL_SELECTION);
		viewer.setContentProvider(new VariableContentProvider());
		viewer.setLabelProvider(new VariableLabelProvider());
		
		final TreeColumn column1 = new TreeColumn(viewer.getTree(), SWT.LEFT);
		column1.setText("Name");
		final TreeColumn column2 = new TreeColumn(viewer.getTree(), SWT.LEFT);
		column2.setText("Value");
		
		viewer.getTree().setHeaderVisible(true);
		viewer.getTree().setLinesVisible(true);
		
		viewer.addDoubleClickListener(new IDoubleClickListener() {
			@Override
			public void doubleClick(DoubleClickEvent event) {
				Object selection = ((IStructuredSelection)event.getSelection()).getFirstElement();
				if (selection != null) {
				    viewer.setExpandedState(selection, !viewer.getExpandedState(selection));
				}   
			}
		});
		
		parent.addControlListener(new ControlAdapter() {
		    @Override
			public void controlResized(ControlEvent e) {
		    	Rectangle area = parent.getClientArea();
		    	int width = area.width;
    	
		    	column1.setWidth(width/3);
		    	column2.setWidth(2*width/3);
		    }
		  });
		return viewer;
	}

	@Override
	protected void createActions() {}

	@Override
	protected String getHelpContextId() { return null; }

	@Override
	protected void fillContextMenu(IMenuManager menu) {}

	@Override
	protected void configureToolBar(IToolBarManager tbm) {}

}
