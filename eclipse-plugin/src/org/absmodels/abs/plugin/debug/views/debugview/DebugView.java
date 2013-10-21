/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.absmodels.abs.plugin.debug.views.debugview;

import static org.absmodels.abs.plugin.debug.DebugUtils.getSchedulerRef;
import static org.absmodels.abs.plugin.debug.DebugUtils.highlightLine;
import static org.absmodels.abs.plugin.debug.DebugUtils.openVariableView;
import static org.absmodels.abs.plugin.debug.DebugUtils.refreshButtonEnablement;
import static org.absmodels.abs.plugin.debug.DebugUtils.refreshVariableView;

import org.absmodels.abs.plugin.debug.model.Debugger;
import org.eclipse.debug.ui.AbstractDebugView;
import org.eclipse.jface.action.IMenuManager;
import org.eclipse.jface.action.IToolBarManager;
import org.eclipse.jface.viewers.DoubleClickEvent;
import org.eclipse.jface.viewers.IDoubleClickListener;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.IViewSite;
import org.eclipse.ui.PartInitException;

import abs.backend.java.debugging.TaskInfo;
import abs.backend.java.observing.ObjectView;
import abs.backend.java.observing.TaskStackFrameView;
import abs.backend.java.observing.TaskView;

/**
 * This is the ABS debug view intended to be used in the ABS debug perspective. It contains
 * a TreeViewer representing the current state of the debugged ABS program as well as a set of 
 * buttons which allow the user to control the debug process (see plugin.xml to see how buttons are added).
 * @author tfischer
 */
public class DebugView extends AbstractDebugView {
	
	private Debugger debugger;
	
	public DebugView() {
		debugger = new Debugger();
	}

	@Override
	public void init(IViewSite site) throws PartInitException {
		super.init(site);
		// active scope of the debug view when component is active
	}
	
	@Override
	protected void configureToolBar(IToolBarManager arg0) { }
	@Override
	protected void createActions() { }

	@Override
	protected Viewer createViewer(Composite parent) {
		final TreeViewer viewer = new TreeViewer(parent);
		viewer.setContentProvider(new DebugTreeContentProvider());
		viewer.setLabelProvider(new DebugTreeStyledLabelProvider());
		viewer.setInput(debugger.getModel());
		viewer.setAutoExpandLevel(TreeViewer.ALL_LEVELS);
		viewer.addSelectionChangedListener(new ISelectionChangedListener() {
			@Override
			public void selectionChanged(SelectionChangedEvent event) {
				refreshButtonEnablement();
				Object selection = ((IStructuredSelection)event.getSelection()).getFirstElement();
				if(selection instanceof TaskView || selection instanceof ObjectView || selection instanceof TaskStackFrameView){
					refreshVariableView();
					if(selection instanceof TaskView){
						TaskView tv = (TaskView) selection;
						TaskInfo taskInfo = getDebugger().getModel().getTaskInfo(tv);
						highlightLine(taskInfo);
					}
				}
				getSchedulerRef().taskSelectionChanged(selection);
			}
		});
		
		viewer.addDoubleClickListener(new IDoubleClickListener() {
			
			@Override
			public void doubleClick(DoubleClickEvent event) {
				Object selection = ((IStructuredSelection)event.getSelection()).getFirstElement();
				if(selection instanceof ObjectView || selection instanceof TaskStackFrameView){
					openVariableView();
					refreshVariableView();
				} else{
					viewer.setExpandedState(selection, !viewer.getExpandedState(selection));
				}
			}
		});
		
		viewer.expandAll();
		return viewer;
	}

	@Override
	protected void fillContextMenu(IMenuManager arg0) {	}

	@Override
	protected String getHelpContextId() { return null; }
	
	/**
	 * Returns the org.abs-models.abs.plugin.debug.model.Debugger of this view. Whenever a DebugView is created,
	 * a new debugger is created and added, so a debugger always exists.
	 * @return Debugger of this view
	 */
	public Debugger getDebugger(){
		return debugger;
	}	
	
}
