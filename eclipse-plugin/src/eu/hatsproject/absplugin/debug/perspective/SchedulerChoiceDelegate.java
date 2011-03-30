/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package eu.hatsproject.absplugin.debug.perspective;

import static eu.hatsproject.absplugin.debug.DebugUtils.getSchedulerRef;
import static eu.hatsproject.absplugin.debug.DebugUtils.scheduler;
import static eu.hatsproject.absplugin.util.Images.DEBUGGER_INTERACTIVE;
import static eu.hatsproject.absplugin.util.Images.DEBUGGER_RANDOM;

import org.eclipse.jface.action.IAction;
import org.eclipse.jface.action.IMenuCreator;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.MenuItem;
import org.eclipse.swt.widgets.ToolItem;
import org.eclipse.ui.IActionDelegate2;
import org.eclipse.ui.IViewActionDelegate;
import org.eclipse.ui.IViewPart;

import eu.hatsproject.absplugin.debug.DebugUtils;
import eu.hatsproject.absplugin.util.Constants.Scheduler;

//import eu.hatsproject.absplugin.debug.DebugUtils;
//import eu.hatsproject.absplugin.util.Constants.Scheduler;
//import eu.hatsproject.absplugin.util.Images;

/**
 * Class handling clicks on the selectScheduler button.
 * @author mweber
 */
public class SchedulerChoiceDelegate implements	IViewActionDelegate, IActionDelegate2, IMenuCreator{

	private Menu schedulerMenu;
	private MenuItem interactiveItem;
	private MenuItem randomItem;
	
	@Override
	public void dispose() {
		if(schedulerMenu != null){
			interactiveItem.dispose();
			randomItem.dispose();
			schedulerMenu.dispose();
		}
	}

	@Override
	public void init(IAction action) {
		action.setMenuCreator(this);
		DebugUtils.schedulerMenu = action;
		action.setEnabled(false);
	}

	@Override
	public Menu getMenu(Menu parent) {
		return null;
	}

	@Override
	public Menu getMenu(Control parent) {
		if(schedulerMenu == null){
			schedulerMenu = new Menu(parent);

			interactiveItem = new MenuItem(schedulerMenu, SWT.RADIO);
			interactiveItem.setText("&Interactive Scheduler");
			interactiveItem.setImage(DEBUGGER_INTERACTIVE);
			interactiveItem.setSelection(true);
			interactiveItem.addSelectionListener(new SelectionListener() {
				@Override
				public void widgetSelected(SelectionEvent e) {
					scheduler = Scheduler.interactive;
					if(getSchedulerRef() != null)
						getSchedulerRef().updateScheduler();
				}
				
				@Override
				public void widgetDefaultSelected(SelectionEvent e) {
				}
			});
			
			randomItem = new MenuItem(schedulerMenu, SWT.RADIO);
			randomItem.setText("&Random Scheduler");
			randomItem.setImage(DEBUGGER_RANDOM);
			randomItem.setSelection(false);
			randomItem.addSelectionListener(new SelectionListener() {
				@Override
				public void widgetSelected(SelectionEvent e) {
					scheduler = Scheduler.random;
					if(getSchedulerRef() != null)
						getSchedulerRef().updateScheduler();
				}
				
				@Override
				public void widgetDefaultSelected(SelectionEvent e) {
				}
			});
		}
		return schedulerMenu;
	}

	@Override
	public void selectionChanged(IAction action, ISelection selection) {

	}

	@Override
	public void runWithEvent(IAction action, Event event) {
		if (event.widget instanceof ToolItem) {
			ToolItem toolItem = (ToolItem) event.widget;
			Control control = toolItem.getParent();
    		Menu menu= getMenu(control);
    		
    		Rectangle bounds = toolItem.getBounds();
    		Point topLeft = new Point(bounds.x, bounds.y + bounds.height);
    		menu.setLocation(control.toDisplay(topLeft));
    		menu.setVisible(true);
    	}
	}

	@Override
	public void init(IViewPart view) {
		
	}

	@Override
	public void run(IAction action) {
		
	}

}
