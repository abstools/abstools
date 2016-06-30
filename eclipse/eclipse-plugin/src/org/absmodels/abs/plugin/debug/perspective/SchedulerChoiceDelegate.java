/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.absmodels.abs.plugin.debug.perspective;

import static org.absmodels.abs.plugin.debug.DebugUtils.getSchedulerRef;
import static org.absmodels.abs.plugin.util.Images.DEBUGGER_INTERACTIVE;
import static org.absmodels.abs.plugin.util.Images.DEBUGGER_RANDOM;

import org.absmodels.abs.plugin.debug.DebugUtils;
import org.absmodels.abs.plugin.util.Images;
import org.absmodels.abs.plugin.util.Constants.Scheduler;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.action.IMenuCreator;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.graphics.Image;
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



/**
 * Class handling clicks on the selectScheduler button.
 * @author mweber
 */
public class SchedulerChoiceDelegate implements	IViewActionDelegate, IActionDelegate2, IMenuCreator{

	private Menu schedulerMenu;
	private MenuItem interactiveItem;
	private MenuItem randomItem;
	private MenuItem historyItem;
	
	@Override
	public void dispose() {
		if(schedulerMenu != null){
			interactiveItem.dispose();
			randomItem.dispose();
			historyItem.dispose();
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
			
			interactiveItem = newSchedulerChoiceMenuItem(schedulerMenu, "&Interactive Scheduler",
                    Images.DEBUGGER_INTERACTIVE, Scheduler.interactive); 
			
			randomItem = newSchedulerChoiceMenuItem(schedulerMenu, "&Random Scheduler",
                    Images.DEBUGGER_RANDOM, Scheduler.random); 
            
			historyItem = newSchedulerChoiceMenuItem(schedulerMenu, "Replay &History",
                    Images.DEBUGGER_HISTORY, Scheduler.replay); 
            
		}
		return schedulerMenu;
	}

	private MenuItem newSchedulerChoiceMenuItem(Menu schedulerMenu, String text, Image img, final Scheduler schedulerType) {
	    final MenuItem item = new MenuItem(schedulerMenu, SWT.RADIO);
        item.setText(text);
        item.setImage(img);
        item.setSelection(DebugUtils.getScheduler() == schedulerType);
        item.addSelectionListener(new SelectionListener() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                if (item.getSelection()) {
                    DebugUtils.setScheduler(schedulerType);
                }
            }

            @Override
            public void widgetDefaultSelected(SelectionEvent e) {
            }
        });
        return item;
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

    public void setSelection(Scheduler scheduler) {
        if(schedulerMenu != null){
            interactiveItem.setSelection(scheduler == Scheduler.interactive);
            randomItem.setSelection(scheduler == Scheduler.random);
            historyItem.setSelection(scheduler == Scheduler.replay);
        }
    }

}
