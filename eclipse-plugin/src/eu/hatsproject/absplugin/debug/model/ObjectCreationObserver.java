/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package eu.hatsproject.absplugin.debug.model;

import static eu.hatsproject.absplugin.debug.DebugUtils.*;

import org.eclipse.swt.widgets.Display;

import eu.hatsproject.absplugin.debug.DebugUtils;

import abs.backend.java.observing.ObjectView;

/**
 * Observer watching creation and changes of objects
 * @author tfischer
 */
public class ObjectCreationObserver implements abs.backend.java.observing.ObjectCreationObserver{
	
	@Override
	public void objectCreated(final ObjectView o) {
		if(DebugUtils.highlightStep){
			Display.getDefault().asyncExec(new Runnable() {
				@Override
				public void run() {
					getDebugger().getObjects(o.getCOG()).addObject(o);	
				}
			});
			refreshDebugViewer();
		}
	}
	@Override
	public void objectInitialized(ObjectView o) {
		if(DebugUtils.highlightStep){
			refreshDebugViewer();
		}
	}
}