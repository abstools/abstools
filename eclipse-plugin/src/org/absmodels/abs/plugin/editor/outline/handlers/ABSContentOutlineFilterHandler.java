/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.absmodels.abs.plugin.editor.outline.handlers;

import org.absmodels.abs.plugin.editor.outline.ABSContentOutlineFiltersAndComparators;
import org.absmodels.abs.plugin.editor.outline.ABSContentOutlinePage;
import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.commands.IHandler;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.jface.viewers.ViewerFilter;
import org.eclipse.swt.SWTException;
import org.eclipse.swt.widgets.Display;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.handlers.HandlerUtil;
import org.eclipse.ui.views.contentoutline.IContentOutlinePage;


/**
 * Handler for adding and removing filters to and from the ABS content outline
 * 
 * @author cseise
 * 
 */
public class ABSContentOutlineFilterHandler extends AbstractHandler implements IHandler {
	/**
	 * {@inheritDoc}
	 * 
	 * @throws SWTException
	 *             - ERROR_DEVICE_DISPOSED - if the default {@link Display} is
	 *             already disposed
	 */
	@Override
	public Object execute(ExecutionEvent event) throws ExecutionException {
		IEditorPart editor = HandlerUtil.getActiveEditorChecked(event);
		String commandId = event.getCommand().getId();
		IContentOutlinePage page = (IContentOutlinePage) editor.getAdapter(IContentOutlinePage.class);

		if (page != null && page instanceof ABSContentOutlinePage) {

			ABSContentOutlinePage absOutlinePage = (ABSContentOutlinePage) page;

			final ViewerFilter filter = ABSContentOutlineFiltersAndComparators.getFilterOfCommand(commandId);

			boolean oldState = HandlerUtil.toggleCommandState(event.getCommand());

			if (!oldState) {
				absOutlinePage.getTreeView().addFilter(filter);
			} else {
				absOutlinePage.getTreeView().removeFilter(filter);
			}

			absOutlinePage.getTreeView().setAutoExpandLevel(TreeViewer.ALL_LEVELS);
		}
		return null;
	}
}
