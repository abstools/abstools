/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.absmodels.abs.plugin.debug.perspective;

import static org.absmodels.abs.plugin.util.Constants.ABSPERSPECTIVE_ID;
import static org.absmodels.abs.plugin.util.Constants.ABS_DEBUG_VARIABLE_VIEW;
import static org.absmodels.abs.plugin.util.Constants.ABS_DEBUG_VIEW;
import static org.absmodels.abs.plugin.util.Constants.NAVIGATOR_ID;

import org.eclipse.ui.IFolderLayout;
import org.eclipse.ui.IPageLayout;
import org.eclipse.ui.IPerspectiveFactory;
import org.eclipse.ui.console.IConsoleConstants;


/**
 * Factory for creating the ABS debug perspective. The initial perspective contains
 * an ABS debug view, a variable view, as well as an editor, outline, console, task view and navigator.
 * @author tfischer
 */
public class ABSDebugPerspectiveFactory implements IPerspectiveFactory {
	@Override
	public void createInitialLayout(IPageLayout layout) {
        String editorArea = layout.getEditorArea();
        
        IFolderLayout bottom = layout.createFolder("bottom", IPageLayout.BOTTOM,0.75f, editorArea);   
        IFolderLayout left   = layout.createFolder("left", IPageLayout.LEFT, 0.2f, editorArea);
        IFolderLayout top    = layout.createFolder("top", IPageLayout.TOP, 0.333f, editorArea);
        IFolderLayout right  = layout.createFolder("center", IPageLayout.RIGHT, 0.75f, editorArea);
       
        left.addView(ABS_DEBUG_VIEW);
        top.addView(ABS_DEBUG_VARIABLE_VIEW);
        
        right.addView(IPageLayout.ID_OUTLINE);
        bottom.addView(IConsoleConstants.ID_CONSOLE_VIEW);
        bottom.addView(IPageLayout.ID_TASK_LIST);
        
        //Add perspective and view shortcuts
        layout.addPerspectiveShortcut(ABSPERSPECTIVE_ID);
        layout.addShowViewShortcut(ABS_DEBUG_VIEW);
        layout.addShowViewShortcut(ABS_DEBUG_VARIABLE_VIEW);
        layout.addShowViewShortcut(NAVIGATOR_ID);
        //add non-abs specific view shortcuts
        
        layout.addShowViewShortcut(IPageLayout.ID_OUTLINE);
        layout.addShowViewShortcut(IConsoleConstants.ID_CONSOLE_VIEW);
        layout.addShowViewShortcut(IPageLayout.ID_TASK_LIST);
        
	}
}
