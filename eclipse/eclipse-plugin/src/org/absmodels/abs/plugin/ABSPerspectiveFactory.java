/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.absmodels.abs.plugin;

import static org.absmodels.abs.plugin.util.Constants.ABSDEBUGPERSPECTIVE_ID;
import static org.absmodels.abs.plugin.util.Constants.NAVIGATOR_ID;

import org.eclipse.ui.IFolderLayout;
import org.eclipse.ui.IPageLayout;
import org.eclipse.ui.IPerspectiveFactory;
import org.eclipse.ui.console.IConsoleConstants;

public class ABSPerspectiveFactory implements IPerspectiveFactory {
	@Override
	public void createInitialLayout(IPageLayout layout) {
        String editorArea = layout.getEditorArea();
		//String editorArea = "eu.hatsproject.abs.Abs";

        IFolderLayout left = layout.createFolder("left", IPageLayout.LEFT, 0.20f, editorArea);
        IFolderLayout leftTop = layout.createFolder("leftTop", IPageLayout.BOTTOM, 0.5f, "left");
        IFolderLayout bottomRight = layout.createFolder("bottomRight", IPageLayout.BOTTOM, 0.75f, editorArea);
        IFolderLayout topRight = layout.createFolder("topRight", IPageLayout.RIGHT, 0.80f, editorArea);

        left.addView(IPageLayout.ID_PROJECT_EXPLORER);
        left.addPlaceholder(IPageLayout.ID_BOOKMARKS);
        
        //Adding ABS module explorer
        leftTop.addView(NAVIGATOR_ID);
        
        bottomRight.addView(IConsoleConstants.ID_CONSOLE_VIEW);
        bottomRight.addView(IPageLayout.ID_TASK_LIST);
        bottomRight.addView(IPageLayout.ID_PROBLEM_VIEW);
        
        topRight.addView(IPageLayout.ID_OUTLINE);
        
        layout.addNewWizardShortcut("org.abs-models.abs.plugin.abswizard");
        layout.addNewWizardShortcut("org.abs-models.abs.plugin.absfilewizard");
        
        //add perspective and view shortcuts
        layout.addPerspectiveShortcut(ABSDEBUGPERSPECTIVE_ID);
        layout.addShowViewShortcut(NAVIGATOR_ID);
        //add non-abs specific view shortcuts
        layout.addShowViewShortcut(IPageLayout.ID_TASK_LIST);
        layout.addShowViewShortcut(IPageLayout.ID_PROBLEM_VIEW);
        layout.addShowViewShortcut(IConsoleConstants.ID_CONSOLE_VIEW);
        layout.addShowViewShortcut(IPageLayout.ID_OUTLINE);
        layout.addShowViewShortcut(IPageLayout.ID_PROJECT_EXPLORER);
	}
}
