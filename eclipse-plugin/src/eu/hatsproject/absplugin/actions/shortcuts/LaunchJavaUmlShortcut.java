package eu.hatsproject.absplugin.actions.shortcuts;

import static eu.hatsproject.absplugin.util.Constants.ACTION_START_SDE;

import org.eclipse.debug.ui.ILaunchShortcut;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.ui.IEditorPart;


public class LaunchJavaUmlShortcut extends AbstractLaunchJavaShortcut implements ILaunchShortcut {
	
	@Override
	public void launch(ISelection selection, String mode) {
        launchSelectedFile(selection, ACTION_START_SDE);
	}

	@Override
	public void launch(IEditorPart editor, String mode) {
		launchActiveFile(editor, ACTION_START_SDE);
	}
}
