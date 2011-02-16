package eu.hatsproject.absplugin.actions.shortcuts;

import static eu.hatsproject.absplugin.util.Constants.ACTION_DEBUG_ID;

import org.eclipse.debug.ui.ILaunchShortcut;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.ui.IEditorPart;

public class LaunchJavaShortcut extends AbstractLaunchJavaShortcut implements ILaunchShortcut {

	@Override
	public void launch(ISelection selection, String mode) {
        launchSelectedFile(selection, ACTION_DEBUG_ID, true);
	}

	@Override
	public void launch(IEditorPart editor, String mode) {
		launchActiveFile(editor, ACTION_DEBUG_ID, true);
	}
}
