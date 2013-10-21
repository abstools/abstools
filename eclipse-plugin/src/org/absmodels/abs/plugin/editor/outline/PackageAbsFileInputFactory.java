package org.absmodels.abs.plugin.editor.outline;

import org.absmodels.abs.plugin.util.UtilityFunctions;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.IAdaptable;
import org.eclipse.ui.IElementFactory;
import org.eclipse.ui.IMemento;


/**
 * 
 * An implementation of {@link IElementFactory} to restore
 * {@link PackageAbsFileEditorInput} for viewing .abs file enclosed in an ABS
 * package.
 * 
 * @author woner
 */
public class PackageAbsFileInputFactory implements IElementFactory {

	/**
	 * This factory's ID.
	 */
	static final String ID = "org.abs-models.abs.plugin.editor.outline.PackageAbsFileInputFactory"; //$NON-NLS-1$
	
	/**
	 * Saves the state of the given editor input into the given memento.
	 *
	 * @param memento the storage area for element state
	 * @param input the file editor input
	 */
	static void saveState(IMemento memento, PackageAbsFileEditorInput input) {
		PackageAbsFile file = input.getFile();
		memento.putString(ABS_PACKAGE, file.getParent().getPath());
		memento.putString(ABS_PACKAGE_ENTRY, file.getName());
		IProject project = file.getProject();
		if (project != null) {
			memento.putString(ABS_PROJECT, project.getName());
		}
	}
	
	/**
	 * Tag for the URI string.
	 */
	private static final String ABS_PACKAGE_ENTRY = "abs_entry"; //$NON-NLS-1$
	private static final String ABS_PACKAGE = "abs_package"; //$NON-NLS-1$
	private static final String ABS_PROJECT = "abs_project"; //$NON-NLS-1$
	
	public IAdaptable createElement(IMemento memento) {
		String pak = memento.getString(ABS_PACKAGE);
		String entry = memento.getString(ABS_PACKAGE_ENTRY);
		String name = memento.getString(ABS_PROJECT);
		IProject proj = UtilityFunctions.getAbsProjectFromWorkspace(name);
		return new PackageAbsFileEditorInput(
				UtilityFunctions.getPackageAbsFile(proj , pak,entry));
	}
	
}
