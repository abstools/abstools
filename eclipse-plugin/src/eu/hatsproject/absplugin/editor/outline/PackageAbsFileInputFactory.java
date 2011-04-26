package eu.hatsproject.absplugin.editor.outline;

import org.eclipse.core.runtime.IAdaptable;
import org.eclipse.ui.IElementFactory;
import org.eclipse.ui.IMemento;

import eu.hatsproject.absplugin.util.UtilityFunctions;

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
	static final String ID = "eu.hatsproject.absplugin.editor.outline.PackageAbsFileInputFactory"; //$NON-NLS-1$
	
	/**
	 * Saves the state of the given editor input into the given memento.
	 *
	 * @param memento the storage area for element state
	 * @param input the file editor input
	 */
	static void saveState(IMemento memento, PackageAbsFileEditorInput input) {
		memento.putString(ABS_PACKAGE, input.getFile().getParent().getPath());
		memento.putString(ABS_PACKAGE_ENTRY, input.getFile().getName());
	}
	
	/**
	 * Tag for the URI string.
	 */
	private static final String ABS_PACKAGE_ENTRY = "abs_entry"; //$NON-NLS-1$
	private static final String ABS_PACKAGE = "abs_package"; //$NON-NLS-1$
	
	public IAdaptable createElement(IMemento memento) {
		String pak = memento.getString(ABS_PACKAGE);
		String entry = memento.getString(ABS_PACKAGE_ENTRY);
		return new PackageAbsFileEditorInput(UtilityFunctions.getPackageAbsFile(pak,entry));
	}
	
}
