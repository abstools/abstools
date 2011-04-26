package eu.hatsproject.absplugin.editor.outline;

import org.eclipse.ui.IMemento;
import org.eclipse.ui.ide.FileStoreEditorInput;

/**
 * 
 * An implementation of {@link FileStoreEditorInput} displaying
 * the .abs file enclosed in a ABS package
 * 
 * @author woner
 */
public class PackageAbsFileEditorInput extends FileStoreEditorInput {

	private final PackageAbsFile file;
	
	public PackageAbsFileEditorInput(PackageAbsFile file) {
		super(new PackageAbsFileStore(file));
		this.file = file;
	}
	
	public String getFactoryId() {
		return PackageAbsFileInputFactory.ID;
	}
	
	public PackageAbsFile getFile() {
		return file;
	}

	public void saveState(IMemento memento) {
		PackageAbsFileInputFactory.saveState(memento, this);
	}

}
