package org.absmodels.abs.plugin.editor.outline;

import org.eclipse.core.resources.IStorage;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.Platform;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.ui.IMemento;
import org.eclipse.ui.IPersistableElement;
import org.eclipse.ui.IStorageEditorInput;
import org.eclipse.ui.ide.FileStoreEditorInput;

/**
 * 
 * An implementation of {@link FileStoreEditorInput} displaying
 * the .abs file enclosed in a ABS package
 * 
 * @author woner
 */
public class PackageAbsFileEditorInput implements IStorageEditorInput {

	private final PackageAbsFile file;
	private PackageAbsFileStore fileStore;
	
	public PackageAbsFileEditorInput(PackageAbsFile file) {
		this.fileStore = new PackageAbsFileStore(file);
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

	@Override
	public IStorage getStorage() throws CoreException {
		return new PackageAbsFileStorage(file, fileStore);
	}

	@Override
	public boolean exists() {
		return fileStore.fetchInfo().exists();
	}

	@Override
	public ImageDescriptor getImageDescriptor() {
		return null;
	}

	@Override
	public String getName() {
		return fileStore.getName();
	}

	@Override
	public IPersistableElement getPersistable() {
		return null;
	}

	@Override
	public String getToolTipText() {
		return fileStore.toString();
	}

	@Override
	public Object getAdapter(@SuppressWarnings("rawtypes") Class adapter) {
		return Platform.getAdapterManager().getAdapter(this, adapter);
	}
	
	public boolean equals(Object o) {
		if (o == this)
			return true;

		if (o instanceof PackageAbsFileEditorInput) {
			PackageAbsFileEditorInput input = (PackageAbsFileEditorInput) o;
			return fileStore.equals(input.fileStore);
		}

		return false;
	}

	public int hashCode() {
		return fileStore.hashCode();
	}

}
