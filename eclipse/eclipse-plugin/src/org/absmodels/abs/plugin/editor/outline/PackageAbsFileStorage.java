package org.absmodels.abs.plugin.editor.outline;

import java.io.InputStream;

import org.eclipse.core.resources.IStorage;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.PlatformObject;

public class PackageAbsFileStorage extends PlatformObject implements IStorage {

	private PackageAbsFileStore fileStore;
	private PackageAbsFile file;

	public PackageAbsFileStorage(PackageAbsFile file, PackageAbsFileStore fileStore) {
		this.file = file;
		this.fileStore = fileStore;
		
	}


	@Override
	public InputStream getContents() throws CoreException {
		return fileStore.openInputStream(0, null);
	}

	@Override
	public IPath getFullPath() {
		return new Path(file.getAbsoluteFilePath());
	}

	@Override
	public String getName() {
		return fileStore.getName();
	}

	@Override
	public boolean isReadOnly() {
		return true;
	}

	
	
}
