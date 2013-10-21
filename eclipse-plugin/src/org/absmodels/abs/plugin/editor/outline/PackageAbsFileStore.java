package org.absmodels.abs.plugin.editor.outline;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;

import org.absmodels.abs.plugin.Activator;
import org.eclipse.core.internal.resources.VirtualFileStore;
import org.eclipse.core.runtime.IProgressMonitor;


import abs.frontend.parser.ABSPackageFile;

@SuppressWarnings("restriction")
public class PackageAbsFileStore extends VirtualFileStore {

	private final PackageAbsFile file;
	
	public PackageAbsFileStore(PackageAbsFile file) {
		super(file.getURI());
		this.file = file;
	}
	
	public String getName() {
		return file.getName();
	}

	@Override
	public InputStream openInputStream(int options, IProgressMonitor monitor) {
		try {
			ABSPackageFile pak = new ABSPackageFile(new File(file.getParent().getPath()));
			return pak.getInputStream(pak.getJarEntry(file.getName()));
		} catch (IOException e) {
			Activator.logException(e);
		}
		return null;
	}

}
