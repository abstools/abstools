package org.absmodels.abs.plugin.editor.outline;

import java.io.File;
import java.net.URI;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.URIUtil;

/**
 * A lightweight class recording a single ABS file entry in a ABS package.
 * It implements {@link AbsFile}
 * 
 * @author pwong
 *
 */
public class PackageAbsFile implements AbsFile {

	private final PackageEntry parent;
	private final String name;
	private final String extension;
	private final URI uri;
	
	public PackageAbsFile(PackageEntry parent, String name) {
		this.parent = parent;
		this.name = name;
		this.extension = name.substring(name.lastIndexOf(".")+1,name.length());
		this.uri = URIUtil.toJarURI(new File(parent.getPath()).toURI(),new Path(name));
	}

	public PackageEntry getParent() {
		return parent;
	}

	public String getName() {
		return name;
	}

	public IProject getProject() {
		PackageContainer container = parent.getPackageContainer();
		if (container != null) {
			return container.getProject();
		}
		return null;
	}

	public String getFileExtension() {
		return extension;
	}

	public String getAbsoluteFilePath() {
		return uri.toString();
	}

	public URI getURI() {
		return uri;
	}
	
}
