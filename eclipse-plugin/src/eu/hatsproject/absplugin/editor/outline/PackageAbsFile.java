package eu.hatsproject.absplugin.editor.outline;

import org.eclipse.core.resources.IProject;

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
	private final String path;
	
	public PackageAbsFile(PackageEntry parent, String name) {
		this.parent = parent;
		this.name = name;
		this.extension = name.substring(name.lastIndexOf(".")+1,name.length());
		this.path = "jar:file:"+parent.getPath()+"!/"+name;
	}

	public PackageEntry getParent() {
		return parent;
	}

	public String getName() {
		return name;
	}

	public IProject getProject() {
		return parent.getPackageContainer().getProject();
	}

	public String getFileExtension() {
		return extension;
	}

	public String getAbsoluteFilePath() {
		return path;
	}
	
}
