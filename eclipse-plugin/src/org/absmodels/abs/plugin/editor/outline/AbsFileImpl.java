package org.absmodels.abs.plugin.editor.outline;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;

/**
 * A wrapper class over {@link IFile} representing an {@link AbsFile}
 * @author pwong
 *
 */
public class AbsFileImpl implements AbsFile {

	private final IFile file;
	
	public AbsFileImpl(IFile file) {
		this.file = file;
	}
	
	public String getFileExtension() {
		return file.getFileExtension();
	}

	public IProject getProject() {
		return file.getProject();
	}

	public String getAbsoluteFilePath() {
		return file.getLocation().toFile().getAbsolutePath();
	}

}
