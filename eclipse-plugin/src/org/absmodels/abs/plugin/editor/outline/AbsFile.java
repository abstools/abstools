package org.absmodels.abs.plugin.editor.outline;

import org.eclipse.core.resources.IProject;

/**
 * An interface representing an ABS file (.abs)
 * @author pwong
 *
 */
public interface AbsFile {
	String getFileExtension();
	IProject getProject();
	String getAbsoluteFilePath();
}
