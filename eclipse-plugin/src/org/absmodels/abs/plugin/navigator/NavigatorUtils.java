/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.absmodels.abs.plugin.navigator;

import static org.absmodels.abs.plugin.util.UtilityFunctions.*;

import org.absmodels.abs.plugin.builder.AbsNature;
import org.absmodels.abs.plugin.editor.ABSEditor;
import org.absmodels.abs.plugin.editor.outline.PackageAbsFile;
import org.absmodels.abs.plugin.util.InternalASTNode;
import org.eclipse.core.resources.IProject;
import org.eclipse.jface.viewers.TreePath;
import org.eclipse.jface.viewers.TreeSelection;
import org.eclipse.ui.PartInitException;

import abs.frontend.ast.ModuleDecl;

/**
 * Utility class used by ABS Navigator
 * @author cseise
 *
 */
public abstract class NavigatorUtils {

	private NavigatorUtils(){}
	
	/**
	 * Opens the file in an editor that corresponds to the given
	 * TreeSelection. Only the first element of the selection will be
	 * taken into account.
	 * 
	 * @param ts TreeSelection that is used as a base to find an appropriate editor
	 * @throws PartInitException - if the editor could not be opened for highlighting
	 */
	public static void openEditor(TreeSelection ts) throws PartInitException {
		if (!ts.equals(TreeSelection.EMPTY)) {
			TreePath path = ts.getPaths()[0];
			IProject project = getProject(path);

			if (project != null) {
				if (path.getLastSegment() instanceof InternalASTNode<?>) {
					InternalASTNode<?> node = (InternalASTNode<?>) path.getLastSegment();
					openAndHighlightEditor(node);
				} else if (path.getLastSegment() instanceof ModulePath){
					ModulePath mp = (ModulePath) path.getLastSegment();
					if (mp.hasModuleWithDecls()){
						InternalASTNode<ModuleDecl> moduleDecl = mp.getModuleDecl();
						openAndHighlightEditor(moduleDecl);
					}
				} else if (path.getLastSegment() instanceof PackageAbsFile) {
					openABSEditorForFile((PackageAbsFile) path.getLastSegment());
				}
			}
		}
	}

	private static void openAndHighlightEditor(InternalASTNode<?> node) throws PartInitException {
		ABSEditor editorPart = openABSEditorForFile(node.getFileName(), node.getProject());
		editorPart.highlightInEditor(node, true);
	}	
	
	public static void updateDependencies(TreeSelection ts) {
		IProject project = getProject(ts);
		if (project != null) {
			AbsNature nature = getAbsNature(project);
			nature.initDependencies();
		}
	}
	
	public static IProject getProject(TreeSelection ts) {
		if (!ts.equals(TreeSelection.EMPTY)) {
			TreePath path = ts.getPaths()[0];
			return getProject(path);
		}
		return null;
	}
	
	/**
	 * Retrieves the IProject for a given TreePath that comes from the
	 * ABSContentNavigator
	 * 
	 * @param path
	 *            the corresponding tree path
	 * @return the according IProject or null, if the path does not
	 *         contain an IProject
	 */
	private static IProject getProject(TreePath path) {

		for (int i = 0; i < path.getSegmentCount(); i++) {
			if (path.getSegment(i) instanceof IProject) {
				return ((IProject) path.getSegment(i));
			}
		}
		return null;
	}

	/**
	 * Returns a regex used for finding module structures.
	 * <br/>
	 * <code>
	 * regex = ^(prefix)\.([^\.]+)$
	 * </code>
	 *  
	 * @param prefix prefix to be used in the Regex
	 * @return A Regex for finding module structures
	 */
	public static String buildRegex(String prefix){
		return  "^" + prefix + "\\.[^\\.]+$";
	}
	
	/**
	 * Determines, whether the given module declaration has sub modules.
	 * @param m The module declaration to be checked.
	 * @param nature
	 *            an ABSNature
	 * @return TRUE if the module declaration has submodules starting with
	 *         m.getName()+".", <br/>FALSE if the module declaration does not have
	 *         submodules or nature or m is null.
	 */
	public static boolean hasSubModules(ModuleDecl m, AbsNature nature) {
		if (m != null && nature != null) {
			synchronized (nature.modelLock) {
				for (ModuleDecl md : nature.getCompleteModel().getModuleDecls()) {
					if (md.getName().startsWith(m.getName() + ".")) {
						return true;
					}
				}
			}
		}
		return false;
	}
	
	/**
	 * Wrapper method for {@link #hasSubModules(ModuleDecl, AbsNature)}
	 * @see #hasSubModules(ModuleDecl, AbsNature)
	 * @param m the internal AST node to be checked
	 * @return TRUE if the module declaration has submodules starting with
	 *         m.getName()+".", <br/>FALSE if the module declaration does not have
	 *         submodules or nature or m is null.
	 */
	public static boolean hasSubModules(InternalASTNode<ModuleDecl> m) {
		if (m != null) {
			return hasSubModules(m.getASTNode(), m.getNature());
		}
		return false;
	}
}
