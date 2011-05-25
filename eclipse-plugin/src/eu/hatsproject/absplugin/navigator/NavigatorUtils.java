/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package eu.hatsproject.absplugin.navigator;

import static eu.hatsproject.absplugin.util.UtilityFunctions.getAbsNature;
import static eu.hatsproject.absplugin.util.UtilityFunctions.getCompilationUnitOfASTNode;
import static eu.hatsproject.absplugin.util.UtilityFunctions.highlightInEditor;
import static eu.hatsproject.absplugin.util.UtilityFunctions.openABSEditorForFile;

import java.util.ArrayList;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
import org.eclipse.jface.viewers.TreePath;
import org.eclipse.jface.viewers.TreeSelection;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.editors.text.TextEditor;

import abs.frontend.ast.CompilationUnit;
import abs.frontend.ast.ModuleDecl;
import eu.hatsproject.absplugin.builder.AbsNature;
import eu.hatsproject.absplugin.editor.outline.PackageAbsFile;
import eu.hatsproject.absplugin.util.InternalASTNode;

/**
 * Utility class used by ABS Navigator
 * @author cseise
 *
 */
public class NavigatorUtils {

	private NavigatorUtils(){
		
	}
	
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
		IEditorPart editorPart = openABSEditorForFile(getFileName(node));
		if (editorPart instanceof TextEditor){
			TextEditor texteditor = ((TextEditor) editorPart);
			highlightInEditor(texteditor, node);
		}
	}	
	
	public static void updateDependencies(TreeSelection ts) {
		if (!ts.equals(TreeSelection.EMPTY)) {
			TreePath path = ts.getPaths()[0];
			IProject project = getProject(path);
			if (project != null) {
				AbsNature nature = getAbsNature(project);
				nature.initDependencies();
			}
		}
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
	 * Retrieves the Path of an ASTNode's Compilation Unit
	 * 
	 * @param node
	 * @return The path of the compilation unit or null if the
	 *         compilation unit could not retrieved,
	 */
	private static IPath getFileName(InternalASTNode<?> node) {
		CompilationUnit cu = getCompilationUnitOfASTNode(node.getASTNode());
		if (cu != null) {
			return new Path(cu.getFileName());
		} else {
			return null;
		}
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
	 * Gives the module hierarchy for a given ModuleDecl
	 * 
	 * @param md
	 *            The target ModuleDecl
	 * @param nature
	 *            The AbsNature connected to this ModuleDecl
	 * @return An ArrayList of ModulePaths. This List will be empty if the
	 *         ModuleDecl is null
	 */
	public static ArrayList<ModulePath> getParentHierarchyForModuleDecl(ModuleDecl md, AbsNature nature) {
		ArrayList<ModulePath> hierarchy = new ArrayList<ModulePath>();
	
		if (md != null) {
			String moduleName = md.getName();
			String[] split = moduleName.split("\\.");
	
			String work = "";
	
			for (int i = 0; i < split.length - 1; i++) {
				work = work + split[i];
				ModulePath path = new ModulePath(nature, work);
				hierarchy.add(path);
				work = work + ".";
	
			}
		}
	
		return hierarchy;
	
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
