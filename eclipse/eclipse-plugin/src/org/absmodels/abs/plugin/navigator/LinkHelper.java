/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.absmodels.abs.plugin.navigator;

import java.util.ArrayList;

import static org.absmodels.abs.plugin.util.UtilityFunctions.getAbsNature;

import org.absmodels.abs.plugin.builder.AbsNature;
import org.absmodels.abs.plugin.util.InternalASTNode;
import org.absmodels.abs.plugin.util.UtilityFunctions;
import org.absmodels.abs.plugin.util.UtilityFunctions.EditorPosition;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.jface.text.ITextSelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.TreePath;
import org.eclipse.jface.viewers.TreeSelection;
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.navigator.ILinkHelper;
import org.eclipse.ui.part.FileEditorInput;
import org.eclipse.ui.texteditor.ITextEditor;

import abs.frontend.ast.CompilationUnit;
import abs.frontend.ast.ModuleDecl;

/**
 * Class for providing 'link with editor support' for the ABSNavigator 
 * @author cseise
 *
 */
public class LinkHelper implements ILinkHelper {

	/**
	 * {@inheritDoc}
	 */
	@Override
	public IStructuredSelection findSelection(IEditorInput anInput) {
		if (anInput instanceof FileEditorInput) {
			FileEditorInput fileInput = (FileEditorInput) anInput;
			
			IProject project = getProject(fileInput);
			AbsNature nature = getAbsNature(project);

			//If we did not get back an ABS nature, do nothing.
			if (nature == null) {
				return null;
			}	
			
			ITextEditor editor = UtilityFunctions.openABSEditorForFile(fileInput.getPath(), project);
		
			IFile file = fileInput.getFile();
			ModuleDecl md = getModuleDeclAtCurrentCursor(file, nature, editor);

			return buildTreeSelection(nature, project, new InternalASTNode<ModuleDecl>(md,nature));
		}
		return null;
	}

	private ModuleDecl getModuleDeclAtCurrentCursor(IFile file, AbsNature nature, ITextEditor editor) {
		ModuleDecl md = null;
		
		synchronized (nature.modelLock) {
			if (nature.getCompilationUnit(file).getNumModuleDecl() > 0){
				//get start line of editor selection
				ITextSelection selection = (ITextSelection) editor.getSelectionProvider().getSelection();
				int startLine = selection.getStartLine();
				
				CompilationUnit cu = nature.getCompilationUnit(file);
				
				//find the module which the selection is located in...
				for (ModuleDecl m : cu.getModuleDecls()){
					EditorPosition position = UtilityFunctions.getPosition(m);
					int moduleStartLine = position.getLinestart();
					int moduleEndLine = position.getLineend();
					if (startLine >= moduleStartLine && startLine <= moduleEndLine){
						md = m;
						break;
					}
				}
				
				//if no module was found or no selection was made, take the first one in the compilation unit as fallback
				if (md == null){
					md = cu.getModuleDecl(0);
				}
			}
		}
		return md;
	}

	/**
	 * Build up a {@link TreeSelection} suitable for the ABSNavigator from the
	 * given component
	 * 
	 * @param project
	 *            IProject The according project of the ModuleDecl.
	 * @param md
	 *            ModuleDecl The target ModuleDecl to highlight.
	 * @return A TreeSelection including a {@link TreePath} from project to md
	 *         or null if md or project is null, or if no ABSnature can be
	 *         retrieved from project
	 */
	private static TreeSelection buildTreeSelection(AbsNature nature, IProject project, InternalASTNode<ModuleDecl> md) {
		assert nature != null;
		assert nature == md.getNature(); // TODO: if this holds (as it should), remove argument
		if (project != null && md != null) {
			// Split the module's name and return the module hierarchy
			ArrayList<ModulePath> paths = md.getParentHierarchyForModuleDecl();

			ArrayList<Object> arli = new ArrayList<Object>();
			arli.add(project);

			if (paths.size() > 0){
				ModulePath lastElement = paths.get(paths.size()-1);
				arli.addAll(paths);
				if (!(lastElement.hasModule() && lastElement.getModulePath().equals(md.getASTNode().getName()))){
					arli.add(md);
				}

			}else{
				if (NavigatorUtils.hasSubModules(md)){
					arli.add(new ModulePath(nature, md.getASTNode().getName()));
				}else{
					arli.add(md);
				}
			}
			TreePath path = new TreePath(arli.toArray());
			TreeSelection ts = new TreeSelection(new TreePath[] { path });
			return ts;
		}
		return null;
	}
	
	private IProject getProject(FileEditorInput anInput){	
		return anInput.getFile().getProject();
	}

	@Override
	public void activateEditor(IWorkbenchPage aPage, IStructuredSelection aSelection) {
		if (aSelection instanceof TreeSelection) {
			TreeSelection ts = (TreeSelection) aSelection;
			try {
				NavigatorUtils.openEditor(ts);
			} catch (PartInitException e) {
				UtilityFunctions.showErrorMessage("Fatal error in ABS Navigator View:\n Could not open the editor connected to the selected tree element.");
			}
		}
	}

}
