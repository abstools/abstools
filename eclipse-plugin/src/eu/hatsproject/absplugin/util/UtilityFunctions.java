/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package eu.hatsproject.absplugin.util;

import static eu.hatsproject.absplugin.util.Constants.*;

import java.io.*;
import java.net.URI;
import java.net.URISyntaxException;

import org.eclipse.core.filesystem.EFS;
import org.eclipse.core.filesystem.IFileStore;
import org.eclipse.core.filesystem.URIUtil;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IncrementalProjectBuilder;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.QualifiedName;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.preference.IPersistentPreferenceStore;
import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.IDocument;
import org.eclipse.swt.widgets.Display;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.ide.IDE;
import org.eclipse.ui.texteditor.ITextEditor;

import abs.frontend.ast.ASTNode;
import abs.frontend.ast.Annotation;
import abs.frontend.ast.ClassDecl;
import abs.frontend.ast.CompilationUnit;
import abs.frontend.ast.DataConstructorExp;
import abs.frontend.ast.Model;
import abs.frontend.ast.ModuleDecl;
import abs.frontend.parser.ABSPackageFile;
import abs.frontend.parser.SourcePosition;
import beaver.Symbol;
import eu.hatsproject.absplugin.Activator;
import eu.hatsproject.absplugin.builder.AbsNature;
import eu.hatsproject.absplugin.editor.ABSEditor;
import eu.hatsproject.absplugin.editor.outline.ABSContentOutlineConstants.AnnotationType;
import eu.hatsproject.absplugin.editor.outline.PackageAbsFile;
import eu.hatsproject.absplugin.editor.outline.PackageAbsFileEditorInput;
import eu.hatsproject.absplugin.editor.outline.PackageContainer;
import eu.hatsproject.absplugin.editor.outline.PackageEntry;
/**
 * Collection of project-wide utility functions
 * @author cseise, tfischer
 */
public class UtilityFunctions {

	public static class EditorPosition{
		private int linestart;
		private int colstart;
		private int lineend;
		private int colend;
		private IPath path;
		
		public int getLinestart() {
			return linestart;
		}
	
		public int getColstart() {
			return colstart;
		}
	
		public int getLineend() {
			return lineend;
		}
	
		public int getColend() {
			return colend;
		}
		
		public IFile getFile() {
			return ResourcesPlugin.getWorkspace().getRoot().getFileForLocation(path);
		}
	
		public IPath getPath() {
			return path;
		}
	
		public EditorPosition(IPath path, int linestart, int colstart, int lineend, int colend) {
			this.path      = path;
			this.linestart = linestart;
			this.colstart  = colstart;
			this.lineend   = lineend;
			this.colend    = colend;
		}
		
	}

	/**
	 * Checks, whether a ModuleDecl has declarations
	 * @param m
	 * @return TRUE if a module declaration has
	 * <ul>
	 *  <li>Exports</li>
	 *  <li>Imports</li>
	 *  <li>Products</li>
	 *  <li>MainBlock</li>
	 *  <li>ProductLine</li>
	 * </ul>
	 *  declarations,<br/>
	 * FALSE if m is null or the the module declarations
	 */
	public static boolean hasDecls(ModuleDecl m){
		if (m!=null){
			return (m.getNumDecl() > 0) ||
						(m.getNumExport() > 0) ||
						(m.getNumImport() > 1) ||
						(m.getNumProduct() > 0) ||
						(m.hasBlock()) ||
						(m.hasProductLine());
		}else{
			return false;
		}
	} 
	
	/**
	 * Checks whether a class declaration has a <code>[...]</code> annotation
	 * with a specific content
	 * 
	 * @param cd
	 *            The classDecl to be checked
	 * @param type
	 *            the type of the annotation
	 * @return TRUE if cd has the respective annotation, FALSE else
	 */
	public static boolean hasClassAnnotation(ClassDecl cd, AnnotationType type) {

		for (Annotation ant : cd.getAnnotationList()) {
			if (ant.getNumChild() > 0) {

				if (ant.getChild(0) instanceof DataConstructorExp) {
					DataConstructorExp exp = (DataConstructorExp) ant.getChild(0);
					if (exp.getConstructor().equals(type.getAnnotationString())) {
						return true;
					}
				}

			}
		}
		return false;
	}
	
	public static IPreferenceStore getDefaultPreferenceStore(){
		return Activator.getDefault().getPreferenceStore();
	}
	
	/**
	 * Returns an {@link UtilityFunctions.EditorPosition} for the specified {@link ASTNode}
	 * @param node The target ASTNode
	 * @return The EditorPosition of the ASTNode, or null if node is null
	 */
	public static EditorPosition getPosition(ASTNode<?> node){
		if (node != null){
			//Get the start position	
			int start = node.getStart();
			int startLine = Symbol.getLine(start) - 1;
			int startCol = Symbol.getColumn(start) - 1;
			//Get the end position
			int end = node.getEnd();
			int endLine = Symbol.getLine(end) - 1;
			int endCol = Symbol.getColumn(end);
			return new UtilityFunctions.EditorPosition(null,startLine,startCol,endLine,endCol);
		}
		
		return null;	
	}
	
	/**
	 * deletes a file or directory. In case of a directory, it is deleted including all contents.
	 */
	public static boolean deleteRecursive(File dir){
		if (dir.isDirectory()) {
			String[] children = dir.list();
			for (int i=0; i<children.length; i++) {
				boolean success = deleteRecursive(new File(dir, children[i]));
				if (!success) {
					return false;
				}
			}
		}
		// The directory is now empty so delete it 
		return dir.delete(); 
	}
	
	public static boolean isIdentifierChar(char c){
		return Character.isLetterOrDigit(c) || c=='_';
	}
	
	/** Get String value of a persistent property.
	 * @param name QualifiedName of the property which should be retrieved
	 * @param project project where the property is stored
	 * @return String value of the stored property
	 * @throws CoreException
	 */
	public static String getPersistentProperty(QualifiedName name, IProject project) throws CoreException {
		return project.getPersistentProperty(name);
	}
	
	/** Get boolean value of a persistent property.
	 * @param name QualifiedName of the property which should be retrieved
	 * @param project project where the property is stored
	 * @return boolean value of the stored property
	 * @throws CoreException
	 */
	public static boolean getBooleanProperty(QualifiedName name, IProject project) throws CoreException {
		return Boolean.parseBoolean(getPersistentProperty(name, project));
	}
	
	/**
	 * Set String value of a persistent property
	 * @param name
	 * @param project
	 * @param value
	 * @throws CoreException
	 */
	public static void setProperty(QualifiedName name, IProject project, String value) throws CoreException {
		project.setPersistentProperty(name, value);
	}
	
	/**
	 * Set boolean value of a boolean persistent property.
	 * @param name
	 * @param project
	 * @param value
	 * @throws CoreException
	 */
	public static void setProperty(QualifiedName name, IProject project, boolean value) throws CoreException {
		setProperty(name, project, String.valueOf(value));
	}
	
	/**
	 * Copies a file to a different location
	 * @param source
	 * @param target
	 * @throws IOException Is thrown, if the source file does not exist
	 */
	public static void copyFile(File source, File target) throws IOException {
		InputStream inputStream   = new FileInputStream(source);
		OutputStream outputStream = new FileOutputStream(target);
		
		try {
			copyFile(inputStream, outputStream);
		} finally{
			if(inputStream != null){
				inputStream.close();
			}
			if(outputStream != null){
				outputStream.close();
			}
		}
	}
	
	/**
	 * Copies an input stream into an output stream
	 * @param inputStream
	 * @param outputStream
	 * @throws IOException
	 */
	public static void copyFile(InputStream inputStream, OutputStream outputStream) throws IOException{
		int length;
		byte[] buffer = new byte[4096];
		while((length = inputStream.read(buffer))!= -1 ){
			outputStream.write(buffer,0 , length);
		}
	}
	
	
	/**
	 * Writes output of a process in a StringBuffer. Returns true if the process terminated without errors (output was copied
	 * from output stream), false if the process encountered an error (output was copied from error stream)
	 * @param p
	 * @param output
	 * @return false if the process encountered an error, true otherwise
	 * @throws IOException
	 * @throws InterruptedException
	 */
	public static boolean getProcessOutput(Process p, StringBuffer output) throws IOException, InterruptedException {
		return getProcessOutput(p, output, output);
	}	
	
	public static boolean getProcessOutput(Process p, Appendable output, Appendable error) throws IOException, InterruptedException {
		String s;
		boolean success = true;
		BufferedReader in = new BufferedReader(new InputStreamReader(p.getInputStream()));
		BufferedReader er = new BufferedReader(new InputStreamReader(p.getErrorStream()));

		//Wait until some output is available
		while(p.getInputStream().available() <= 0 && p.getErrorStream().available() <= 0){
			Thread.sleep(10);
		}
		
		//If output is available append it to the output string
		while(p.getInputStream().available() > 0){	
			while((s = in.readLine()) != null) {
				output.append(s);
				output.append('\n');
		    }
		}
			
		//If Maude encountered an error, append this to the output string
		while(p.getErrorStream().available() > 0){
			while((s = er.readLine()) != null) {
				error.append(s);
				error.append('\n');
				success = false;
		    }
		}
		return success;
	}
	
	/**
	 * Returns a super-ordinated node of a specific class from an
	 * <code>{@link ASTNode}</code>
	 * 
	 * @param node
	 *            The base ASTNode (the start node of the lookup)
	 * @param c
	 *            the class of the super-ordinated node that should be returned
	 * @return The super-ordinated node of class <code>c</code> of the
	 *         <code>ASTNode</code> or null if no such node could be found.
	 */
	public static ASTNode<?> getSuperOfASTNode(ASTNode<?> node,
			Class<? extends ASTNode<?>> c) {
		ASTNode<?> tmpNode = node;
		while (tmpNode != null && (!c.isInstance(tmpNode))) {
			tmpNode = tmpNode.getParent();
		}

		return tmpNode;
	}
	
	/**
	 * Calls <code>getSuperOfASTNode(node, CompilationUnit.class)</code> and
	 * returns its results
	 * 
	 * @see #getSuperOfASTNode(ASTNode, Class)
	 */
	public static CompilationUnit getCompilationUnitOfASTNode(ASTNode<?> node) {
		return (CompilationUnit) getSuperOfASTNode(node, CompilationUnit.class);
	}
	
	/**
	 * Returns the super-ordinated Model node from an
	 * <code>{@link ASTNode}</code>
	 * 
	 * @param element
	 *            The base ASTNode (the start node of the lookup)
	 * @return The super-ordinated node of class <code>Model</code> of the
	 *         <code>ASTNode</code> or null if no such node could be found.
	 */	
	public static Model getModelFromASTNode(ASTNode<?> element) {
	    return (Model) getSuperOfASTNode((ASTNode<?>) element, Model.class);
	}
	
	/**
	 * Looks for the first project with an ABSNature that contains this file
	 * 
	 * @return The ABSNature for a file with the given IPath. If the file is not in a project with an ABS nature null will be returned.
	 * @deprecated This method does not work with linked files that are members of multiple projects.
	 *  
	 */
	public static AbsNature getNatureForPath(IPath path) {
		IFile[] filesForLocation = ResourcesPlugin.getWorkspace().getRoot().findFilesForLocationURI(URIUtil.toURI(path));

		if (filesForLocation.length > 0) {
 
			for (IFile file : filesForLocation) {
				IProject project = file.getProject();
				try {
					if (project.isAccessible() && project.hasNature(Constants.NATURE_ID)) {
						return UtilityFunctions.getAbsNature(project);
					}
				} catch (CoreException e) {
					e.printStackTrace();
				}
			}
		}
		return null;
	}
	
	/**
	 * Wrapper method for {@link #getAbsNature(IProject)}
	 * 
	 * @param file
	 * @see #getAbsNature(IProject)
	 * @return The corresponding AbsNature of the file is returned, or null if
	 *         file is null
	 */
	public static AbsNature getAbsNature(IFile file){
		if(file==null)
			return null;
		return getAbsNature(file.getProject());
	}

	/**
	 * Returns the AbsNature of the corresponding project
	 * 
	 * @param project
	 * @return the AbsNature of the project or null, not accessible, if project
	 *         is null or a CoreException occurs
	 */
	public static AbsNature getAbsNature(IProject project){
		try {
			if (project != null && project.isAccessible()){
				return (AbsNature)project.getNature(Constants.NATURE_ID);
			}
		} catch (CoreException e) {
			standardExceptionHandling(e);
			return null;
		}
		
		return null;
	}
	
	/**
	 * Convenience method showing an error dialog with the given error message.
	 */
	public static void showErrorMessage(final String errorMessage){
		MessageDialog.openError(Display.getCurrent().getActiveShell(), "Error", errorMessage);
	}

	public static void syncPreferenceStore(IPersistentPreferenceStore prefstore) {
		try {
			prefstore.save();
		} catch (IOException e) {
			standardExceptionHandling(e);
		}
	}

	public static ASTNode<?> getASTNodeOfOffset(IDocument doc,
			CompilationUnit compunit, int offset) throws BadLocationException {
		int selline = doc.getLineOfOffset(offset) + 1;
		int lineoff = doc.getLineOffset(selline - 1);
		int selcol = offset - lineoff + 1;
		SourcePosition sourcepos = SourcePosition.findPosition(compunit, selline, selcol);
		if(sourcepos==null)
		    return compunit;
		ASTNode<?> node = sourcepos.getContextNode();
		return node;
	}
	
	/**
	 * Creates the Maude command to make a partial run with a given step number and quit afterwards
	 * @param steps Number of steps to be done. If negative, no steps will be taken at all
	 * @return The assembled Maude command
	 */
	public static String getMaudeCommand (int steps){
		return MAUDE_COMMAND1 + '[' + (steps < 0 ? 0 : steps) + ']' + MAUDE_COMMAND2 + MAUDE_COMMAND_QUIT;
	}

	/**
	 * Highlights the given {@link ASTNode} in the editor.<br/>
	 * Only one ASTNode at a time can be highlighted (This is a restriction of {@link ITextEditor}}).<br/><br/>
     *
	 * @param edit The target editor
	 * @param node The node that should be highlighted.
	 */
	public static void highlightInEditor(ITextEditor edit, ASTNode<?> node) {
	
		EditorPosition pos = getPosition(node);
	
		if (pos != null) {
			IDocument doc = edit.getDocumentProvider().getDocument(
					edit.getEditorInput());
	
			try {
				/* Calculate the position on the editor by retrieving the char offset
				 * of the position line and the target column in this line.
				 */
				int startOff = doc.getLineOffset(pos.getLinestart()) + pos.getColstart();
				int endOff = doc.getLineOffset(pos.getLineend()) + pos.getColend();
	
				edit.setHighlightRange(startOff, endOff - startOff, true);
			} catch (BadLocationException e) {
				/*
				 * Should not be thrown, as an ASTNode in a document must have a
				 * specific location.
				 */
			}
	
		}
	}
	
	/**
	 * Highlights the a given {@link InternalASTNode} in the editor.<br/>
	 * @see #highlightInEditor(ITextEditor, ASTNode)
	 */
	public static void highlightInEditor(ITextEditor edit, InternalASTNode<?> node) {
		if (node != null){
			highlightInEditor(edit, node.getASTNode());
		}
	}
	
	public static ABSEditor openABSEditorForFile(PackageAbsFile file){
		try {
			return (ABSEditor) IDE.openEditor(getActiveWorkbenchPage(),
					new PackageAbsFileEditorInput(file), Constants.EDITOR_ID);
		} catch (PartInitException e) {
			
		}
		return null;
	}
	
	public static PackageAbsFile getPackageAbsFile(String pak, String entry) {
		return getPackageAbsFile(pak,entry,null);
	}

	/**
	 * A convenient method to reconstruct a {@link PackageAbsFile} from the absolute
	 * path to the ABS package and the name to the specific entry in the package.
	 * @param pak
	 * @param entry
	 * @param name name of the project this ABS package is from. 
	 * @return 
	 */
	public static PackageAbsFile getPackageAbsFile(String pak, String entry,
			String name) {
		File file = new File(pak);
		try {
			if (new ABSPackageFile(file).isABSPackage()) {
				
				PackageContainer container = null;
				PackageEntry pentry = null;
				if (name != null) {
					IProject proj = getAbsProjectFromWorkspace(name);
					AbsNature nature = getAbsNature(proj);
					for (PackageEntry e : nature.getPackages().getPackages()) {
						if (e.getPath().equals(file.getAbsolutePath())) {
							pentry = e;
							break;
						}
					}
					if (pentry == null) {
						container = new PackageContainer();
						container.setProject(proj);
					}
				}
				
				if (pentry == null) {
					pentry = new PackageEntry(
								container, 
								file.getName(), 
								file.getAbsolutePath(), 
								true);
				}
				
				return new PackageAbsFile(pentry, entry);
			}
		} catch (IOException e) {
			
		}
		return null;
	}
	
	private static IProject getAbsProjectFromWorkspace(String name) {
		IProject p = ResourcesPlugin.getWorkspace().getRoot().getProject(name);
		if (getAbsNature(p) != null) {
			return p;
		}
		return null;
	}
	
	/**
	 * Get the current active {@link IWorkbenchPage}.
	 * @return the current active {@link IWorkbenchPage}.
	 */
	private static IWorkbenchPage getActiveWorkbenchPage() {
		return PlatformUI.getWorkbench().getActiveWorkbenchWindow().getActivePage();
	}
	
	public static ABSEditor openABSEditorForFile(IPath path) {
		return openABSEditorForFile(path, null);
	}
	
	public static ABSEditor openABSEditorForFile(IPath path, IProject project){
		IFileStore fileStore = EFS.getLocalFileSystem().getStore(path);
		if (!fileStore.fetchInfo().isDirectory() && fileStore.fetchInfo().exists()) {
		    try {
		        return (ABSEditor)IDE.openEditorOnFileStore(getActiveWorkbenchPage(), fileStore);
		    } catch (PartInitException e) {
		    	standardExceptionHandling(e);
		    }
		} else if (path.segment(0).equals("jar:file:")) {
			// a jar file
			try {
				String parts = new URI(path.toString()).getRawSchemeSpecificPart();
				String pak = new URI(parts.split("!/")[0]).getSchemeSpecificPart();
				String entry = parts.split("!/")[1];
				String pname = (project != null) ? project.getName() : null;
				return openABSEditorForFile(getPackageAbsFile(pak, entry, pname));
			} catch (URISyntaxException e) {
				standardExceptionHandling(e);
			}
		}
		return null;
	}

	/**
	 * Returns the IPath of a ModuleDecl's compilation unit
	 * @param mDecl 
	 * @return IPath of a ModuleDecl's compilation unit
	 */
	public static IPath getPathOfModuleDecl(InternalASTNode<ModuleDecl> mDecl) {
		if (mDecl != null){
			IPath path = new Path(mDecl.getASTNode().getCompilationUnit().getFileName());
			return path;
		}
		return null;
	}
	
	public static boolean saveEditors(IProject project, boolean withConfirmation){
		IEditorPart[] dirtyEditors = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getActivePage().getDirtyEditors();
		if(dirtyEditors.length>0 && withConfirmation){
			boolean doit = MessageDialog.openQuestion(Display.getDefault().getActiveShell(), "Save modified files", "Should the modified files be saved?");
			if(!doit)
				return false;
		}
		boolean allsaved = true;
		for(IEditorPart iep : dirtyEditors){
			IFile editorFile = (IFile)iep.getEditorInput().getAdapter(IFile.class);
			if(editorFile!=null && editorFile.getProject().equals(project)){
				allsaved = allsaved && PlatformUI.getWorkbench().getActiveWorkbenchWindow().getActivePage().saveEditor(iep, false);
			}
		}
		try {
			project.build(IncrementalProjectBuilder.INCREMENTAL_BUILD, null);
		} catch (CoreException e) {
			standardExceptionHandling(e);
		}
		return allsaved;
	}
	
	public static boolean saveEditor(IFile file, boolean withConfirmation){
		IEditorPart[] dirtyEditors = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getActivePage().getDirtyEditors();
		for(IEditorPart iep : dirtyEditors){
			IFile editorFile = (IFile)iep.getEditorInput().getAdapter(IFile.class);
			if(editorFile!=null && editorFile.equals(file)){
				return PlatformUI.getWorkbench().getActiveWorkbenchWindow().getActivePage().saveEditor(iep, withConfirmation);
			}
		}
		return true; //file was not dirty
	}

	public static IFile getFileOfModuleDecl(ModuleDecl m) {
		if (m != null) {
			CompilationUnit compilationUnitOfASTNode = getCompilationUnitOfASTNode(m);
			Path path = new Path(compilationUnitOfASTNode.getFileName());
			path.makeRelative();
			IFile file = ResourcesPlugin.getWorkspace().getRoot().getFileForLocation(path);
			return file;
		}
		return null;
	}
	
	public static boolean hasABSFileExtension(IFile file){
		if (file != null){
			return ABS_FILE_EXTENSION.equals(file.getFileExtension());
		}
		
		return false;
	}
	
	public static boolean isABSSourceFile(IResource file){
	    if (file != null && file instanceof IFile)
	       return hasABSFileExtension((IFile)file);
	    return false;
	}

	public static boolean isABSFile(IResource file){
		if (file != null && file instanceof IFile){
			return hasABSFileExtension((IFile)file) || isABSPackage((IFile)file);
		}
		
		return false;
	}	
	
	public static boolean isABSPackage(File file) throws IOException {
		return file.exists() && new ABSPackageFile(file).isABSPackage();
	}

	// assumes file != null
	public static boolean isABSPackage(IFile file) {
		if (! "jar".equals(file.getFileExtension()))
			return false;

		try {
			return isABSPackage(file.getLocation().toFile());
		} catch (IOException e) {
			standardExceptionHandling(e);
			return false;
		}
	}

   /**
	 * Always log exceptions in the Error Log.
	 * @param e
	 */
	public static void standardExceptionHandling(Exception e){
		Activator.logException(e);
	}
	
}
