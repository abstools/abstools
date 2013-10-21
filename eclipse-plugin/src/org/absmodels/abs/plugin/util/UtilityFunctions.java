/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.absmodels.abs.plugin.util;

import static org.absmodels.abs.plugin.util.Constants.*;

import java.io.*;
import java.net.URI;
import java.net.URISyntaxException;

import org.absmodels.abs.plugin.Activator;
import org.absmodels.abs.plugin.builder.AbsNature;
import org.absmodels.abs.plugin.console.ConsoleManager;
import org.absmodels.abs.plugin.console.MsgConsole;
import org.absmodels.abs.plugin.console.ConsoleManager.MessageType;
import org.absmodels.abs.plugin.editor.ABSEditor;
import org.absmodels.abs.plugin.editor.outline.PackageAbsFile;
import org.absmodels.abs.plugin.editor.outline.PackageAbsFileEditorInput;
import org.absmodels.abs.plugin.editor.outline.PackageContainer;
import org.absmodels.abs.plugin.editor.outline.PackageEntry;
import org.absmodels.abs.plugin.editor.outline.ABSContentOutlineConstants.AnnotationType;
import org.eclipse.core.filesystem.EFS;
import org.eclipse.core.filesystem.IFileStore;
import org.eclipse.core.resources.*;
import org.eclipse.core.runtime.*;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.preference.IPersistentPreferenceStore;
import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.TextSelection;
import org.eclipse.swt.widgets.Display;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.ide.IDE;

import abs.frontend.ast.*;
import abs.frontend.parser.ABSPackageFile;
import abs.frontend.parser.SourcePosition;

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
	 *  <li>MainBlock</li>
	 * </ul>
	 *  declarations,<br/>
	 * FALSE if m is null or the the module declarations
	 */
	public static boolean hasDecls(ModuleDecl m){
		if (m!=null){
			return (m.getNumDecl() > 0) ||
						(m.getNumExport() > 0) ||
						(m.getNumImport() > 1) ||
						(m.hasBlock());
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
			int startLine = node.getStartLine() - 1;
			int startCol = node.getStartColumn() - 1;
			//Get the end position
			int endLine = node.getEndLine() - 1;
			int endCol = node.getEndColumn();
			return new EditorPosition(null,startLine,startCol,endLine,endCol);
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
	 * Wrapper method for {@link #getAbsNature(IProject)}
	 * 
	 * @param file
	 * @see #getAbsNature(IProject)
	 * @return The corresponding AbsNature of the file is returned, or null if
	 *         file is null
	 */
	public static AbsNature getAbsNature(IResource file){
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

	/**
	 * shows an error dialog using asyncExec
	 */
	public static void showErrorMessageAsync(final String errorMessage){
	    Display.getDefault().asyncExec(new Runnable() {
            @Override
            public void run() {
                MessageDialog.openError(Display.getCurrent().getActiveShell(), "Error", errorMessage);
            }
	    });
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
	
	public static ABSEditor openABSEditorForFile(PackageAbsFile file){
		try {
			return (ABSEditor) IDE.openEditor(getActiveWorkbenchPage(),
					new PackageAbsFileEditorInput(file), Constants.EDITOR_ID);
		} catch (PartInitException e) {
			assert false : e;
		}
		return null;
	}
	

	/**
	 * A convenient method to reconstruct a {@link PackageAbsFile} from the absolute
	 * path to the ABS package and the name to the specific entry in the package.
	 * @param proj the project which the package belongs to
	 * @param pak
	 * @param entry
	 * @return 
	 */
	public static PackageAbsFile getPackageAbsFile(IProject proj, String pak, String entry) {
		File file = new File(pak);
		try {
			if (new ABSPackageFile(file).isABSPackage()) {
				
				PackageEntry pentry = null;
				if (proj != null) {
					AbsNature nature = getAbsNature(proj);
					for (PackageEntry e : nature.getPackages().getPackages()) {
						if (e.getPath().equals(file.getAbsolutePath())) {
							pentry = e;
							break;
						}
					}
				}
				
				if (pentry == null) {
					PackageContainer container = new PackageContainer();
					container.setProject(proj);
					
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
	
	public static IProject getAbsProjectFromWorkspace(String name) {
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
	
	
	public static ABSEditor openABSEditorForFile(IPath path, IProject project){
		IFileStore fileStore = EFS.getLocalFileSystem().getStore(path);
		if (!fileStore.fetchInfo().isDirectory() && fileStore.fetchInfo().exists()) {
		    try {
		        IEditorPart part = IDE.openEditorOnFileStore(getActiveWorkbenchPage(), fileStore);
		        // Could be an ErrorEditorPart:
		        if (part instanceof ABSEditor) {
		        	return (ABSEditor) part;
		        } else {
		        	throw new PartInitException("Couldn't open editor, sorry.");
		        }
		    } catch (PartInitException e) {
		    	standardExceptionHandling(e);
		    }
		} else if (path.toPortableString().startsWith("jar:file:") // works for windows systems
				|| path.segment(0).equals("jar:file:")) { // works for linux systems
			// TODO make conditions above cross platform.
			// a jar file
			try {
				String parts = new URI(path.toString()).getRawSchemeSpecificPart();
				String pak = new URI(parts.split("!/")[0]).getSchemeSpecificPart();
				String entry = parts.split("!/")[1];
				return openABSEditorForFile(getPackageAbsFile(project, pak, entry));
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
			IResource editorFile = (IResource)iep.getEditorInput().getAdapter(IResource.class);
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
			IResource editorFile = (IResource)iep.getEditorInput().getAdapter(IResource.class);
			if(editorFile!=null && editorFile.equals(file)){
				return PlatformUI.getWorkbench().getActiveWorkbenchWindow().getActivePage().saveEditor(iep, withConfirmation);
			}
		}
		return true; //file was not dirty
	}

	public static IFile getFileOfModuleDecl(ModuleDecl m) {
		if (m != null) {
			Path path = new Path(m.getFileName());
			path.makeRelative();
			IFile file = ResourcesPlugin.getWorkspace().getRoot().getFileForLocation(path);
			return file;
		}
		return null;
	}
	
	public static boolean hasABSFileExtension(IResource file){
		return file != null && ABS_FILE_EXTENSION.equals(file.getFileExtension());
	}
	
	public static boolean isABSSourceFile(IResource file){
	    if (file != null && file instanceof IFile) // TODO: IResource?
	       return hasABSFileExtension(file);
	    return false;
	}

	public static boolean isABSFile(IResource file){
		if (file != null && file instanceof IFile){ // TODO: IResource?
			return hasABSFileExtension(file) || isABSPackage((IFile)file);
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


    /**
     * checks if a string is formatted as a number
     */
	public static boolean isNumber(String s) {
        try {
            Integer.parseInt(s);
            return true;
        } catch (NumberFormatException e) {
            return false;
        }
    }

    /**
     * prints a message to the default console
     */
	public static void printToConsole(String msg) {
	    MsgConsole c = ConsoleManager.getDefault();
	    c.println(msg, MessageType.MESSAGE_INFO);
	}

    public static boolean jumpToPosition(IProject project, EditorPosition pos) {
        ABSEditor targeteditor = UtilityFunctions.openABSEditorForFile(pos.getPath(),project );
            if(targeteditor==null){
                return false;
            }
            IDocument doc = targeteditor.getDocumentProvider().getDocument(targeteditor.getEditorInput());
            try {    
                int startoff = doc.getLineOffset(pos.getLinestart()-1) + pos.getColstart()-1;
                targeteditor.getSelectionProvider().setSelection(new TextSelection(startoff,0));
                return true;
            } catch(BadLocationException ex){
                Activator.logException(ex);
                return false;
            }
    }
	
}
