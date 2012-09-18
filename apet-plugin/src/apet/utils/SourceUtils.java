/**
Copyright (C) 2010  E.Albert, P.Arenas, S.Genaim, G.Puebla, and D.Zanardini, G. Roman
                    https://costa.ls.fi.upm.es

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.

*/
package apet.utils;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.jdt.core.ICompilationUnit;
import org.eclipse.jdt.core.IJavaElement;
import org.eclipse.jdt.core.IJavaProject;
import org.eclipse.jdt.core.JavaCore;
import org.eclipse.jdt.core.JavaModelException;
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.part.FileEditorInput;

import apet.exceptions.ApetException;



/**
 * Class to interact with source files
 * @author groman
 * @license GPL
 *
 */
public class SourceUtils {

	
	public static IResource extractResource(IEditorPart editor) {
		IEditorInput input = editor.getEditorInput();
		if (!(input instanceof FileEditorInput))
			return null;
		return ((FileEditorInput)input).getFile();
	}
	
	private static IJavaProject obtainJavaProjectFromResource(IResource jresource){
		IProject project = jresource.getProject();
		return JavaCore.create(project);
	}
	
	public static IProject obtainCurrProject()throws ApetException{
		IEditorPart ieditorpart;
		//we obtain the current window that is being edited
		try{
			ieditorpart = SourceUtils.obtainActiveEditor();
		}catch(NullPointerException e){
			throw(new ApetException("There is not any selected class."));
		}
		IResource jresource = extractResource (ieditorpart);
		if(jresource==null)
			throw(new ApetException("Could not extract file from the current editor"));
		
		return jresource.getProject();
	}
	
	public static Class<?> ObtainCurrentlyEditingClass()throws ApetException{
		IEditorPart ieditorpart;
		ClassLoader loader;		
		//we obtain the current window that is being edited
		try{
			ieditorpart = SourceUtils.obtainActiveEditor();

		//If the file is not safe we fail and say it
		if (ieditorpart.isDirty()) {
			throw(new ApetException("Java file must be saved to analyze it."));
		}
		}catch(NullPointerException e){
			throw(new ApetException("There is not any selected class."));

		}
		//obtain the file being modified
		IResource jresource = extractResource (ieditorpart);
		if(jresource==null)
			throw(new ApetException("Could not extract file from the current editor"));

		//get the project that owns the file
		IJavaProject jproject=obtainJavaProjectFromResource(jresource);
		if(jproject==null)
			throw(new ApetException("Cannot load the File: project must be a Java Project"));
	
		IJavaElement javaFile=JavaCore.create(jresource, jproject);
		if(javaFile==null)
			throw(new ApetException("Cannot load the File: file must be a Java File"));
		try {
			loader = ClasspathUtils.getProjectClassLoader(jproject);
		}
		catch (Exception e ) {
			throw(new ApetException("Non valid project, failed to buid class loader"));
		}
		
		fileEvaluations(javaFile);
		return getClassFromResource((ICompilationUnit)javaFile,loader);
	}
	
	public static IEditorPart obtainActiveEditor()throws NullPointerException{
		IWorkbench iworkbench = PlatformUI.getWorkbench();
		IWorkbenchWindow iworkbenchwindow = iworkbench.getActiveWorkbenchWindow();
		IWorkbenchPage iworkbenchpage = iworkbenchwindow.getActivePage();
		return iworkbenchpage.getActiveEditor();
	}
	
	private static void fileEvaluations (IJavaElement javaFile) throws ApetException {
		try {
			if (javaFile.getElementType() != IJavaElement.COMPILATION_UNIT) {
				throw new ApetException ("The file must be a Java file");
			}
			ICompilationUnit javaFileComp=(ICompilationUnit)javaFile;

			if (!javaFile.isStructureKnown()) {
				throw new ApetException ("The file cannot have errors to analyze it");
			}
			if (!javaFile.getElementName().endsWith(".java")) {
				throw new ApetException ("The file is not a Java File");
			}
		
			if (!javaFileComp.isConsistent()) {
				throw new ApetException ("The file is not consistent, cannot proccess it");
			}

		}
		catch (JavaModelException e) {
			throw new ApetException ("Cannot evaluate the file, it may be incorrect");

		}
	}
	
	private static Class<?> getClassFromResource (ICompilationUnit javaFile,ClassLoader loader) throws ApetException {
		try {
			String className = javaFile.getElementName();
			className = className.substring(0, className.length() - 5);
			if (!"".equals(javaFile.getParent().getElementName()))
				className = javaFile.getParent().getElementName() + "." + className;

			Class<?> clazz = loader.loadClass(className);

			return clazz;
		}
		
		catch (ClassNotFoundException t) {
			//t.printStackTrace();
			throw new ApetException ("The class loading failed... Classpath may be wrong");
		}
	}
}
