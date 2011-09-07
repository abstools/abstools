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
package costabs.utils;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.jdt.core.ICompilationUnit;
import org.eclipse.jdt.core.IJavaElement;
import org.eclipse.jdt.core.IJavaProject;
import org.eclipse.jdt.core.IMethod;
import org.eclipse.jdt.core.JavaCore;
import org.eclipse.jdt.core.JavaModelException;
import org.eclipse.jdt.internal.core.SourceType;
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.part.FileEditorInput;

import costabs.exceptions.CostabsException;



/**
 * Class to interact with source files
 * @author groman
 * @license GPL
 *
 */
public class SourceUtils {

	
	
	public static IMethod[] getMethodsFromJavaFile (IJavaElement element) throws Exception{
		try {
			ICompilationUnit unit = (ICompilationUnit)element;
			IJavaElement elements[] = unit.getChildren();
			for (int i = 0; i < elements.length; i ++) {
				//LogUtils.debug(elements[i].getClass());
				if (elements[i] instanceof org.eclipse.jdt.internal.core.SourceType) {
					SourceType source = (SourceType) elements[i];

					return source.getMethods();
				}
			}
			throw new Exception ("No methods found in file ");

		}
		catch (Exception e) {
			throw new Exception ("Cannot find the methods from Java file");
		}

	}
	
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
	public static IProject obtainCurrProject()throws CostabsException{
		IEditorPart ieditorpart;
		ClassLoader loader;		
		//we obtain the current window that is being edited
		try{
			ieditorpart = SourceUtils.obtainActiveEditor();
		}catch(NullPointerException e){
			throw(new CostabsException("There is not any selected class."));
		}
		IResource jresource = extractResource (ieditorpart);
		if(jresource==null)
			throw(new CostabsException("Could not extract file from the current editor"));
		
		return jresource.getProject();
	}
	public static Class ObtainCurrentlyEditingClass()throws CostabsException{
		IEditorPart ieditorpart;
		ClassLoader loader;		
		//we obtain the current window that is being edited
		try{
			ieditorpart = SourceUtils.obtainActiveEditor();

		//If the file is not safe we fail and say it
		if (ieditorpart.isDirty()) {
			throw(new CostabsException("Java file must be saved to analyze it."));
		}
		}catch(NullPointerException e){
			throw(new CostabsException("There is not any selected class."));

		}
		//obtain the file being modified
		IResource jresource = extractResource (ieditorpart);
		if(jresource==null)
			throw(new CostabsException("Could not extract file from the current editor"));

		//get the project that owns the file
		IJavaProject jproject=obtainJavaProjectFromResource(jresource);
		if(jproject==null)
			throw(new CostabsException("Cannot load the File: project must be a Java Project"));
	
		IJavaElement javaFile=JavaCore.create(jresource, jproject);
		if(javaFile==null)
			throw(new CostabsException("Cannot load the File: file must be a Java File"));
		try {
			loader = ClasspathUtils.getProjectClassLoader(jproject);
		}
		catch (Exception e ) {
			throw(new CostabsException("Non valid project, failed to buid class loader"));
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
	private static void fileEvaluations (IJavaElement javaFile) throws CostabsException {
		try {
			if (javaFile.getElementType() != IJavaElement.COMPILATION_UNIT) {
				throw new CostabsException ("The file must be a Java file");
			}
			ICompilationUnit javaFileComp=(ICompilationUnit)javaFile;

			if (!javaFile.isStructureKnown()) {
				throw new CostabsException ("The file cannot have errors to analyze it");
			}
			if (!javaFile.getElementName().endsWith(".java")) {
				throw new CostabsException ("The file is not a Java File");
			}
		
			if (!javaFileComp.isConsistent()) {
				throw new CostabsException ("The file is not consistent, cannot proccess it");
			}

		}
		catch (JavaModelException e) {
			throw new CostabsException ("Cannot evaluate the file, it may be incorrect");

		}
	}
	
	private static Class getClassFromResource (ICompilationUnit javaFile,ClassLoader loader) throws CostabsException {
		try {
			String className = javaFile.getElementName();
			className = className.substring(0, className.length() - 5);
			if (!"".equals(javaFile.getParent().getElementName()))
				className = javaFile.getParent().getElementName() + "." + className;

			Class clazz = loader.loadClass(className);

			return clazz;
		}
		
		catch (ClassNotFoundException t) {
			//t.printStackTrace();
			throw new CostabsException ("The class loading failed... Classpath may be wrong");
		}
	}
}
