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

import java.io.File;
import java.lang.reflect.Constructor;
import java.lang.reflect.Method;
import java.net.URL;
import java.net.URLClassLoader;

import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.jdt.core.IClasspathEntry;
import org.eclipse.jdt.core.IJavaProject;
import org.eclipse.jdt.core.JavaCore;
import org.eclipse.jdt.internal.core.JavaModel;

/**
 * Class to get the project classpath
 * @author groman
 * @license GPL
 */
public class ClasspathUtils {

	/**
	 * Returns the Eclipse Project class loader using project build path
	 * @param javaProject Eclipse Java Project
	 * @return A class loader using project build path
	 * @throws Exception If the project is no valid
	 */
	public static ClassLoader getProjectClassLoader(IJavaProject javaProject) throws Exception {
		IClasspathEntry[] entries = javaProject.getResolvedClasspath(true);
		String wsPath = javaProject.getProject().getLocation().toPortableString();
		String firstEntryLocation = javaProject.getPath().toPortableString();
		int idx = wsPath.indexOf(firstEntryLocation);
		
		URL[] urls = null;
		int i = 0;

		//System.out.println("ClassLoader " + wsPath);
		//System.out.println("ClassLoader " + firstEntryLocation);
		
		String output = javaProject.getOutputLocation().toPortableString();
		urls = new URL[entries.length+1];
		
		if (idx != -1) {
			wsPath = wsPath.substring(0,idx);
		}
		else {
			output = output.substring(firstEntryLocation.length());
		}
		urls[i++] = new File(wsPath + output).toURL();

		//System.out.println("ClassLoader " + output);
		
		String fullPath = null;

		for (IClasspathEntry entry : entries)
		{
			if(entry.getEntryKind() == IClasspathEntry.CPE_PROJECT)
			{
				IResource project =  ResourcesPlugin.getWorkspace().getRoot().findMember(entry.getPath());
				String projectPath = 
					JavaCore.create(project.getProject()).getOutputLocation().toPortableString();
				fullPath = wsPath + projectPath;
			}
			else
			{
				Object resource = JavaModel.getTarget(entry.getPath(),true);

				if (resource instanceof IResource)
				{
					IResource iFile = (IResource) resource;
					fullPath = iFile.getLocation().toPortableString();
				}
				else if (resource instanceof File)
				{
					File file = (File) resource;
					fullPath = file.getAbsolutePath();
				}
			}

			urls[i++] = new File(fullPath).toURL();
			//System.out.println(fullPath);
		}

		URLClassLoader classLoader = new URLClassLoader(urls,String.class.getClassLoader());

		/*for (int j = 0; j < urls.length; j ++) {
			System.out.println(urls[j]);
		}*/
		
		return classLoader;
	}

	/**
	 * Returns a String with all libraries defined in build path
	 * @param javaProject Eclipse Java Project
	 * @return a String with all libraries defined in build path
	 * @throws Exception If the project is no valid
	 */
	public static String getStringClasspath (IJavaProject javaProject) throws Exception {

		IClasspathEntry[] entries = javaProject.getResolvedClasspath(true);
		String wsPath = javaProject.getProject().getLocation().toPortableString();
		String firstEntryLocation = javaProject.getPath().toPortableString();
		int idx = wsPath.indexOf(firstEntryLocation);
	
		String[] urls = new String[entries.length+1];
		String fullPath = null;
		int i = 0;
		String output = javaProject.getOutputLocation().toPortableString();

		if (idx != -1) {
			wsPath = wsPath.substring(0,idx);
		}
		else {
			output = output.substring(firstEntryLocation.length());
		}
		urls[i++] = wsPath + output;
		
		for (IClasspathEntry entry : entries)
		{
			if(entry.getEntryKind() == IClasspathEntry.CPE_PROJECT)
			{
				IResource project =  ResourcesPlugin.getWorkspace().getRoot().findMember(entry.getPath());
				String projectPath = 
					JavaCore.create(project.getProject()).getOutputLocation().toPortableString();
				fullPath = wsPath + projectPath;
			}
			else
			{
				Object resource = JavaModel.getTarget(entry.getPath(),true);

				if (resource instanceof IResource)
				{
					IResource iFile = (IResource) resource;
					fullPath = iFile.getLocation().toPortableString();
				}
				else if (resource instanceof File)
				{
					File file = (File) resource;
					fullPath = file.getAbsolutePath();
				}
			}

			urls[i++] = fullPath;
		}

		StringBuffer buffer = new StringBuffer ();
		for (String url : urls) {
			buffer.append(url);
			buffer.append(":");
		}

		
		return buffer.toString().substring(0, buffer.length()-1);

	}
	
	public static void createPath(String path) {
		new File(path.replaceAll("[/]", "//")).mkdirs();
	}

}
