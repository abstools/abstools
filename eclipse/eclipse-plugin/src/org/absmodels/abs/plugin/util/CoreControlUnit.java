/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.absmodels.abs.plugin.util;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;

import org.eclipse.core.resources.IResource;



public class CoreControlUnit {
	
	private static List<ResourceBuildListener> buildListeners = new ArrayList<ResourceBuildListener>();
	
	//-------------------- INTERFACES ------------------------------------------------------------------------
	public interface ResourceBuildListener{
		public void resourceBuilt(ResourceBuiltEvent builtevent);
	}
	
	public static class ResourceBuiltEvent{
		private HashSet<String> changed;

		public ResourceBuiltEvent(HashSet<String> changed){
			this.changed = changed;
		}

		public boolean hasChanged(IResource editorres) {
			if(editorres!=null && editorres.getFullPath()!=null)
				return changed.contains(editorres.getFullPath().toString());
			else
				return false;
		}
	}
	
	//-------------------- METHODS ---------------------------------------------------------------------------	
	public static void addResourceBuildListener(ResourceBuildListener listener){
		if(buildListeners.contains(listener))
			return;
		buildListeners.add(listener);
	}
	
	public static void removeResourceBuildListener(ResourceBuildListener listener){
		if(buildListeners.contains(listener))
			buildListeners.remove(listener);
	}
	
	public static void notifyBuildListener(HashSet<String> changed){
		for(ResourceBuildListener listener : new ArrayList<ResourceBuildListener>(buildListeners)){
			listener.resourceBuilt(new ResourceBuiltEvent(changed));
		}
	}
}
