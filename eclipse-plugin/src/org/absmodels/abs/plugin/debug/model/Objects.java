/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.absmodels.abs.plugin.debug.model;

import java.util.ArrayList;
import java.util.List;

import abs.backend.java.observing.COGView;
import abs.backend.java.observing.ObjectView;

/**
 * This class is necessary to group objects in the tree viewer of the debug view.
 * @see org.absmodels.abs.plugin.debug.views.debugview.DebugTreeContentProvider
 * @author tfischer
 */
public class Objects {
	private COGView cog;
	private List<ObjectView> objects;
	
	public Objects(COGView cog){
		this.cog = cog;
		objects = new ArrayList<ObjectView>();
	}
	
	public COGView getCOG(){
		return cog;
	}
	
	public void addObject(ObjectView object){
		objects.add(object);
	}
	
	public List<ObjectView> getObjects(){
		return objects;
	}
}
