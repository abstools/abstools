/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.absmodels.abs.plugin.debug.model;

import abs.backend.java.observing.COGView;

/**
 * This class is necessary to group tasks in the tree viewer of the debug view.
 * @see org.absmodels.abs.plugin.debug.views.debugview.DebugTreeContentProvider
 * @author tfischer
 */
public class Tasks {
	private COGView cog;
	
	public Tasks(COGView cog){
		this.cog = cog;
	}
	
	public COGView getCOG(){
		return cog;
	}
}
