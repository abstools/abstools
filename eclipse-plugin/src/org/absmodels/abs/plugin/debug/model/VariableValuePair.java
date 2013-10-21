/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.absmodels.abs.plugin.debug.model;

import abs.backend.java.lib.types.ABSValue;

/**
 * This class is necessary to support objects in the tree viewer of the variable view.
 * @see org.absmodels.abs.plugin.debug.views.variablesview.VariableContentProvider
 * @author tfischer
 */
public class VariableValuePair{
	private String name;
	private ABSValue value;
	
	public VariableValuePair(String name, ABSValue value){
		this.name = name;
		this.value = value;
	}
	
	public String getIdentifier(){
		return name;
	}
	
	public ABSValue getValue(){
		return value;
	}
}
