package eu.hatsproject.absplugin.debug.model;

import abs.backend.java.lib.types.ABSValue;

/**
 * This class is necessary to support objects in the tree viewer of the variable view.
 * @see eu.hatsproject.absplugin.debug.views.variablesview.VariableContentProvider
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
