/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.absmodels.abs.plugin.debug.views.variablesview;

import java.util.ArrayList;

import org.absmodels.abs.plugin.debug.model.VariableValuePair;
import org.eclipse.jface.viewers.ITreeContentProvider;
import org.eclipse.jface.viewers.Viewer;


import abs.backend.java.lib.runtime.ABSObject;
import abs.backend.java.observing.ObjectView;
import abs.backend.java.observing.TaskStackFrameView;

/**
 * The TreeContentProvider for the TreeViewer of the VariableView. This class provides
 * the structural information about the variable / object tree, i.e. what the children of specific 
 * elements are.
 * @author tfischer
 */
public class VariableContentProvider implements ITreeContentProvider {
	@Override
	public Object[] getChildren(Object parentElement) {
		if(parentElement != null){
			if(parentElement instanceof TaskStackFrameView){
				TaskStackFrameView stackFrame = (TaskStackFrameView) parentElement;
				ArrayList<VariableValuePair> children = new ArrayList<VariableValuePair>();
				for(String variableName : stackFrame.getVariableNames()){
					children.add(new VariableValuePair(variableName, stackFrame.getValue(variableName)));
				}
				return children.toArray();
			} else if(parentElement instanceof ObjectView){
				ObjectView obj = (ObjectView)parentElement;
				ArrayList<VariableValuePair> children  = new ArrayList<VariableValuePair>();
				for(String fieldName : obj.getFieldNames()){
					try {
						children.add(new VariableValuePair(fieldName, obj.getFieldValue(fieldName)));
					} catch (NoSuchFieldException e) {
						//can never happen. Since we iterate over the field names, the field is guaranteed
						//to exist as long as the objectView is implemented correctly.
					}
				}
				return children.toArray();
			} else if(parentElement instanceof VariableValuePair){
				return getChildren(((VariableValuePair)parentElement).getValue());
			} else if(parentElement instanceof ABSObject){
				return getChildren(((ABSObject)parentElement).getView());
			}
		}
		//single (or unknown) value - no children
		return new Object[0];
	}

	@Override
	public Object[] getElements(Object inputElement) {
		if (inputElement != null){
			if (inputElement instanceof TaskStackFrameView
					|| inputElement instanceof ObjectView){
				return getChildren(inputElement);
			} else if (inputElement instanceof Object[]) {
				return (Object[]) inputElement;
			} 
		}
		return new Object[0];
	}

	@Override
	public boolean hasChildren(Object element) {
		if(element instanceof TaskStackFrameView){
			TaskStackFrameView stackFrame = (TaskStackFrameView)element;
			if(stackFrame.getStack() == null){
				return false;
			} else{
				return stackFrame.getStack().getFrames().size() > 0;
			}
		} else if(element instanceof ObjectView){
			return ((ObjectView)element).getFieldNames().size() > 0;
		} else if(element instanceof VariableValuePair){
			return hasChildren(((VariableValuePair)element).getValue());
		} else if(element instanceof ABSObject){
			return hasChildren(((ABSObject)element).getView());
		} else{
			return false;
		}
	}
	
	@Override
	public Object getParent(Object element) {
		//unnecessary to implement this method here
		return null;
	}
	
	@Override
	public void dispose() {	}

	@Override
	public void inputChanged(Viewer viewer, Object oldInput, Object newInput) {	}
	
}
