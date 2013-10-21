/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.absmodels.abs.plugin.debug.views.variablesview;


import org.absmodels.abs.plugin.debug.model.VariableValuePair;
import org.eclipse.jface.viewers.ILabelProviderListener;
import org.eclipse.jface.viewers.ITableLabelProvider;
import org.eclipse.swt.graphics.Image;

import abs.backend.java.observing.ObjectView;

/**
 * TableLabelProvider for the TreeViewer of the VariablView. This class provides labels (and could also 
 * provide icons if wanted in later revisions) for the variable tree reflecting the state of a stack frame 
 * or an object.
 * @author tfischer
 */
public class VariableLabelProvider implements ITableLabelProvider{
	
	@Override
	public void addListener(ILabelProviderListener listener) { }
	@Override
	public void dispose() { }
	@Override
	public boolean isLabelProperty(Object element, String property) { return false;	}
	@Override
	public void removeListener(ILabelProviderListener listener) { }
	@Override
	public Image getColumnImage(Object element, int columnIndex) { return null;	}

	@Override
	public String getColumnText(Object element, int columnIndex) {
		if(element != null){
			if(element instanceof VariableValuePair){
				VariableValuePair elem = (VariableValuePair)element;
				if(elem.getValue() instanceof ObjectView && columnIndex == 0){
					return elem.getIdentifier();
				} else{
					switch(columnIndex){
					case 0 : return elem.getIdentifier();
					case 1 : 
						if(elem.getValue() == null){
							return "null";
						} else{
							return elem.getValue().toString();
						}
					default: return "Errornous Column";
					}
				}
			} else{
				return "Error: The VariableView can only display VariableValuePairs. Encountered element " + element.getClass().toString();
			}
		} else{
			return "Error: The VariableView can only display VariableValuePairs. Encountered null.";
		}
		
	}
}
