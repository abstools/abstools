/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.absmodels.abs.plugin.exceptions;

import java.util.List;

import abs.frontend.analyser.SemanticCondition;
import abs.frontend.analyser.SemanticConditionList;

public class TypeCheckerException extends Exception{
	private static final long serialVersionUID = -6951753769373256177L;

	public TypeCheckerException(){
	}
	
	public TypeCheckerException(SemanticConditionList typeErrors){
		super(getMessage(typeErrors));
	}
	
	private static String getMessage(SemanticConditionList typeErrors){
		StringBuffer result = new StringBuffer("Project contains parse errors: ");
		for(SemanticCondition error : typeErrors){
			//TODO: newline doesn't work ):
			result.append('\n');
			result.append(error.getFileName());
			result.append(':');
			result.append(error.getLine());
			result.append(':');
			result.append(error.getColumn());
			result.append(' ');
			result.append(error.getMessage());
		}
		return result.toString();
	}
}
