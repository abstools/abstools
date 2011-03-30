/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package eu.hatsproject.absplugin.exceptions;

import java.util.List;

import abs.frontend.analyser.SemanticError;

public class TypeCheckerException extends Exception{
	private static final long serialVersionUID = -6951753769373256177L;

	public TypeCheckerException(){
	}
	
	public TypeCheckerException(List<SemanticError> typeErrors){
		super(getMessage(typeErrors));
	}
	
	private static String getMessage(List<SemanticError> typeErrors){
		StringBuffer result = new StringBuffer("Project contains parse errors: ");
		for(SemanticError error : typeErrors){
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
