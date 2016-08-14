/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.absmodels.abs.plugin.exceptions;

import java.util.List;

import abs.frontend.parser.ParserError;

public class ParseException extends Exception{
	private static final long serialVersionUID = 1924363294827297713L;
	
	public ParseException(){		
	}
	
	public ParseException(List<ParserError> parseErrors){
		super(getMessage(parseErrors));
	}
	
	private static String getMessage(List<ParserError> parseErrors){
		StringBuffer result = new StringBuffer("Project contains parse errors: ");
		for(ParserError error : parseErrors){
			//TODO: newline doesn't work ):
			result.append("\n");
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
