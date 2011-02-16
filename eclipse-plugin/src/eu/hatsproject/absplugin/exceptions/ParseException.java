package eu.hatsproject.absplugin.exceptions;

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
