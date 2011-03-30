/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package eu.hatsproject.absplugin.exceptions;

/**
 * This class represents exceptions in a java job 
 * which not maps to the existing exceptions
 * e.g. for user mistakes
 */
public class AbsJobException extends Exception {
	
	/**
	 * a generated serialVersionUId
	 */
	private static final long serialVersionUID = 4390613464722126595L;

	public AbsJobException() {
		super();
	}
	
	public AbsJobException(String message) {
		super(message);
	}

}
