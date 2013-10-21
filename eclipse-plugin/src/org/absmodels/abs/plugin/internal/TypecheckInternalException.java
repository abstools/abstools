/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.absmodels.abs.plugin.internal;

public class TypecheckInternalException extends Exception {

	private static final long serialVersionUID = -3846977590027791843L;
	private Exception wrappedException;
	
	public TypecheckInternalException(Exception wrapped){
		super(wrapped);
		wrappedException = wrapped;
	}
	
	public Exception getWrappedException(){
		return wrappedException;
	}
	
	@Override
	public String toString() {
		return super.toString() + "\nWrapped:\n" + wrappedException.toString();
	}
}
