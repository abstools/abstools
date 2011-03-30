/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.backend.java.lib.runtime;

public class SystemTerminatedException extends RuntimeException {
    private static final long serialVersionUID = 1L;
    
    public SystemTerminatedException() {
        super("The ABS Runtime has been terminated");
    }

}
