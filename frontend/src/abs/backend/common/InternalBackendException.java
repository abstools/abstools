/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.backend.common;

@SuppressWarnings("serial")
public class InternalBackendException extends Exception {

    public InternalBackendException() {
        super("An internal exception occurred but the programmer did not supply a message, sorry");
    }

    public InternalBackendException(String message) {
        super(message);
    }

    public InternalBackendException(Throwable cause) {
        super(cause);
    }

    public InternalBackendException(String message, Throwable cause) {
        super(message, cause);
    }

    public InternalBackendException(String message, Throwable cause, boolean enableSuppression,
            boolean writableStackTrace) {
        super(message, cause, enableSuppression, writableStackTrace);
    }

}
