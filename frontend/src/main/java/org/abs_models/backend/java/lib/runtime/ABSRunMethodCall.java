/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.abs_models.backend.java.lib.runtime;

public class ABSRunMethodCall<T extends ABSObject> extends AbstractAsyncCall<T> {

    public ABSRunMethodCall(Task<?> sender, ABSObject source, T target) {
        super(sender, source, target);
    }

    @Override
    public Object call() {
        target.run();
        return null;
    }

    @Override
    public String methodName() {
        return "run";
    }

}
