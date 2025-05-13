/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.abs_models.backend.java.lib.runtime;

import java.util.Collections;
import java.util.List;

import org.abs_models.backend.java.lib.types.ABSRef;

public abstract class AbstractAsyncCall<T extends ABSRef> implements AsyncCall<T> {

    protected final ABSObject source;
    protected final T target;
    protected final Task<?> sender;

    public AbstractAsyncCall(ABSObject source, T target) {
        this(ABSThread.getCurrentTask(),source,target);
    }
    
    public AbstractAsyncCall(Task<?> sender, ABSObject source, T target) {
        this.sender = sender;
        this.source = source;
        this.target = target;
    }
    
    @Override
    public List<Object> getArgs() {
        return Collections.emptyList();
    }

    @Override
    public T getTarget() {
        return target;
    }

    @Override
    public ABSObject getSource() {
        return source;
    }

    public Task<?> getSender() {
        return sender;
    }
    
}
