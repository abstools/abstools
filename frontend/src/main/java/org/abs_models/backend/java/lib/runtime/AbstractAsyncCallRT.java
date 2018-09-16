/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.abs_models.backend.java.lib.runtime;

import org.abs_models.backend.java.JavaBackendException;
import org.abs_models.backend.java.lib.types.ABSBool;
import org.abs_models.backend.java.lib.types.ABSDataType;
import org.abs_models.backend.java.lib.types.ABSRational;
import org.abs_models.backend.java.lib.types.ABSRef;

public abstract class AbstractAsyncCallRT<T extends ABSRef> extends AbstractAsyncCall<T> implements AsyncCallRTAttributes {

    private long deadline;    //milliseconds
    private long cost;        //milliseconds
    private boolean critical;

    public AbstractAsyncCallRT(ABSObject source, T target, ABSDataType dl, ABSDataType co, ABSBool cr) {
        super(source, target);
        deadline = convertFromDuration(dl);
        cost = convertFromDuration(co);
        critical = cr.toBoolean();
    }
    
    @Override
    public long getDeadline() {
        return deadline;
    }

    @Override
    public long getCost() {
        return cost;
    }

    @Override
    public boolean isCritical() {
        return critical;
    }

    private long convertFromDuration(ABSDataType duration) {
        if (duration.getConstructorName().equals("Duration")) {
            ABSRational rat = (ABSRational)duration.getArg(0);
            // convert from seconds to milliseconds
            return rat.multiply(ABSRational.fromInt(1000)).toInt();
        } else if (duration.getConstructorName().equals("InfDuration")) {
            return -1;
        } else {
            // should never happen
            throw new JavaBackendException("Argument is not of type ABS.StdLib.Duration");
        }
    }

}
