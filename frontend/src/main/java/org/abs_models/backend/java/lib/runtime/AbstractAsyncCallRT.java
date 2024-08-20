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

import org.apfloat.Aprational;

public abstract class AbstractAsyncCallRT<T extends ABSRef> extends AbstractAsyncCall<T> implements AsyncCallRTAttributes {

    /**
     * The absolute value of the deadline, or {@code -1} if no deadline given.
     * In ABS code, we express deadlines relative to the current value of the
     * clock, but we store them as absolute values internally.
     */
    private Aprational deadline_t;
    /**
     * The computation time, as given as an annotation at the method
     * definition, or {@code -1} if no cost annotation present at the method
     * definition.
     */
    private Aprational cost;
    /**
     * Whether the deadline is critical, and the process should be terminated
     * when meeting it (currently unused).
     */
    private boolean critical;

    public AbstractAsyncCallRT(ABSObject source, T target, ABSDataType dl, ABSDataType co, ABSBool cr) {
        super(source, target);
        deadline_t = convertFromDuration(dl);
        if (deadline_t.signum() > 0) {
            // it's a real deadline; convert to absolute time value
            deadline_t = deadline_t.add(ABSRuntime.getRuntime().getClock());
        }
        cost = convertFromDuration(co);
        critical = cr.toBoolean();
    }

    @Override
    public Aprational getDeadlineAbsolute() {
        return deadline_t;
    }

    @Override
    public Aprational getCost() {
        return cost;
    }

    @Override
    public boolean isCritical() {
        return critical;
    }

    private Aprational convertFromDuration(ABSDataType duration) {
        if (duration.getConstructorName().equals("Duration")) {
            ABSRational rat = (ABSRational)duration.getArg(0);
            return rat.toAprational();
        } else if (duration.getConstructorName().equals("InfDuration")) {
            return new Aprational(-1);
        } else {
            // should never happen
            throw new JavaBackendException("Argument is not of type ABS.StdLib.Duration: " + duration);
        }
    }

}
