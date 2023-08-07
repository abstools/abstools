/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.abs_models.backend.java.lib.runtime;

import org.apfloat.Aprational;

public interface AsyncCallRTAttributes {
    Aprational getDeadlineAbsolute();
    Aprational getCost();
    boolean isCritical();
}
