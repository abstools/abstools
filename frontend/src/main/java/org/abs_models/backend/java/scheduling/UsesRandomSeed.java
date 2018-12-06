/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.abs_models.backend.java.scheduling;

import java.util.Random;

/**
 * implement this interface if a scheduler makes random decisions 
 */
public interface UsesRandomSeed {
    void setRandom(Random random);
}
