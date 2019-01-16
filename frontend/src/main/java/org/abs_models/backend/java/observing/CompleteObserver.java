/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.abs_models.backend.java.observing;

/**
 * A convenience interface that represents all possible observers
 * 
 * @author Jan Schäfer
 *
 */
public interface CompleteObserver extends SystemObserver, ObjectCreationObserver, ObjectObserver, TaskSchedulerObserver {
}
