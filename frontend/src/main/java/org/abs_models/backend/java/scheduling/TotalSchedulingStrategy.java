/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.abs_models.backend.java.scheduling;

/**
 * A scheduling strategy which does global and task scheduling. Can be set by
 * the property -Dabs.totalscheduler=<classname>
 * 
 * @author Jan Schäfer
 * 
 */
public interface TotalSchedulingStrategy extends GlobalSchedulingStrategy, TaskSchedulingStrategy {

}
