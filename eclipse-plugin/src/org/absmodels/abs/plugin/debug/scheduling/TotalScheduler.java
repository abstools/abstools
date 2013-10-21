/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.absmodels.abs.plugin.debug.scheduling;

import abs.backend.java.scheduling.TotalSchedulingStrategy;

/**
 * {@link TotalScheduler} is an extension of the {@link TotalSchedulingStrategy} with a state.
 * 
 * @author mweber
 *
 */
public interface TotalScheduler extends TotalSchedulingStrategy{
	/**
	 * resets the internal state of the scheduler
	 */
	public void reset();
}