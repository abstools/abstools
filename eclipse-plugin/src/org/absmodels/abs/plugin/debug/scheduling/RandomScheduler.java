/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.absmodels.abs.plugin.debug.scheduling;

import java.util.Random;

import abs.backend.java.scheduling.RandomSchedulingStrategy;

/**
 * This is a base scheduler. It is based on the {@link RandomSchedulingStrategy} found in the
 * abs frontend. It returns always a random next step of the program as long as there are steps left.
 * 
 * @author mweber
 *
 */
public class RandomScheduler extends RandomSchedulingStrategy implements TotalScheduler{
	public RandomScheduler() {
		super();
	}
	
	public RandomScheduler(Random random){
		super(random);
	}
	
	@Override
	public void reset() {
	}
	
}