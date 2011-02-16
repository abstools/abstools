package eu.hatsproject.absplugin.debug.scheduling;

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