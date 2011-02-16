package eu.hatsproject.absplugin.debug.scheduling;

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