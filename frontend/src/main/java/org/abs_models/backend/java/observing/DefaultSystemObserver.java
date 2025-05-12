/** 
 * Copyright (c) 2025, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.abs_models.backend.java.observing;

import org.abs_models.backend.java.lib.runtime.ABSException;

/**
 * A default implementation of the {@link SystemObserver} interface.
 *
 * @author Rudi Schlatte
 */
public class DefaultSystemObserver implements SystemObserver {

	@Override
	public void systemStarted() {
	}

	@Override
	public void newCOGCreated(COGView cog, ObjectView initialObject) {
	}

	@Override
	public void systemError(ABSException e) {
	}

	@Override
	public void systemFinished() {
	}
}
