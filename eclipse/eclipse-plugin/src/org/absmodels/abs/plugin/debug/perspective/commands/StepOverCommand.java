/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.absmodels.abs.plugin.debug.perspective.commands;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;

import static org.absmodels.abs.plugin.debug.DebugUtils.executeSingleStep;

/**
 * Class handling the f6 shortcut for single steps
 * @author mweber
 */
public class StepOverCommand extends AbstractHandler {

	@Override
	public Object execute(ExecutionEvent event) throws ExecutionException {
		if(executeSingleStep != null && executeSingleStep.isEnabled()){
			executeSingleStep.run();
		}
		return null;
	}

}
