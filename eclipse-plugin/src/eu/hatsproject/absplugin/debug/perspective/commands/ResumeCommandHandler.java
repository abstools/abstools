/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package eu.hatsproject.absplugin.debug.perspective.commands;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;

import static eu.hatsproject.absplugin.debug.DebugUtils.resume;

/**
 * Class handling the f8 shortcut for resuming
 * @author mweber
 */
public class ResumeCommandHandler extends AbstractHandler {

	@Override
	public Object execute(ExecutionEvent event) throws ExecutionException {
		if(resume != null && resume.isEnabled()){
			resume.run();
		}
		return null;
	}

}
