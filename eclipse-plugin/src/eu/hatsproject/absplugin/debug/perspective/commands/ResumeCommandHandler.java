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
		if(resume.isEnabled()){
			resume.run();
		}
		return null;
	}

}
