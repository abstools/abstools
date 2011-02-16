package eu.hatsproject.absplugin.wizards.composites;

import static eu.hatsproject.absplugin.util.Constants.STYLER_BLACK;

import org.eclipse.jface.viewers.ILabelProvider;
import org.eclipse.jface.viewers.StyledString;

import abs.frontend.ast.ModuleDecl;
import eu.hatsproject.absplugin.navigator.ABSNavigatorStyledLabelProvider;

/**
 * Slightly altered version of the {@link ABSNavigatorStyledLabelProvider} that
 * prints the full name of ModuleDecls
 * 
 * @author cseise
 * 
 */
public class ABSWizardStyledLabelProvider extends ABSNavigatorStyledLabelProvider implements ILabelProvider {

	@Override
	protected StyledString getStyledString(Object obj) {
		StyledString styledString;
		if (obj instanceof ModuleDecl) {
			String name = ((ModuleDecl) obj).getName();

			styledString = new StyledString(name, STYLER_BLACK);
		} else {
			styledString = super.getStyledString(obj);
		}
		return styledString;
	}
}
