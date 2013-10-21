/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.absmodels.abs.plugin.wizards.composites;

import static org.absmodels.abs.plugin.util.Constants.STYLER_BLACK;

import org.absmodels.abs.plugin.navigator.ABSNavigatorStyledLabelProvider;
import org.eclipse.jface.viewers.ILabelProvider;
import org.eclipse.jface.viewers.StyledString;

import abs.frontend.ast.ModuleDecl;

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
