/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.absmodels.abs.plugin.navigator;

import static org.absmodels.abs.plugin.editor.outline.ABSContentOutlineUtils.getLabel;
import static org.absmodels.abs.plugin.util.Constants.STYLER_BLACK;
import static org.absmodels.abs.plugin.util.Images.getImageForObject;

import org.absmodels.abs.plugin.util.InternalASTNode;
import org.eclipse.jface.viewers.ILabelProvider;
import org.eclipse.jface.viewers.StyledCellLabelProvider;
import org.eclipse.jface.viewers.StyledString;
import org.eclipse.jface.viewers.ViewerCell;
import org.eclipse.swt.graphics.Image;

import abs.frontend.ast.ModuleDecl;

/**
 * The label provider is responsible for converting ASTNodes into their String
 * representations
 * 
 * @author cseise
 * 
 */
public class ABSNavigatorStyledLabelProvider extends StyledCellLabelProvider implements ILabelProvider{

	@Override
	public void update(ViewerCell cell) {
		Object obj = cell.getElement();

		StyledString styledString = getStyledString(obj);

		cell.setText(styledString.toString());
		cell.setStyleRanges(styledString.getStyleRanges());
		cell.setImage(getImage(obj));
		super.update(cell);
	}

	/**
	 * Gets a styledString for the object. If the object is 
	 * <ul>
	 *  <li>an InternalASTnode&lt;ModuleDecl&gt;, the part of the name after the last '.' will be returned.</li>
	 * </ul>
	 * In all other cases {@link org.absmodels.abs.plugin.editor.outline.ABSContentOutlineUtils#getLabel(ModulePath)} will be used.
	 * @param obj the Object whose String representation should be returned
	 * @return A String representation of obj
	 */
	protected StyledString getStyledString(Object obj) {
		StyledString styledString;
		if (obj instanceof InternalASTNode) {
			InternalASTNode<?> node = (InternalASTNode<?>) obj;
			if (node.getASTNode() instanceof ModuleDecl) {
				String name = ((ModuleDecl) node.getASTNode()).getName();
				String lastName;

				if (name.indexOf('.') > 0) {
					lastName = name.substring(name.lastIndexOf('.') + 1, name.length());
				} else {
					lastName = name;
				}
				styledString = new StyledString(lastName, STYLER_BLACK);
			} else {
				styledString = getLabel(node);
			}
		} else if (obj instanceof ModulePath){
			styledString = getLabel((ModulePath)obj);
		}else {
			styledString = getLabel(obj);
		}
		return styledString;
	}

	@Override
	public Image getImage(Object element) {
		return getImageForObject(element);
	}

	@Override
	public String getText(Object element) {
		return getStyledString(element).toString();
	}
}