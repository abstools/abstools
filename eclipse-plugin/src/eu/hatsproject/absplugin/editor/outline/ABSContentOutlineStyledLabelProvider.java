/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package eu.hatsproject.absplugin.editor.outline;

import static eu.hatsproject.absplugin.util.Images.getImageForObject;

import org.eclipse.jface.viewers.ILabelProvider;
import org.eclipse.jface.viewers.StyledCellLabelProvider;
import org.eclipse.jface.viewers.StyledString;
import org.eclipse.jface.viewers.ViewerCell;
import org.eclipse.swt.graphics.Image;

import eu.hatsproject.absplugin.navigator.ModulePath;
import eu.hatsproject.absplugin.navigator.PackageAbsFile;
import eu.hatsproject.absplugin.navigator.PackageContainer;
import eu.hatsproject.absplugin.navigator.PackageEntry;
import eu.hatsproject.absplugin.util.InternalASTNode;

/**
 * The label provider is responsible for converting ASTNodes into their String
 * representations
 * 
 * @author cseise
 * 
 */
public class ABSContentOutlineStyledLabelProvider extends StyledCellLabelProvider implements ILabelProvider{

	public ABSContentOutlineStyledLabelProvider(){
		super(StyledCellLabelProvider.COLORS_ON_SELECTION);
	}


	@Override
	public void update(ViewerCell cell) {
		Object obj = cell.getElement();

		StyledString styledString = getLabel(obj);

		cell.setText(styledString.toString());
		cell.setStyleRanges(styledString.getStyleRanges());
		cell.setImage(getImage(obj));
		super.update(cell);
	}

	@Override
	public Image getImage(Object element) {
		return getImageForObject(element);
	}

	@Override
	public String getText(Object element) {
		return getLabel(element).toString();
	}
	
	public static StyledString getLabel(Object obj){
		if (obj instanceof InternalASTNode<?>){
			return ABSContentOutlineUtils.getLabel((InternalASTNode<?>)obj);
		}else if (obj instanceof ModulePath){
			return ABSContentOutlineUtils.getLabel((ModulePath)obj);			
		}else if (obj instanceof PackageContainer) {
			return ABSContentOutlineUtils.getLabel((PackageContainer)obj);
		}else if (obj instanceof PackageEntry) {
			return ABSContentOutlineUtils.getLabel((PackageEntry)obj);
		}else if (obj instanceof PackageAbsFile) {
			return ABSContentOutlineUtils.getLabel((PackageAbsFile)obj);
		}	
		return ABSContentOutlineUtils.getLabel(obj);
	}
}