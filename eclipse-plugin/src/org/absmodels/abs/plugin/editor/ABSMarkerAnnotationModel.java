/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.absmodels.abs.plugin.editor;

import static org.absmodels.abs.plugin.util.Constants.*;

import org.absmodels.abs.plugin.util.Constants;
import org.eclipse.core.resources.IMarker;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.Position;
import org.eclipse.ui.texteditor.ResourceMarkerAnnotationModel;


public class ABSMarkerAnnotationModel extends ResourceMarkerAnnotationModel {

	public ABSMarkerAnnotationModel(IResource resource) {
		super(resource);
	}

	/**
	 * the markers for the ABS plug-in have new attributes: {@link Constants#START_LINE}, {@link Constants#END_LINE},
	 * {@link Constants#START_COLUMN}, {@link Constants#END_COLUMN}. From these attributes, the position inside
	 * the document is computed here.
	 * @see org.eclipse.ui.texteditor.AbstractMarkerAnnotationModel#createPositionFromMarker(org.eclipse.core.resources.IMarker)
	 */
	@Override
	protected Position createPositionFromMarker(IMarker marker) {
		try {
			if((marker.isSubtypeOf(MARKER_TYPE) || marker.isSubtypeOf(LOCATION_TYPE_INFERENCE_MARKER_TYPE))
					&& marker.getAttribute(IMarker.CHAR_START, -1) == -1){
				int startline = marker.getAttribute(START_LINE, -1);
				if(startline != -1){
			    	int endline     = marker.getAttribute(END_LINE, -1);
			    	int startcolumn = marker.getAttribute(START_COLUMN, -1);
			    	int endcolumn   = marker.getAttribute(END_COLUMN, -1);
			    	IDocument doc   = fDocument;
			    	if(doc != null){
			    		int start = doc.getLineOffset(startline) + startcolumn;
			    		int end;
			    		if(endcolumn == -1){
			    			end = doc.getLineOffset(endline) + doc.getLineLength(endline);
			    		} else {
			    			end = doc.getLineOffset(endline) + endcolumn;
			    		}
			    		return new Position(start, end-start);
			    	}
				}
			} else if(marker.isSubtypeOf(Constants.CURRENT_INSTRUCTION_POINTER_ID)){
				int line = marker.getAttribute(IMarker.LINE_NUMBER, -1);
				if(line != -1){
					IDocument doc = fDocument;
					if(doc != null){
						int start = doc.getLineOffset(line);
						int length = doc.getLineLength(line);
						return new Position(start, length);
					}
				}
			}
		} catch (CoreException e) {
			return super.createPositionFromMarker(marker);
		} catch (BadLocationException e) {
			return super.createPositionFromMarker(marker);
		}
		return super.createPositionFromMarker(marker);
	}
	
}
