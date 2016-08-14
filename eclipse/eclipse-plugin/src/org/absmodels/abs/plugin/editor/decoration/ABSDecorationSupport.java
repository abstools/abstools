/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.absmodels.abs.plugin.editor.decoration;

import org.eclipse.jface.text.source.Annotation;
import org.eclipse.jface.text.source.AnnotationPainter;
import org.eclipse.jface.text.source.AnnotationPainter.IDrawingStrategy;
import org.eclipse.jface.text.source.IAnnotationAccess;
import org.eclipse.jface.text.source.IOverviewRuler;
import org.eclipse.jface.text.source.ISharedTextColors;
import org.eclipse.jface.text.source.ISourceViewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.StyledText;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.graphics.FontData;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.ui.texteditor.SourceViewerDecorationSupport;

import static org.absmodels.abs.plugin.util.Constants.*;
import static org.absmodels.abs.plugin.util.UtilityFunctions.*;

/**
 * An extension of the standard {@link SourceViewerDecorationSupport} for decorating
 * the inferred ABS location types in a custom way.
 * 
 * @author mweber
 *
 */
public class ABSDecorationSupport extends SourceViewerDecorationSupport {

	
	public ABSDecorationSupport(ISourceViewer sourceViewer,
			IOverviewRuler overviewRuler, IAnnotationAccess annotationAccess,
			ISharedTextColors sharedTextColors) {
		super(sourceViewer, overviewRuler, annotationAccess, sharedTextColors);
	}
	
	@Override
	protected AnnotationPainter createAnnotationPainter() {
		AnnotationPainter painter= super.createAnnotationPainter();

		painter.addDrawingStrategy(LOCATION_TYPE_NEAR_TEXTSTYLE_VALUE, new LocationTypeDrawingStrategy("N"));
		painter.addDrawingStrategy(LOCATION_TYPE_FAR_TEXTSTYLE_VALUE, new LocationTypeDrawingStrategy("F"));
		painter.addDrawingStrategy(LOCATION_TYPE_SOMEWHERE_TEXTSTYLE_VALUE, new LocationTypeDrawingStrategy("S"));

		return painter;
	}

}


class LocationTypeDrawingStrategy implements IDrawingStrategy {
	private String symbol;
	
	public LocationTypeDrawingStrategy(String symbol) {
		this.symbol = symbol;
   }

	@Override
	public void draw(Annotation annotation, GC gc, StyledText textWidget,
			int offset, int length, Color color) {
	    Rectangle textBounds = null;
	    try {
	        textBounds = textWidget.getTextBounds(offset, offset + length - 1);
	    } catch (IllegalArgumentException e) {
	        // this can happen and is not an error
	        return;
	    }
	    if (gc != null) {
			int x1 = textBounds.x + textBounds.width;
			int y1 = textBounds.y - (textWidget.getLineHeight() / 10);

			gc.setForeground(color);
			Font font = textWidget.getFont();
			gc.setFont(getFontWithNewHeight(font, 0.6));
			gc.drawString(symbol, x1, y1, true);
		} else {
			textWidget.redraw(textBounds.x, textBounds.y, textBounds.width+textWidget.getLineHeight(), textBounds.height + textWidget.getLineHeight(), true);
		}
	}

	public static Font getFontWithNewHeight(Font font, double newHeightFact) {
      FontData[] fontData = font.getFontData();
      for (int i = 0; i < fontData.length; i++) {
      	fontData[i].setHeight((int) (fontData[i].getHeight() * newHeightFact));
      	fontData[i].setStyle(SWT.BOLD);
      }
      return new Font(font.getDevice(), fontData);
   }
}
