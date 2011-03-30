/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package eu.hatsproject.absplugin.editor;

import org.eclipse.jface.text.*;
import org.eclipse.jface.text.rules.DefaultDamagerRepairer;
import org.eclipse.jface.text.rules.ITokenScanner;

import static eu.hatsproject.absplugin.util.Constants.*;

public class ABSDamagerRepairer extends DefaultDamagerRepairer {

	public ABSDamagerRepairer(ITokenScanner scanner) {
		super(scanner);
	}
	
	@Override
	public IRegion getDamageRegion(ITypedRegion partition, DocumentEvent e, boolean documentPartitioningChanged) {
		if (!documentPartitioningChanged) {
			IDocument doc = e.getDocument();
			if(PARTITION_MULTI_LINE_COMMENT.equals(partition.getType())){
				try{
					int startline = doc.getLineOfOffset(partition.getOffset());
					int endline = doc.getLineOfOffset(partition.getOffset() + partition.getLength());
					int start = doc.getLineOffset(startline);
					int end = doc.getLineOffset(endline) + doc.getLineLength(endline);
					return new Region(start, end - start);
				} catch (BadLocationException ex){
					return partition;
				}
			} else{
				try{
					int startline = doc.getLineOfOffset(e.getOffset());
					int startoffset = doc.getLineOffset(startline);
					int endline = doc.getLineOfOffset(e.getOffset() + e.getLength());
					int endoffset = doc.getLineOffset(endline) + doc.getLineLength(endline);
					return new Region(startoffset, endoffset - startoffset);
				} catch (BadLocationException ex){
					return partition;
				}
			}
		}
		return partition;
	}
}
