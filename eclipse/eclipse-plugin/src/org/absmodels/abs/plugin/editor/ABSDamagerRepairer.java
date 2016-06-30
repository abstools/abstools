/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.absmodels.abs.plugin.editor;

import org.eclipse.jface.text.*;
import org.eclipse.jface.text.rules.DefaultDamagerRepairer;
import org.eclipse.jface.text.rules.ITokenScanner;

import static org.absmodels.abs.plugin.util.Constants.*;

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
				return super.getDamageRegion(partition, e, documentPartitioningChanged);
			}
		}
		return partition;
	}
}
