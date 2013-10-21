/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.absmodels.abs.plugin.editor;

import static org.absmodels.abs.plugin.util.Constants.PARTITION_CHARACTER;
import static org.absmodels.abs.plugin.util.Constants.PARTITION_MULTI_LINE_COMMENT;
import static org.absmodels.abs.plugin.util.Constants.PARTITION_SINLGE_LINE_COMMENT;
import static org.absmodels.abs.plugin.util.Constants.PARTITION_STRING;

import org.eclipse.jface.text.rules.EndOfLineRule;
import org.eclipse.jface.text.rules.IPredicateRule;
import org.eclipse.jface.text.rules.IToken;
import org.eclipse.jface.text.rules.MultiLineRule;
import org.eclipse.jface.text.rules.RuleBasedPartitionScanner;
import org.eclipse.jface.text.rules.SingleLineRule;
import org.eclipse.jface.text.rules.Token;

public class ABSPartitionScanner extends RuleBasedPartitionScanner{
	public ABSPartitionScanner() {
		IToken singleLineCommentToken = new Token(PARTITION_SINLGE_LINE_COMMENT);
		IToken multiLineCommentToken  = new Token(PARTITION_MULTI_LINE_COMMENT);
		IToken stringToken            = new Token(PARTITION_STRING);
		IToken charToken              = new Token(PARTITION_CHARACTER);
		
		IPredicateRule[] rules = {
				new EndOfLineRule("//", singleLineCommentToken),
				new MultiLineRule("/*","*/", multiLineCommentToken, '\\', true),
				new SingleLineRule("\"", "\"", stringToken, '\\'),
				new SingleLineRule("'", "'", charToken, '\\'),
		};
		setPredicateRules(rules);
	}	
}
