/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.absmodels.abs.plugin.editor.contentassist;

import static org.absmodels.abs.plugin.util.UtilityFunctions.isIdentifierChar;

import java.util.ArrayList;
import java.util.List;

import org.absmodels.abs.plugin.editor.ABSEditor;
import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.ITextViewer;
import org.eclipse.jface.text.contentassist.ICompletionProposal;
import org.eclipse.jface.text.contentassist.IContentAssistProcessor;
import org.eclipse.jface.text.contentassist.IContextInformation;
import org.eclipse.jface.text.contentassist.IContextInformationValidator;


/**
 * creates the proposals the user is presented with when pressing ctrl-space
 * 
 * @author mweber, tfischer
 */
public class ABSCompletionProcessor implements IContentAssistProcessor {

	private ABSEditor editor;
	
	/**
	 * The Qualifier is the part of the string the user entered before hitting Ctrl-Space
	 * @author mweber
	 */
	static class Qualifier{
		private int offset;
		private String qualifier;
		
		public int getOffset() {
			return offset;
		}

		public String getQualifier() {
			return qualifier;
		}

		public Qualifier(int offset, String qualifier){
			this.offset = offset;
			this.qualifier = qualifier;
		}
	}
	
	public ABSCompletionProcessor(ABSEditor editor){
		this.editor = editor;
	}
	
	@Override
	public ICompletionProposal[] computeCompletionProposals(ITextViewer viewer, int documentOffset) {
		if(viewer == null){
			throw new IllegalArgumentException("The Textviewer may not be null!");
		}
		
		IDocument doc = viewer.getDocument();
		
		List<ICompletionProposal> proposals = new ArrayList<ICompletionProposal>();
		
		//Retrieve so far typed qualifier to restrict proposals
		Qualifier qualifier = getQualifier(doc, documentOffset);

		//Compute completion proposals based on so far typed qualifier and position of cursors
		new ProposalFactory(qualifier, doc, editor, proposals).computeStructureProposals();
		
		ICompletionProposal[] p = new ICompletionProposal[proposals.size()];
		return proposals.toArray(p);
	}

	/**
	 * Returns so far typed String to restrict content assistant proposals.
	 * @param doc
	 * @param documentOffset
	 * @return String typed in by the user so far
	 */
	private Qualifier getQualifier(IDocument doc, int documentOffset) {
		StringBuffer buf = new StringBuffer();
		while (true) {
			try {
				//Read last character
				char c = doc.getChar(--documentOffset);
				
				//End of word has been reached.
				if (!isIdentifierChar(c))
					return new Qualifier(documentOffset+1, buf.reverse().toString());

				buf.append(c);
			} catch (BadLocationException e) {
				return new Qualifier(documentOffset, "");
			}
		}
	}
	
	@Override
	public IContextInformation[] computeContextInformation(ITextViewer arg0, int arg1) {
		return null;
	}

	@Override
	public char[] getCompletionProposalAutoActivationCharacters() {
		return new char[]{'.','!'};
	}

	@Override
	public char[] getContextInformationAutoActivationCharacters() {
		return null;
	}

	@Override
	public IContextInformationValidator getContextInformationValidator() {
		return null;
	}

	@Override
	public String getErrorMessage() {
		return null;
	}
}
