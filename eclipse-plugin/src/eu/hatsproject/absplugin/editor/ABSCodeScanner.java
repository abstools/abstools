/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package eu.hatsproject.absplugin.editor;

import static eu.hatsproject.absplugin.util.Constants.*;
import static eu.hatsproject.absplugin.util.UtilityFunctions.*;

import java.io.File;
import java.io.StringReader;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.Assert;
import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.jface.preference.PreferenceConverter;
import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.TextAttribute;
import org.eclipse.jface.text.rules.*;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.widgets.Display;

import abs.frontend.ast.*;
import abs.frontend.parser.Main;
import abs.frontend.parser.ParseException;
import abs.frontend.typechecker.KindedName;
import abs.frontend.typechecker.KindedName.Kind;
import eu.hatsproject.absplugin.builder.AbsNature;

/**
 * scans the document and analyzes each token for it's semantic.
 * If an ABS AST exists for the document/file it parses, this
 * information is used as a basis for the analytic step.
 * This class is the main part of the semantic (and syntax) highlighting.
 * 
 * @author mweber
 *
 */
public class ABSCodeScanner implements ITokenScanner {
	private static final boolean doDebug = false;
	
	protected static final int UNDEFINED = -1;
	protected static final int EOF = -1;
   private static final IToken DEFAULT_RETURN_TOKEN = new Token(null);

   private IDocument fDocument;
	private int fOffset;
	private int fRangeEnd;
	private char[][] fDelimiters;
;
	private int fTokenOffset;
	
	private final IToken keywordToken;
	
	private Set<String> keywords = new HashSet<String>();
	private ArrayList<RegexRule> wordrules = new ArrayList<RegexRule>();
	private ArrayList<SemanticRule> semanticrules = new ArrayList<SemanticRule>();

	private CompilationUnit compilationUnit;
	
	private IWordDetector fDetector;
	private StringBuffer fBuffer = new StringBuffer();
	private ABSEditor fEditor;
	private IPreferenceStore preferencestore;
	
	private static final boolean doSemanticHighlighting = true;
	
	public class RegexRule{
		private IToken token;
		private String rule;
		private Pattern pattern;

		public IToken getToken() {
			return token;
		}

		public Pattern getPattern() {
			return pattern;
		}
		
		public String getRule() {
			return rule;
		}

		/**
		 * build a RegexRule out of a regex and a token
		 * @param rule the regex
		 * @param token the token to use if the regex matches
		 */
		public RegexRule(String rule, IToken token) {
			this.rule = rule;
			this.token = token;
			this.pattern = Pattern.compile(rule);
		}
	}
	
	/**
	 * Rules for semantic highlighting (mapping between ASTNode-class and token)
	 * 
	 * @author mweber
	 *
	 */
	public class SemanticRule{
		private IToken token;
		private Class<? extends ASTNode<?>> semclass;
		
		public IToken getToken() {
			return token;
		}

		public Class<? extends ASTNode<?>> getSemclass() {
			return semclass;
		}

		public SemanticRule(Class<? extends ASTNode<?>> semclass, IToken token) {
			this.semclass = semclass;
			this.token = token;
		}
	}
	
	
	public ABSCodeScanner(IWordDetector detector, ABSEditor editor) {
		this.fDetector = detector;
		this.fEditor = editor;
		
		preferencestore = getDefaultPreferenceStore();
		Color keycolor = new Color(Display.getCurrent(),
				PreferenceConverter.getColor(preferencestore, SYNTAXCOLOR_COLOR + SYNTAXCOLOR_KEYWORD));
		keywordToken = new Token(new TextAttribute(keycolor, null,
				computeAttributes(preferencestore, SYNTAXCOLOR_KEYWORD)));
		
		for(String keyword : abs.frontend.parser.Keywords.getKeywords()){
			keywords.add(keyword);
		}
		
		initRegexRules();
		initSemanticRules();
	}

	public void initRegexRules() {
		Color funcolor = new Color(Display.getCurrent(),
				PreferenceConverter.getColor(preferencestore, SYNTAXCOLOR_COLOR + SYNTAXCOLOR_FUNCTION));
		IToken funtoken = new Token(new TextAttribute(funcolor, null,
				computeAttributes(preferencestore, SYNTAXCOLOR_FUNCTION)));
		
		Color typecolor = new Color(Display.getCurrent(),
				PreferenceConverter.getColor(preferencestore, SYNTAXCOLOR_COLOR + SYNTAXCOLOR_DATATYPE));
		IToken typetoken = new Token(new TextAttribute(typecolor, null,
				computeAttributes(preferencestore, SYNTAXCOLOR_DATATYPE)));
		
		String NO_WORD = "[^A-Za-z0-9_]*";
		String WORD = "[A-Za-z0-9_]*";
		String UPPERCASE_WORD = "[A-Z]" + WORD;
		String LOWERCASE_WORD = "[a-z]" + WORD;
		
		wordrules.add(new RegexRule(NO_WORD + "(" + UPPERCASE_WORD + ")[(]?.*", typetoken));
		wordrules.add(new RegexRule(NO_WORD + "(" + LOWERCASE_WORD + ")[(\\[].*", funtoken));
		wordrules.add(new RegexRule(NO_WORD + "(" + LOWERCASE_WORD + ")[^\\(].*", Token.UNDEFINED));
	}
	
	public void initSemanticRules(){
		Color color;
		IToken token;
		
		color = new Color(Display.getCurrent(),
				PreferenceConverter.getColor(preferencestore, SYNTAXCOLOR_COLOR + SYNTAXCOLOR_VAR));
		token = new Token(new TextAttribute(color, null,
				computeAttributes(preferencestore, SYNTAXCOLOR_VAR)));
		semanticrules.add(new SemanticRule(VarDecl.class, token));
		
		color = new Color(Display.getCurrent(),
				PreferenceConverter.getColor(preferencestore, SYNTAXCOLOR_COLOR + SYNTAXCOLOR_PARAM));
		token = new Token(new TextAttribute(color, null,
				computeAttributes(preferencestore, SYNTAXCOLOR_PARAM)));
		semanticrules.add(new SemanticRule(ParamDecl.class, token));
		
		color = new Color(Display.getCurrent(),
				PreferenceConverter.getColor(preferencestore, SYNTAXCOLOR_COLOR + SYNTAXCOLOR_FIELD));
		token = new Token(new TextAttribute(color, null,
				computeAttributes(preferencestore, SYNTAXCOLOR_FIELD)));
		semanticrules.add(new SemanticRule(FieldDecl.class, token));
		
		color = new Color(Display.getCurrent(),
				PreferenceConverter.getColor(preferencestore, SYNTAXCOLOR_COLOR + SYNTAXCOLOR_FUNCTION));
		token = new Token(new TextAttribute(color, null,
				computeAttributes(preferencestore, SYNTAXCOLOR_FUNCTION)));
		semanticrules.add(new SemanticRule(FnApp.class, token));
		
		color = new Color(Display.getCurrent(),
				PreferenceConverter.getColor(preferencestore, SYNTAXCOLOR_COLOR + SYNTAXCOLOR_DATATYPE));
		token = new Token(new TextAttribute(color, null,
				computeAttributes(preferencestore, SYNTAXCOLOR_DATATYPE)));
		semanticrules.add(new SemanticRule(DataTypeDecl.class, token));
		
		color = new Color(Display.getCurrent(),
				PreferenceConverter.getColor(preferencestore, SYNTAXCOLOR_COLOR + SYNTAXCOLOR_INTERFACE));
		token = new Token(new TextAttribute(color, null,
				computeAttributes(preferencestore, SYNTAXCOLOR_INTERFACE)));
		semanticrules.add(new SemanticRule(InterfaceDecl.class, token));
		
		color = new Color(Display.getCurrent(),
				PreferenceConverter.getColor(preferencestore, SYNTAXCOLOR_COLOR + SYNTAXCOLOR_CONSTRUCTOR));
		token = new Token(new TextAttribute(color, null,
				computeAttributes(preferencestore, SYNTAXCOLOR_CONSTRUCTOR)));
		semanticrules.add(new SemanticRule(DataConstructorExp.class, token));
	}
	
	/**
	 * Compute the attributes like bold, italic, underline and strike through for the given key-postfix
	 * @param store the preference store the attributes are stored in
	 * @param postfix the postfix of the key. It is combined with e.g. {@link Constants.SYNTAXCOLOR_BOLD}
	 * @return the int corresponding to the set of attributes
	 */
	private static int computeAttributes(IPreferenceStore store, String postfix) {
		int funattr = 0;
		//bold
		boolean attrbold = store.getBoolean(SYNTAXCOLOR_BOLD + postfix);
		if(attrbold) funattr = funattr | SWT.BOLD;
		//italic
		boolean attritalic = store.getBoolean(SYNTAXCOLOR_ITALIC + postfix);
		if(attritalic) funattr = funattr | SWT.ITALIC;
		//underline
		boolean attrunderline = store.getBoolean(SYNTAXCOLOR_UNDERLINE + postfix);
		if(attrunderline) funattr = funattr | TextAttribute.UNDERLINE;
		//strike through
		boolean attrstrikethrough = store.getBoolean(SYNTAXCOLOR_STRIKETHROUGH + postfix);
		if(attrstrikethrough) funattr = funattr | TextAttribute.STRIKETHROUGH;
		return funattr;
	}
	
	/**
	 * Gets called before the first {@link #nextToken()}. Parses the current document.
	 * 
	 * @see org.eclipse.jface.text.rules.ITokenScanner#setRange(org.eclipse.jface.text.IDocument, int, int)
	 */
	@Override
	public void setRange(IDocument document, int offset, int length) {
		Assert.isLegal(document != null);
		final int documentLength= document.getLength();
		checkRange(offset, length, documentLength);

		fDocument = document;
		fOffset   = offset;
		fRangeEnd = offset + length;

		String[] delimiters = fDocument.getLegalLineDelimiters();
		fDelimiters= new char[delimiters.length][];
		for (int i= 0; i < delimiters.length; i++)
			fDelimiters[i] = delimiters[i].toCharArray();

		
		String doccontent = document.get();
		try {
			compilationUnit = null;
			Main absParser = new Main();
			absParser.setTypeChecking(false);
			compilationUnit = absParser.parseUnit(
					new File(fEditor.getEditorInput().getName()),
					doccontent,
					new StringReader(doccontent));
		} catch (ParseException e) {
			if (doDebug)
				e.printStackTrace();
		} catch (Exception e) {
			if (doDebug)
				e.printStackTrace();
		}
	}
	
	private void checkRange(int offset, int length, int documentLength) {
		Assert.isLegal(offset > -1);
		Assert.isLegal(length > -1);
		Assert.isLegal(offset + length <= documentLength);
	}

	@Override
	public IToken nextToken() {
	   try {
	      return internalNextToken();
	   } catch (RuntimeException e) {
	      if (doDebug)
	         e.printStackTrace();
	      return DEFAULT_RETURN_TOKEN;
	   }
	}

   private IToken internalNextToken() {
      fTokenOffset = fOffset;

		int c = read();
		if (c == ICharacterScanner.EOF) {
		    return Token.EOF;
		}
		if (fDetector.isWordStart((char) c)) {
			fBuffer.setLength(0);
			// read a complete word
			do {
				fBuffer.append((char) c);
				c= read();
			} while (c != ICharacterScanner.EOF && fDetector.isWordPart((char) c));
			unread();
			
			IToken token;
			token = matchKeywords();
			if(!token.isUndefined())
				return token;
			if(doSemanticHighlighting){
				token = evalAST();
				if(token == null)
					token = matchRegex();
			} else {
				token = matchRegex();
			}
			if (!token.isUndefined()) {
				return token;
			} else if (c == ICharacterScanner.EOF) {
			    return Token.EOF;
			}
		} 
		//jump to next identifier
		while(c != ICharacterScanner.EOF && !fDetector.isWordStart((char) c)){
			c = read();
		}
		unread();
		return DEFAULT_RETURN_TOKEN;
   }

	private IToken matchKeywords() {
		String buffer = fBuffer.toString();
		IToken token = Token.UNDEFINED;
		if(keywords.contains(buffer))
			token = keywordToken;
		return token;
	}

	/**
	 * Takes the AST-node for the current identifier, resolves uses (like e.g. type use) and
	 * matches against the semantic rules
	 * @return null, if the document could not be parsed, else the token of the matching semantic rule
	 * or {@link Token#UNDEFINED} if no rule matched.
	 */
	private IToken evalAST() {
		if(compilationUnit == null){
			return null;
		}
		
		IToken token = Token.UNDEFINED;
		
		IDocument doc = fEditor.getDocumentProvider().getDocument(fEditor.getEditorInput());
		
		try {
			IFile file = (IFile)fEditor.getEditorInput().getAdapter(IFile.class);
			AbsNature nature = getAbsNature(file);
			if (nature == null)
			   return token;
			
			synchronized (nature.modelLock) {
				final ASTNode<?> currentNode = getASTNodeOfOffset(doc, compilationUnit, fTokenOffset);
				Model typecheckedModel = nature.getCompleteModel();
				if(typecheckedModel == null)
					return null;
				
				Class<?> semClass = null;
				ASTNode<?> declNode = null;
				
				if(currentNode instanceof TypeUse){
					TypeUse tu = (TypeUse)currentNode;
					declNode = resolveTypeUse(tu, typecheckedModel);
				}
				
				if (currentNode instanceof VarOrFieldUse) {
					VarOrFieldUse vofu = (VarOrFieldUse) currentNode;
					declNode = vofu.getDecl();
				    if (declNode != null && declNode.getContextMethod() == null) {
				    	semClass = FieldDecl.class;
				    }
				}
				
				if (currentNode instanceof FnApp ||
					currentNode instanceof DataConstructorExp) {
					semClass = currentNode.getClass();
				}
				
				if (semClass == null) {
					if (declNode != null) {
						semClass = declNode.getClass();
					} 
				}
				
				if(semClass != null){
					Iterator<SemanticRule> iter = semanticrules.iterator();
					// iterate over rules and try to match
					while (iter.hasNext()) {
						SemanticRule rule = iter.next();
						if(rule.getSemclass().isAssignableFrom(semClass)){
							token = rule.getToken();
							break;
						}
					}
				}
			}
		} catch (BadLocationException e) {
			if(doDebug)
				e.printStackTrace();
		}
		return token;
	}

	/**
	 * resolves the type use in the type checked model.
	 * @param tu
	 * @param typeCheckedModel
	 * @return the declaration or <b>null</b> if a {@link RuntimeException} occurs
	 */
	public ASTNode<?> resolveTypeUse(TypeUse tu, Model typeCheckedModel) {
		try {
			String tuname = tu.getName();
			ModuleDecl moduleDecl = tu.getModuleDecl();
			ModuleDecl typecheckedMDecl = typeCheckedModel.lookupModule(moduleDecl.getName());
			if(typecheckedMDecl == null)
				return tu;
			ASTNode<?> declNode = typecheckedMDecl.lookup(new KindedName(Kind.TYPE_DECL, tuname));
			if (declNode instanceof TypeSynDecl) {
				TypeSynDecl tsd = (TypeSynDecl) declNode;
				return resolveTypeUse(tsd.getValue(),typeCheckedModel);
			}
			return declNode;
		} catch (RuntimeException e) {
			// if any runtime exception is thrown in the debugger, the complete eclipse explodes.
			// better make sure that can not happen
			if(doDebug)
				e.printStackTrace();
			return null;
		}
	}

	private boolean isLineDelimiter(char c){
		for(char[] delim : fDelimiters){
			if(delim[0] == c){
				return true;
			}
		}
		return false;
	}

	/**
	 * matches the current identifier against the regex rules.
	 * @return the token of the matching regex rule or {@link Token#UNDEFINED} if no rule matches
	 */
	private IToken matchRegex() {
		String identifier = fBuffer.toString();
		IToken token = Token.UNDEFINED;
		StringBuffer sbuffer = new StringBuffer();
		int offset = fTokenOffset;
		int c;
		try{
			while(offset > 0 && !fDetector.isWordPart((char)(c = fDocument.getChar(--offset)))){
				sbuffer.append((char) c);
			}
			sbuffer.reverse();
			sbuffer.append(fBuffer);
			
			offset = fOffset;
			c = fDocument.getChar(offset);
			do {
				sbuffer.append((char) c);
				offset++;
				c = fDocument.getChar(offset);
			} while (c != ICharacterScanner.EOF && !fDetector.isWordPart((char) c) && !isLineDelimiter((char)c));
			String buffer = sbuffer.toString();
			
			Iterator<RegexRule> iter = wordrules.iterator();
			Pattern pat;
			// iterate over rules and try to match
			while (iter.hasNext()) {
				RegexRule rule = iter.next();
				pat = rule.getPattern();
				Matcher m = pat.matcher(buffer);
				if(m.matches() && identifier.equals(m.group(1))){
					token = rule.getToken();
					break;
				}
			}
		} catch(BadLocationException ex){
			if(doDebug)
				ex.printStackTrace();
		}
		return token;
	}

	@Override
	public int getTokenOffset() {
		return fTokenOffset;
	}

	@Override
	public int getTokenLength() {
		if (fOffset < fRangeEnd)
			return fOffset - getTokenOffset();
		return fRangeEnd - getTokenOffset();
	}
	
	/**
	 * reads one character from the current document and advances the internal offset accordingly
	 */
	public int read() {
		try {
			if (fOffset < fRangeEnd) {
				try {
					return fDocument.getChar(fOffset);
				} catch (BadLocationException e) {
					if(doDebug)
						e.printStackTrace();
				}
			}
			return EOF;
		} finally {
			++ fOffset;
		}
	}

	public void unread() {
    	--fOffset;
	}
	
}