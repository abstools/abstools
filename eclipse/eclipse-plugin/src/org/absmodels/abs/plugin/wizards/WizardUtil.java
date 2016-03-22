/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.absmodels.abs.plugin.wizards;

import java.util.Arrays;
import java.util.List;

import org.absmodels.abs.plugin.editor.ABSEditor;
import org.absmodels.abs.plugin.util.InternalASTNode;
import org.absmodels.abs.plugin.util.UtilityFunctions;
import org.absmodels.abs.plugin.util.UtilityFunctions.EditorPosition;
import org.eclipse.core.runtime.Assert;
import org.eclipse.core.runtime.IPath;
import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.TextSelection;
import org.eclipse.ui.PlatformUI;

import abs.frontend.ast.ClassDecl;
import abs.frontend.ast.Decl;
import abs.frontend.ast.InterfaceDecl;
import abs.frontend.ast.MainBlock;
import abs.frontend.ast.ModuleDecl;

/**
 * Utility class used by all ABS wizards
 * 
 * @author cseise
 * 
 */
public class WizardUtil {

	private WizardUtil() {
		// no instantiations allowed
	}

	// Wizard Error Messages

	/**
	 * Enumerations for describing wizard error conditions
	 */
	public static enum ErrorType {
		NO_ERROR(null), 
		ERROR_NO_NAME("Empty name field."), 
		ERROR_DOT_END("The name must not end with a '.'!"), 
		ERROR_DUPLICATE_NAME("Duplicate name: "), 
		ERROR_KEYWORD("The name is a reserved keyword."), 
		ERROR_NO_VALID_FILE("Please select a valid ABS file."), 
		ERROR_NO_VALID_MODULE("Please select a valid ABS module."),
		ERROR_NO_UPPER_CASE("The name must begin with an upper case character!"),
		ERROR_INVALID_NAME("Please enter a valid name.");

		private String description;
		private String informationString = "";

		ErrorType(String desc) {
			description = desc;
		}

		/**
		 * @return returns a description of the error condition
		 */
		public String getDescription() {
			return description;
		}

		/**
		 * Sets the new information String used for detailing the error message.
		 * Currently, the information String is only used to store the name of a
		 * duplicate module
		 * 
		 * @param info
		 *            The new information string
		 */
		public void setInformationString(String info) {
			this.informationString = info;
		}

		/**
		 * Returns additional information to the error condition.
		 * 
		 * Currently, the information String is only used to store the name of a
		 * duplicate module
		 * 
		 * @return Additional information to the error condition or empty String
		 *         if there are no additional information
		 * 
		 */
		public String getInformationString() {
			return informationString;
		}
	}

	/**
	 * Enumeration for determining the strings that have to be inserted on the creation of new 
	 * <ul>
	 * <li>Modules</li>
	 * <li>Classes</li>
	 * <li>Interfaces</li>
	 * </ul> 
	 *
	 */
	public static enum InsertType{
		INSERT_MODULE("\nmodule ",";\n\t",11),
		INSERT_CLASS("\n class ","\n { \n\t \n }\n",14),
		INSERT_INTERFACE("\n interface ","\n { \n\t \n }\n",18);

		String insertBN;
		String insertAN;
		private int    insertOff;

		InsertType(String insertBeforeName, String insertAfterName, int insertOffset){
			insertBN  = insertBeforeName;
			insertAN  = insertAfterName;
			insertOff = insertOffset; 
		}

		public int getInsertOffset() {
			return insertOff;
		}

		public int getInsertOffset(String name) {
			return (name == null) ? getInsertOffset() : insertOff + name.length();
		}

		public String getInsertBeforeName() {
			return insertBN;
		}

		public String getInsertAfterName() {
			return insertAN;
		}

		public String getInsertionString(String name){
			return insertBN + name + insertAN;
		}
	}

	private static final List<String> keywordList = Arrays.asList(abs.frontend.parser.Keywords.getKeywords());

	private static final boolean isKeyword(String str){		
		return keywordList.contains(str);
	}

	/**
	 * Method for validating a module name.<p>
	 * <br/>
	 * A valid module name  
	 * <ul>
	 *  <li>does not contain invalid characters</li>
	 * 	<li>is not empty</li>
	 *  <li>begins with a upper case letter</li>
	 *  <li>is not an ABS keyword</li>
	 *  <li>the name does not end with a '.'</li>
	 * </ul>
	 * @param name
	 *            The module name that is to be validated
	 * @return ErrorType.NO_ERROR if the name is valid and other ErrorType enum
	 *         members for the corresponding error condition. 
	 *  
	 * @see org.absmodels.abs.plugin.wizards.WizardUtil.ErrorType
	 */
	public static ErrorType validateModule(String name) {

		if ("".equals(name)) {
			return ErrorType.ERROR_NO_NAME;
		}

		boolean matches = name.matches("[A-Z a-z _ \\.]*");

		if (WizardUtil.isKeyword(name)) {
			return ErrorType.ERROR_KEYWORD;
		}else if (name.length() > 1 && name.charAt(name.length() - 1) == '.') {
			return ErrorType.ERROR_DOT_END;
		}else if (matches && name.length() > 0 && Character.getType(name.charAt(0)) != Character.UPPERCASE_LETTER) {
			return ErrorType.ERROR_NO_UPPER_CASE;
		}else if (!matches) {
			return ErrorType.ERROR_INVALID_NAME;
		}

		return ErrorType.NO_ERROR;
	}

	/**
	 * Method for validating a class or interface name.<p>
	 * 
	 * A valid class or interface name  
	 * <ul>
	 *  <li>only contains letters or digits as defined by {@link java.lang.Character#isLetterOrDigit(char)}</li>
	 * </ul>
	 * @param name
	 *            The class or interface name that is to be validated
	 * @return ErrorType.NO_ERROR if the name is valid and the ErrorType
	 *         ERROR_INVALID_NAME if the name is invalid.
	 * 
	 * @see org.absmodels.abs.plugin.wizards.WizardUtil.ErrorType
	 */
	public static ErrorType validate(String name) {

		if ("".equals(name))
			return ErrorType.ERROR_NO_NAME;

		if (isKeyword(name)) {
			return ErrorType.ERROR_KEYWORD;
		}

		char[] charName = name.toCharArray();

		for (int i = 0; i < charName.length; i++) {
			if (!Character.isLetterOrDigit(charName[i])) {
				return ErrorType.ERROR_INVALID_NAME;
			} else if (i == 0 && !Character.isUpperCase(charName[i])) {
				return ErrorType.ERROR_NO_UPPER_CASE;
			}

		}

		return ErrorType.NO_ERROR;
	}

	/**
	 * Same as {@link #validate(String)} but additionally checks, whether the specified
	 * class name already exists in the given module declaration
	 * 
	 * @param text
	 *            Name to be checked.
	 * @param decl
	 *            Target ModuleDecl.
	 * @return In case of a duplicate name, this method returns
	 *         ErrorType.ERROR_NO_DUPLICATE_NAME with the duplicate name set in
	 *         its information string.
	 */
	public static ErrorType validateClass(String text, ModuleDecl decl) {
		if (decl != null) {
			for (Decl d : decl.getDecls()) {
				if (d instanceof ClassDecl && d.getName().equals(text)) {
					ErrorType type = ErrorType.ERROR_DUPLICATE_NAME;
					type.setInformationString(text);
					return type;
				}
			}
		}
		return validate(text);
	}

	/**
	 * Same as {@link #validate(String)} but additionally checks, whether the specified
	 * interface name already exists in the given module declaration
	 * 
	 * @param text
	 *            Name to be checked.
	 * @param decl
	 *            Target ModuleDecl.
	 * @return In case of a duplicate name, this method returns
	 *         ErrorType.ERROR_NO_DUPLICATE_NAME with the duplicate name set in
	 *         its information string.
	 */
	public static ErrorType validateInterface(String text, ModuleDecl decl){
		if (decl != null) {
			for (Decl d : decl.getDecls()) {
				if (d instanceof InterfaceDecl && d.getName().equals(text)) {					
					ErrorType type = ErrorType.ERROR_DUPLICATE_NAME;
					type.setInformationString(text);
					return type; 
				}
			}
		}		
		return validate(text);
	}

	/**
	 * Determines the insertion position for a new interface/class declaration.
	 * If the given ModuleDecl has a MainBlock the insertion point is right
	 * before the main block. Otherwise, the insertion point is at the end of
	 * the ModuleDecl.
	 * 
	 * @param d
	 *            The target document
	 * @param astNode
	 *            The target ModuleDecl
	 * @return the insertion position of the new class or interface.
	 * @throws BadLocationException
	 *             if the location of the module declaration or the respective
	 *             main block could not be found
	 */
	public static int getInsertionPosition(IDocument d, InternalASTNode<ModuleDecl> astNode) throws BadLocationException {
		Assert.isNotNull(d);
		Assert.isNotNull(astNode);
		final ModuleDecl m = astNode.getASTNode();
		Assert.isNotNull(m,"ModuleDecl argument is null!");

		final EditorPosition position;
		/* Classes and interfaces go before product lines / products in the grammar.
		 * We don't want to generate invalid input for the user.
		 */
		if (m.hasBlock()) {
				MainBlock block = m.getBlock();
				position = UtilityFunctions.getPosition(block);
				return d.getLineOffset(position.getLinestart()) + position.getColstart();
			} else {
				position = UtilityFunctions.getPosition(m);
				return d.getLineOffset(position.getLineend()) + position.getColend();
			}
		}

	/**
	 * Helper method for formatting the error message of an ErrorType
	 * @param err
	 * @return String representation of the given ErrorType
	 */
	public static String getErrorDescription(ErrorType err){
		String errorMessage = err.getDescription();
		if (err == ErrorType.ERROR_DUPLICATE_NAME){
			errorMessage += err.getInformationString();				
		}

		return errorMessage;
	}

	/**
	 * Saves the editor and sets the selection to the offset insertOffset
	 * @param editor
	 * @param insertOffset
	 */
	protected static void saveEditorAndGotoOffset(ABSEditor editor, int insertOffset) {
		PlatformUI.getWorkbench().getActiveWorkbenchWindow().getActivePage().saveEditor(editor, false);
		editor.getSelectionProvider().setSelection(new TextSelection(insertOffset, 0));
	}

	/**
	 * Gets the editor for a specific ModuleDecl which is wrapped in an InternalASTNode
	 * @param m The target ModuleDecl
	 * @return The according {@link ABSEditor}
	 */
	protected static ABSEditor getEditorForModuleDecl(InternalASTNode<ModuleDecl> m) {
		if (m != null) {
			IPath path = UtilityFunctions.getPathOfModuleDecl(m);
			return UtilityFunctions.openABSEditorForFile(path, m.getProject());
		}

		return null;
	}

	static class EditorData {
		ABSEditor editor;
		IDocument document;
	}

	/**
	 * Returns the current IDocument. ONLY WORKS BECAUSE WE JUST OPENED IT!
	 */
	static EditorData getDocumentForModuleDecl(InternalASTNode<ModuleDecl> m) {
		assert m != null;
		EditorData data = new EditorData();
		data.editor = getEditorForModuleDecl(m);
		if (data.editor != null)
			data.document = data.editor.getDocumentProvider().getDocument(data.editor.getEditorInput());
		return data;
	}
}
