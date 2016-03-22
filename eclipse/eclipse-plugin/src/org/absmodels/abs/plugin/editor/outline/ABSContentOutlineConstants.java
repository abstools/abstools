/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.absmodels.abs.plugin.editor.outline;

import static org.absmodels.abs.plugin.util.Constants.PLUGIN_PACKAGE;
import static org.absmodels.abs.plugin.util.Constants.STYLER_BLACK;

import org.eclipse.jface.viewers.StyledString;

/**
 * Class with constants for the ABS content outline
 * @author cseise
 *
 */
public class ABSContentOutlineConstants {

	private ABSContentOutlineConstants(){
		//prevent instantiations
	}
	
	//-------------------- NAVIGATOR / OUTLINE ------------------------------------------------------------------
	public static final String HIDE_EXPORTS_COMMAND_ID = PLUGIN_PACKAGE + ".hideExportsCommand";
	public static final String HIDE_FIELD_COMMAND_ID   = PLUGIN_PACKAGE + ".hideFieldCommand";
	public static final String HIDE_IMPORTS_COMMAND_ID = PLUGIN_PACKAGE + ".hideImportsCommand";
	public static final String SORT_COMMAND_ID         = PLUGIN_PACKAGE + ".sortCommand";
	public static final String LINK_EDITOR_COMMAND_ID  = PLUGIN_PACKAGE + ".linkEditorCommand";
	
	// Default Strings
	public static final String OUTLINE_PARAM_OPEN_PARENTHESIS       = "(";
	public static final String OUTLINE_PARAM_CLOSE_PARENTHESIS      = ")";
	public static final String OUTLINE_TYPE_PARAM_OPEN_PARENTHESIS  = "<";
	public static final String OUTLINE_TYPE_PARAM_CLOSE_PARENTHESIS = ">";
	public static final String OUTLINE_DELIMITER                    = ",";
	public static final String OUTLINE_TYPE_DELIMITER               = " : ";
	public static final String OUTLINE_STAR                         = "*";

	public static final String OUTLINE_MAIN_BLOCK = "Main Block";
	public static final String OUTLINE_FROM       = " from ";
	public static final String OUTLINE_IMPORTS    = "Imports";
	public static final String OUTLINE_EXPORTS    = "Exports";

	public static final StyledString MAIN_BLOCK_STYLED_STRING     = new StyledString(OUTLINE_MAIN_BLOCK, STYLER_BLACK);
	public static final StyledString IMPORTS_BLOCK_STYLED_STRING  = new StyledString(OUTLINE_IMPORTS, STYLER_BLACK);
	public static final StyledString EXPORTS_BLOCK_STYLED_STRING  = new StyledString(OUTLINE_EXPORTS, STYLER_BLACK);
	public static final StyledString TYPE_DELIMITER_STYLED_STRING = new StyledString(OUTLINE_TYPE_DELIMITER, STYLER_BLACK);
	
	public static final String[] FILTER_COMMANDS = {HIDE_FIELD_COMMAND_ID, HIDE_IMPORTS_COMMAND_ID, HIDE_EXPORTS_COMMAND_ID};
	
	/**
	 * Helper enum for determining the annotation type of a ClassDecl.
	 * Used in
	 * <ul> 
	 *  <li>ABSContentOutline (LabelProviders)</li>
	 *  <li>ABSNavigator (LabelProvider)</li>
	 *  <li>ABS Wizards (New Class/Interface wizard)</li>
	 * </ul>
	 * @author cseise
	 *
	 */
	public static enum AnnotationType{
		/**
		 * The class is a COG class with a COG annotation
		 */
		COG_ANNOTATION ("COG"),
		/**
		 * The class is a Plain class with a plain annotation
		 */
		PLAIN_ANNOTATION ("Plain");
		
		private String AnnotationString;
		
		private AnnotationType(String AnnotationString){
			this.AnnotationString = AnnotationString;
		}
		
		public String getAnnotationString(){
			return AnnotationString;
		}
		
		@Override
		public String toString(){
			return getAnnotationString();
		}

	}

}
