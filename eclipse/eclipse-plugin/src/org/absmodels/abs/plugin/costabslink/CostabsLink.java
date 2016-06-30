package org.absmodels.abs.plugin.costabslink;

import java.util.ArrayList;

import org.absmodels.abs.plugin.builder.AbsNature;

import abs.frontend.ast.ASTNode;

/**
 * This class is used by Costabs plugin in the case it is installed.
 * Costabs check this class to take info from the ABS editor.
 * If costabs plugin is not installed, the ABS editor plugin can work
 * without dependencies in costabs plugin.
 */
public class CostabsLink {

	/**
	 * ABS editor disable some markers that need to be activated in 
	 * org.abs-models.abs.plugin.editor.ABSSourceViewerConfiguration.
	 * (getTextHover and getAnnotationHover)
	 * The costabs upper bound marker is taken from this String.
	 */
	public static String MARKER_UB = "CostabsPlugin.costabs.marker";
	
	public static AbsNature ABS_NATURE;
	
	/**
	 * Costabs uses the Outline view from ABS editor to take the methods
	 * and functions selected. This methods and functions are stored in these 
	 * arrays with the line number in the source code. That way, we can put 
	 * a marker next to the method/function.
	 */
	public static ArrayList<String> ENTRIES_STRINGS = new ArrayList<String>();
	
	public static ArrayList<ASTNode<?>> ENTRIES_NODES = new ArrayList<ASTNode<?>>();
	
	public static ArrayList<Integer> LINE_ITEMS = new ArrayList<Integer>();
	
}
