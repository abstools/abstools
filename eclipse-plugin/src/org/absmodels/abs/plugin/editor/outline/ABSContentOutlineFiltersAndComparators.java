/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.absmodels.abs.plugin.editor.outline;

import static org.absmodels.abs.plugin.editor.outline.ABSContentOutlineConstants.HIDE_EXPORTS_COMMAND_ID;
import static org.absmodels.abs.plugin.editor.outline.ABSContentOutlineConstants.HIDE_FIELD_COMMAND_ID;
import static org.absmodels.abs.plugin.editor.outline.ABSContentOutlineConstants.HIDE_IMPORTS_COMMAND_ID;

import org.absmodels.abs.plugin.navigator.ModulePath;
import org.absmodels.abs.plugin.util.InternalASTNode;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.jface.viewers.ViewerComparator;
import org.eclipse.jface.viewers.ViewerFilter;
import org.eclipse.jface.viewers.ViewerSorter;

import abs.frontend.ast.ClassDecl;
import abs.frontend.ast.Export;
import abs.frontend.ast.FunctionDecl;
import abs.frontend.ast.Import;
import abs.frontend.ast.InterfaceDecl;
import abs.frontend.ast.List;
import abs.frontend.ast.MainBlock;
import abs.frontend.ast.MethodImpl;
import abs.frontend.ast.MethodSig;
import abs.frontend.ast.ModuleDecl;
import abs.frontend.ast.TypeDecl;
import abs.frontend.ast.TypedVarOrFieldDecl;

/**
 * Collection of Filters for the ABS content outline page
 * 
 * @author cseise
 * 
 */
public class ABSContentOutlineFiltersAndComparators {

	private ABSContentOutlineFiltersAndComparators() {
		//prevent instantiations
	}

	/**
	 * ViewerComparator for ASTNodes. Behaves like the normal
	 * {@link ViewerComparator}, but has a new category definition for grouped
	 * ABS outline sorting.
	 * 
	 * @author cseise
	 * 
	 */
	public static class ABSViewComparator extends ViewerSorter {
		/**
		 * {@inheritDoc}
		 */
		@Override
		public int compare(Viewer viewer, Object e1, Object e2) {
			return super.compare(viewer, e1, e2);
		}

		/**
		 * {@inheritDoc}
		 */
		@Override
		public int category(Object element) {
			if (element instanceof InternalASTNode) {
				Object node = ((InternalASTNode<?>) element).getASTNode();
				if (node instanceof FunctionDecl) {
					return 7;
				} else if (node instanceof ClassDecl) {
					return 6;
				} else if (node instanceof TypedVarOrFieldDecl) {
					return 4;
				} else if (node instanceof MethodImpl || node instanceof MethodSig) {
					return 5;
				} else if (node instanceof InterfaceDecl) {
					return 3;
				} else if (node instanceof TypeDecl) {
					return 4;
				} else if (node instanceof ModuleDecl) {
					return 0;
				} else if (node instanceof List<?>) {
					if (ABSContentOutlineUtils.isImportList((List<?>) node)) {
						return 1;
					} else if (ABSContentOutlineUtils.isExportList((List<?>) node)) {
						return 2;
					}
				} else if (node instanceof Import) {
					return 1;
				} else if (node instanceof Export) {
					return 2;
				} else if (node instanceof MainBlock) {
					return 10;
				}
			} else if (element instanceof ModulePath) {
				return 0;
			} else if (element instanceof PackageContainer) {
				return 0;
			}
			return Integer.MAX_VALUE;
		}
	}

	/**
	 * ViewerFilter definition for hiding Fields in the ABS Content Outline
	 * 
	 * @author cseise
	 * 
	 */
	private static class ABSFieldFilter extends ViewerFilter {

		@Override
		public boolean select(Viewer viewer, Object parentElement, Object element) {
			if (element instanceof InternalASTNode<?>) {
				InternalASTNode<?> node = (InternalASTNode<?>) element;
			
				if (node.hasASTNodeOfType(TypedVarOrFieldDecl.class)) {
					return false;
				} else {
					return true;
				}
			} else {
				return true;
			}

		}
	}

	/**
	 * ViewerFilter definition for hiding Imports in the ABS Content Outline
	 * 
	 * @author cseise
	 * 
	 */
	private static class ABSImportFilter extends ViewerFilter {

		@Override
		public boolean select(Viewer viewer, Object parentElement, Object element) {
			if (element instanceof InternalASTNode<?>){
				InternalASTNode<?> node = (InternalASTNode<?>) element;
			
				if (node.hasASTNodeOfType(Import.class)) {
					return false;
				} else if (node.getASTNode() instanceof List<?>) {
					if (ABSContentOutlineUtils.isImportList((List<?>) node.getASTNode())) {
						return false;
					}
				}
				
			}

			return true;
		}
	}

	/**
	 * ViewerFilter definition for hiding Exports in the ABS Content Outline
	 * 
	 * @author cseise
	 * 
	 */
	private static class ABSExportFilter extends ViewerFilter {

		@Override
		public boolean select(Viewer viewer, Object parentElement, Object element) {
			if (element instanceof InternalASTNode<?>) {

				InternalASTNode<?> node = (InternalASTNode<?>) element;

				if (node.hasASTNodeOfType(Export.class)) {
					return false;
				} else if (node.getASTNode() instanceof List<?>) {
					if (ABSContentOutlineUtils.isExportList((List<?>) node.getASTNode())) {
						return false;
					}
				}

			}

			return true;
		}
	}

	// instantiation of filter singletons
	private final static ABSFieldFilter fieldFilter = new ABSFieldFilter();
	private final static ABSImportFilter importFilter = new ABSImportFilter();
	private final static ABSViewComparator viewComparator = new ABSViewComparator();
	private final static ABSExportFilter exportFilter = new ABSExportFilter();

	/**
	 * Returns a ViewerComparator for grouped alphabetical sorting of the
	 * Content Outline
	 * 
	 * @return the ViewComparator
	 */
	public static ABSViewComparator getAlphabeticalASTNodeComparator() {
		return viewComparator;
	}

	/**
	 * Returns a ViewerFilter that can be used for hiding fields in the ABS
	 * Content Outline
	 * 
	 * @return the ViewerFilter
	 */
	public static ABSFieldFilter getFieldFilter() {
		return fieldFilter;
	}

	/**
	 * Returns a ViewerFilter that can be used for hiding imports in the ABS
	 * Content Outline
	 * 
	 * @return the ViewerFilter
	 */
	public static ABSImportFilter getImportFilter() {
		return importFilter;
	}

	/**
	 * Returns a ViewerFilter that can be used for hiding exports in the ABS
	 * Content Outline
	 * 
	 * @return the ViewerFilter
	 */
	public static ABSExportFilter getExportFilter() {
		return exportFilter;
	}

	/**
	 * Gives the a matching ViewerFilter based on the givenCommandId
	 * 
	 * @param commandId
	 * @return The matching ViewerFilter or null if there is no matching viewer
	 *         filter
	 */
	public static ViewerFilter getFilterOfCommand(String commandId) {
		if (HIDE_IMPORTS_COMMAND_ID.equals(commandId)) {

			return ABSContentOutlineFiltersAndComparators.getImportFilter();

		} else if (HIDE_EXPORTS_COMMAND_ID.equals(commandId)) {

			return ABSContentOutlineFiltersAndComparators.getExportFilter();

		} else if (HIDE_FIELD_COMMAND_ID.equals(commandId)) {

			return ABSContentOutlineFiltersAndComparators.getFieldFilter();

		} else {
			return null;
		}
	}

}
