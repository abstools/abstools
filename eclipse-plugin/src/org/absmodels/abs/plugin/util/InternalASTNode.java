/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.absmodels.abs.plugin.util;

import java.util.ArrayList;
import java.util.List;

import org.absmodels.abs.plugin.builder.AbsNature;
import org.absmodels.abs.plugin.navigator.ModulePath;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;

import abs.frontend.ast.ASTNode;
import abs.frontend.ast.ModuleDecl;

/**
 * Class for wrapping abs.frontend.AST.* nodes with their respective project nature
 * @author cseise
 * 
 * @param <T> the type of AST node that will be wrapped
 */
public class InternalASTNode<T extends ASTNode<?>> {
	
	private final AbsNature nature;
	private final T node;
	
	public InternalASTNode(T node, AbsNature nature){
		this.node = node;
		if (nature == null || node == null) throw new IllegalArgumentException("Null argument not allowed here!");
		this.nature = nature;
	}
	
	public T getASTNode(){
		return node;
	}
	
	public AbsNature getNature(){
		return nature;
	}
	
	/**
	 * Checks whether the InternalASTNode contains an ASTNode of the specified
	 * class
	 * 
	 * @param node
	 * @param clazz
	 * @return true if node is not null and contains an ASTNode with the
	 *         specified class, false otherwise
	 */
	public boolean hasASTNodeOfType(Class<? extends ASTNode<?>> clazz) {
		Object astNode = getASTNode();
		return (clazz.isInstance(astNode));
	}
	
	/**
	 * Wraps an array of ASTNode<?> into a List of InternalASTNodes with the specified AbsNature
	 * @param <T> The type of ASTNodes 
	 * @param list The list of ASTNodes
	 * @param nature The nature used for wrapping
	 * @return A list of InternalASTNodes
	 * @throws IllegalArgumentException if list or nature is null
	 */
	public static <T extends ASTNode<?>> List<InternalASTNode<T>> wrapASTNodes(T[] list, AbsNature nature) throws IllegalArgumentException{
		ArrayList<InternalASTNode<T>> nodes = new ArrayList<InternalASTNode<T>>();
		
		if (list == null || nature == null){
			throw new IllegalArgumentException("Null argument not allowed here.");
		}
		
		for (T n : list){
			nodes.add(new InternalASTNode<T>(n, nature));
		}
		
		return nodes;
	}

	public IPath getFileName() {
		return new Path(getASTNode().getCompilationUnit().getFileName());
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		if (getClass() != obj.getClass())
			return false;
		InternalASTNode<?> other = (InternalASTNode<?>) obj;
		if (nature == null) {
			if (other.nature != null)
				return false;
		} else if (!nature.equals(other.nature))
			return false;
		if (node == null) {
			if (other.node != null)
				return false;
		} else if (!node.equals(other.node))
			return false;
		return true;
	}
	
	@Override
	public String toString(){
		return "InternalASTNode: " + getASTNode().toString();
	}
	
	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + ((nature == null) ? 0 : nature.hashCode());
		result = prime * result + ((node == null) ? 0 : node.hashCode());
		return result;
	}

	/**
	 * Returns the project of the given node 
	 * @return the respective IProject the node is located in.
	 */
	public IProject getProject(){
		return getNature().getProject();
	}

	/**
	 * Gives the module hierarchy for a given ModuleDecl
	 * 
	 * @return An ArrayList of ModulePaths. This List will be empty if the
	 *         ModuleDecl is null
	 */
	public ArrayList<ModulePath> getParentHierarchyForModuleDecl() {

		ArrayList<ModulePath> hierarchy = new ArrayList<ModulePath>();

		String moduleName = ((ModuleDecl)getASTNode()).getName();
		String[] split = moduleName.split("\\.");

		StringBuffer work = new StringBuffer();

		for (int i = 0; i < split.length - 1; i++) {
			work.append(split[i]);
			ModulePath path = new ModulePath(getNature(), work.toString());
			hierarchy.add(path);
			work.append('.');
		}

		return hierarchy;
	}
}
