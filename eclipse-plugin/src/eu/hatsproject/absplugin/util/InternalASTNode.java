/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package eu.hatsproject.absplugin.util;

import java.util.ArrayList;
import java.util.List;

import abs.frontend.ast.ASTNode;
import eu.hatsproject.absplugin.builder.AbsNature;

/**
 * Class for wrapping abs.frontend.AST.* nodes with their respective project nature
 * @author cseise
 * 
 * @param <T> the type of AST node that will be wrapped
 */
public class InternalASTNode<T extends ASTNode<?>> {
	
	
	private AbsNature nature;
	private T node;
	
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
	public static boolean  hasASTNodeOfType(InternalASTNode<?> node, Class<? extends ASTNode<?>> clazz) {
		if (node != null) {
			Object astNode = node.getASTNode();
			
			return (clazz.isInstance(astNode));
		}
		
		return false;
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
	
	/**
	 * Wraps an ASTNode<?> into a InternalASTNode with the specified AbsNature
	 * @param <T> The type of ASTNodes 
	 * @param node The ASTNode
	 * @param nature The nature used for wrapping
	 * @return An InternalASTNode
	 * @throws IllegalArgumentException if node or nature is null
	 */	
	public static <T extends ASTNode<?>> InternalASTNode<T> wrapASTNode(T node, AbsNature nature) throws IllegalArgumentException{
		if (node == null || nature == null){
			throw new IllegalArgumentException("Null argument not allowed here.");
		}
		
		return new InternalASTNode<T>(node, nature);
	}
	
	/* (non-Javadoc)
	 * @see java.lang.Object#equals(java.lang.Object)
	 */
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
	
	/* (non-Javadoc)
	 * @see java.lang.Object#hashCode()
	 */
	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + ((nature == null) ? 0 : nature.hashCode());
		result = prime * result + ((node == null) ? 0 : node.hashCode());
		return result;
	}

	
	
	

}
