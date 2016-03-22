/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.absmodels.abs.plugin.testexpressions.navigator;

import org.absmodels.abs.plugin.util.InternalASTNode;
import org.eclipse.core.expressions.PropertyTester;

import abs.frontend.ast.ModuleDecl;
import abs.frontend.ast.Product;

/**
 * Tester class for testing if an InternalASTNode contains a ModuleDecl AST node
 * @author cseise
 *
 */
public class InternalASTNodeTester extends PropertyTester {

	private static final String IS_MODULE_NODE_PROPERTY = "isModuleNode";
	private static final String IS_PRODUCT_NODE_PROPERTY = "isProductNode";
	
	@Override
	public boolean test(Object receiver, String property, Object[] args, Object expectedValue) {
		if (IS_MODULE_NODE_PROPERTY.equals(property)) {
			if (receiver instanceof InternalASTNode<?>) {
				Object astNode = ((InternalASTNode<?>) receiver).getASTNode();
				boolean isModuleDecl = astNode instanceof ModuleDecl;
				//If there is no expected value assume true, else assume the value of expected value
				return expectedValue == null ? isModuleDecl : ((Boolean) expectedValue) == isModuleDecl;
			}
		} else if (IS_PRODUCT_NODE_PROPERTY.equals(property)) {
			if (receiver instanceof InternalASTNode<?>) {
				Object astNode = ((InternalASTNode<?>) receiver).getASTNode();
				boolean isProductDecl = astNode instanceof Product;
				//If there is no expected value assume true, else assume the value of expected value
				return expectedValue == null ? isProductDecl : ((Boolean) expectedValue) == isProductDecl;
			}
		}
		return false;
	}
}
