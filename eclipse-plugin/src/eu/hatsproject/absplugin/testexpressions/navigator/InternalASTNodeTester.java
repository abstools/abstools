package eu.hatsproject.absplugin.testexpressions.navigator;

import org.eclipse.core.expressions.PropertyTester;

import abs.frontend.ast.ModuleDecl;
import eu.hatsproject.absplugin.util.InternalASTNode;

/**
 * Tester class for testing if an InternalASTNode contains a ModuleDecl AST node
 * @author cseise
 *
 */
public class InternalASTNodeTester extends PropertyTester {

	private static final String IS_MODULE_NODE_PROPERTY = "isModuleNode";
	
	@Override
	public boolean test(Object receiver, String property, Object[] args, Object expectedValue) {
		if (IS_MODULE_NODE_PROPERTY.equals(property)) {
			if (receiver instanceof InternalASTNode<?>) {
				Object astNode = ((InternalASTNode<?>) receiver).getASTNode();
				boolean isModuleDecl = astNode instanceof ModuleDecl;
				//If there is no expected value assume true, else assume the value of expected value
				return expectedValue == null ? isModuleDecl : ((Boolean) expectedValue) == isModuleDecl;
			}
		}
		return false;
	}

}
