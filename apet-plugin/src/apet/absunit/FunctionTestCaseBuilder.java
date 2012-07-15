package apet.absunit;

import java.util.List;
import java.util.Set;

import abs.frontend.ast.Exp;
import abs.frontend.ast.FnApp;
import abs.frontend.ast.Model;
import abs.frontend.ast.PureExp;
import apet.testCases.ABSData;

final class FunctionTestCaseBuilder extends ABSUnitTestCaseBuilder {

	private final PureExpressionBuilder pureExpBuilder;
	
	FunctionTestCaseBuilder(PureExpressionBuilder pureExpBuilder,
			DeltaForGetSetFieldsBuilder deltaBuilder,
			Model model) {
		super(pureExpBuilder, deltaBuilder, model);
		this.pureExpBuilder = pureExpBuilder;
	}
	
	@Override
	Exp makeTestExecution(String testName, Set<String> heap,
			String testExecutionName, List<ABSData> inArgs) {
		abs.frontend.ast.List<PureExp> ps = new abs.frontend.ast.List<PureExp>();
		FnApp fa = new FnApp();
		
		if (inArgs.size() == 0) {
			throw new IllegalStateException("Inputs for a method must at least have a reference");
		}
		
		fa.setName(testExecutionName);
		fa.setParamList(ps);
		
		for (int i=0; i<inArgs.size(); i++) {
			ABSData d = inArgs.get(i);
			PureExp exp = pureExpBuilder.createPureExpression(testName, heap, d);
			fa.setParam(exp,i);
		}
		return fa;
	}

}
