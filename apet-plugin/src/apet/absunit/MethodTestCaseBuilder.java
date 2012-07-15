package apet.absunit;

import static abs.backend.tests.AbsASTBuilderUtil.getCall;
import static apet.testCases.ABSTestCaseExtractor.getABSDataValue;

import java.util.List;
import java.util.Set;

import abs.frontend.ast.Call;
import abs.frontend.ast.Model;
import abs.frontend.ast.PureExp;
import abs.frontend.ast.VarUse;
import apet.testCases.ABSData;
import apet.testCases.ABSRef;

final class MethodTestCaseBuilder extends ABSUnitTestCaseBuilder {

	private final PureExpressionBuilder pureExpBuilder;
	
	MethodTestCaseBuilder(PureExpressionBuilder pureExpBuilder,
			DeltaForGetSetFieldsBuilder deltaBuilder,
			Model model) {
		super(pureExpBuilder, deltaBuilder, model);
		this.pureExpBuilder = pureExpBuilder;
	}
	
	@Override
	Call makeTestExecution(String testName, Set<String> heapNames,
			String testExecutionName, List<ABSData> inArgs) {
		
		if (inArgs.size() == 0) {
			throw new IllegalStateException("Inputs for a method must at least have a reference");
		}
		
		ABSData r = inArgs.get(0);
		if (! (r instanceof ABSRef)) {
			throw new IllegalStateException("Inputs for a method must at least have a reference");
		}
		
		PureExp[] ps = new PureExp[inArgs.size() - 1];
		for (int i=1; i<inArgs.size(); i++) {
			ABSData d = inArgs.get(i);
			PureExp exp = pureExpBuilder.createPureExpression(testName, heapNames, d);
			ps[i-1] = exp;
		}
		
		String rn = getABSDataValue(r);
		Call call = getCall(new VarUse(rn), testExecutionName, false, ps);
		return call;
	}

}
