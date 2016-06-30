package apet.absunit;

import static abs.backend.tests.AbsASTBuilderUtil.getCall;
import static apet.testCases.ABSTestCaseExtractor.getABSDataValue;
import static apet.testCases.ABSTestCaseExtractor.getCallArgs;
import static apet.testCases.ABSTestCaseExtractor.getMethodName;

import java.util.ArrayList;
import java.util.List;
import java.util.Set;

import abs.frontend.ast.Call;
import abs.frontend.ast.Exp;
import abs.frontend.ast.Model;
import abs.frontend.ast.PureExp;
import abs.frontend.ast.VarUse;
import apet.testCases.ABSData;
import apet.testCases.ABSRef;
import apet.testCases.PreviousCall;

/**
 * 
 * @author pwong
 *
 */
final class MethodTestCaseBuilder extends ABSUnitTestCaseBuilder {

	private final PureExpressionBuilder pureExpBuilder;
	private final HeapReferenceBuilder heapRefBuilder = new HeapReferenceBuilder();
	
	MethodTestCaseBuilder(PureExpressionBuilder pureExpBuilder,
			DeltaForGetSetFieldsBuilder deltaBuilder,
			Model model) {
		super(pureExpBuilder, deltaBuilder, model);
		this.pureExpBuilder = pureExpBuilder;
	}
	
	private Call makeMethodCall(String testName, Set<String> heapNames,
			String methodName, List<ABSData> inArgs, boolean sync) {
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
		VarUse var = new VarUse(heapRefBuilder.heapReferenceForTest(testName, rn));
		Call call = getCall(var, methodName, sync, ps);
		return call;
		
	}
	
	@Override
	Call makeTestExecution(String testName, Set<String> heapNames,
			String testExecutionName, List<ABSData> inArgs) {
		return makeMethodCall(testName, heapNames, testExecutionName, inArgs, true);
	}
	
	String removeClassName(String name) {
		int index = name.indexOf('.');
		if (0 <= index && index < name.length() - 1) {
			name = name.substring(index + 1);
		}
		return name;
	}
	
	@Override
	List<Exp> makePreviousCalls(String testName, Set<String> heapNames, List<PreviousCall> calls) {
		//task interleaving
		List<Exp> expressions = new ArrayList<Exp>();
		for (PreviousCall call : calls) {
			//test execution
			List<ABSData> callInputArguments = getCallArgs(call);
			String methodName = removeClassName(getMethodName(call));
			expressions.add(makeMethodCall(testName, heapNames, methodName, callInputArguments, false));
		}
		return expressions;
	}

}
