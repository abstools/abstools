package apet.absunit;

import static abs.backend.tests.AbsASTBuilderUtil.findClassOrIfaceModifier;
import static abs.backend.tests.AbsASTBuilderUtil.getCall;
import static abs.backend.tests.AbsASTBuilderUtil.getDecl;
import static abs.backend.tests.AbsASTBuilderUtil.getExpStmt;
import static abs.backend.tests.AbsASTBuilderUtil.getThis;
import static abs.backend.tests.AbsASTBuilderUtil.getUnit;
import static abs.backend.tests.AbsASTBuilderUtil.getVAssign;
import static abs.backend.tests.AbsASTBuilderUtil.getVarDecl;
import static abs.backend.tests.AbsASTBuilderUtil.newObj;
import static apet.absunit.ABSUnitTestCaseTranslatorConstants.ASSERT_HELPER;
import static apet.testCases.ABSTestCaseExtractor.getABSDataType;
import static apet.testCases.ABSTestCaseExtractor.getABSDataValue;
import static apet.testCases.ABSTestCaseExtractor.getABSObjectFields;
import static apet.testCases.ABSTestCaseExtractor.getABSObjectType;
import static apet.testCases.ABSTestCaseExtractor.getABSTermArgs;
import static apet.testCases.ABSTestCaseExtractor.getAfterState;
import static apet.testCases.ABSTestCaseExtractor.getInitialState;
import static apet.testCases.ABSTestCaseExtractor.getInputArgs;
import static apet.testCases.ABSTestCaseExtractor.getReturnData;

import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import abs.backend.tests.AbsASTBuilderUtil.DeclNamePredicate;
import abs.backend.tests.AbsASTBuilderUtil.ModifyClassModifierNamePredicate;
import abs.frontend.ast.Access;
import abs.frontend.ast.Block;
import abs.frontend.ast.ClassDecl;
import abs.frontend.ast.DataTypeUse;
import abs.frontend.ast.DeltaDecl;
import abs.frontend.ast.Exp;
import abs.frontend.ast.FieldDecl;
import abs.frontend.ast.InterfaceTypeUse;
import abs.frontend.ast.MethodImpl;
import abs.frontend.ast.MethodSig;
import abs.frontend.ast.Model;
import abs.frontend.ast.ModifyClassModifier;
import abs.frontend.ast.ModifyMethodModifier;
import abs.frontend.ast.ParamDecl;
import abs.frontend.ast.PureExp;
import abs.frontend.ast.SyncCall;
import abs.frontend.ast.VarUse;
import apet.testCases.ABSData;
import apet.testCases.ABSObject;
import apet.testCases.ABSRef;
import apet.testCases.ABSTerm;
import apet.testCases.TestCase;

abstract class ABSUnitTestCaseBuilder {
	
	private final HeapReferenceBuilder heapRefBuilder = new HeapReferenceBuilder();
	private final TestCaseNamesBuilder testCaseNameBuilder = new TestCaseNamesBuilder();
	
	private final PureExpressionBuilder pureExpBuilder;
	private final DeltaForGetSetFieldsBuilder deltaBuilder;
	private final Model model;
	
	ABSUnitTestCaseBuilder(PureExpressionBuilder pureExpBuilder,
			DeltaForGetSetFieldsBuilder deltaBuilder,
			Model model) {
		this.pureExpBuilder = pureExpBuilder;
		this.deltaBuilder = deltaBuilder;
		this.model = model;
	}
	
	Set<String> referenceNames(Set<ABSRef> refs) {
		Set<String> names = new HashSet<String>();
		for (ABSRef r : refs) {
			names.add(getABSDataValue(r));
		}
		return names;
	}
	
	Map<String, Access> getTypesFromABSData(String testName, ABSData data) {
		Map<String, Access> map = new HashMap<String, Access>();
		if (data instanceof ABSRef) {
			String type = getABSDataType(data);
			String value = getABSDataValue(data);
			map.put(heapRefBuilder.heapReferenceForTest(testName, value), 
					new InterfaceTypeUse(type));
		} else if (data instanceof ABSTerm) {
			ABSTerm term = (ABSTerm) data;
			for (ABSData t : getABSTermArgs(term)) {
				map.putAll(getTypesFromABSData(testName, t));
			}
		}
		return map;
	}

	/**
	 * 
	 * @param testCase
	 * @param testClass
	 * @param method
	 * @param access
	 * @param unitUnderTest
	 */
	void buildTestCase(TestCase testCase, ClassDecl testClass, 
			MethodImpl method, Access access, String unitUnderTest) {
		//initial arg
		List<ABSData> inputArguments = getInputArgs(testCase);
		String testName = method.getMethodSig().getName();
		Block block = method.getBlock();
		
		Map<String,Access> typesOfObjectInHeap = new HashMap<String, Access>();
		for (ABSData d : inputArguments) {
			typesOfObjectInHeap.putAll(getTypesFromABSData(testName, d));
		}
		
		Map<ABSRef,ABSObject> initial = getInitialState(testCase);
		Set<String> initialHeapNames = referenceNames(initial.keySet());
		
		createObjectsInHeap(testName, initialHeapNames, typesOfObjectInHeap, 
				testClass, initial, block);
		
		//test execution
		Exp test = makeTestExecution(testName, initialHeapNames, unitUnderTest, inputArguments);
		
		if (access instanceof DataTypeUse &&
			((DataTypeUse) access).getName().equals("Unit")) {
			block.addStmt(getExpStmt(test)); //no return value
			assert getReturnData(testCase) == null;
		} else {
			block.addStmt(getVarDecl("returnValue", (Access) access.fullCopy(), test));
		}
		
		Map<ABSRef,ABSObject> finalHeap = getAfterState(testCase);
		Set<String> finalHeapNames = referenceNames(finalHeap.keySet());
		
		//check return value
		ABSData rd = getReturnData(testCase);
		if (rd != null) {
			makeOracle(testName, finalHeapNames, finalHeap, "returnValue", (Access) access.fullCopy(), rd, block);
		}
		
		//check return value (using deltas)
		makeGetAndAssertStatements(testName, finalHeapNames, testClass, finalHeap, block);
	}
	
	void createObjectsInHeap(
			String testMethodName, 
			Set<String> heapNames,
			Map<String,Access> objectsInHeap,
			ClassDecl testClass, 
			Map<ABSRef,ABSObject> initialHeap, 
			Block testMethodBlock) {
		
		String testClassName = testClass.getName();
		DeltaDecl delta = deltaBuilder.getDeltaFor(testClassName);
		
		if (delta == null) {
			delta = deltaBuilder.createDeltaFor(testClassName);
		}
		
		ModifyClassModifier modifier =
				findClassOrIfaceModifier(delta, ModifyClassModifier.class, 
						new ModifyClassModifierNamePredicate(testClassName));
		
		if (modifier == null) {
			modifier = new ModifyClassModifier();
			modifier.setName(testClassName);
			delta.addModuleModifier(modifier);
		}
		
		String setMethodForTest =
				testCaseNameBuilder.initialTestMethodName(testMethodName);
		
		MethodSig sig = new MethodSig();
		sig.setName(setMethodForTest);
		sig.setReturnType(getUnit());
		
		//add an empty method to be modified
		MethodImpl setMethodForObjectImpl = new MethodImpl(sig, new Block());
		testClass.addMethod(setMethodForObjectImpl);
		
		ModifyMethodModifier mmm = new ModifyMethodModifier(setMethodForObjectImpl.fullCopy());
		Block modifyBlock = mmm.getMethodImpl().getBlock();
		modifier.addModifier(mmm);
		
		testMethodBlock.addStmt(
				getExpStmt(getCall(getThis(), setMethodForTest, false)));

		for (ABSRef r : initialHeap.keySet()) {
			makeSetStatements(testMethodName, heapNames, initialHeap, 
					objectsInHeap, r, initialHeap.get(r), 
					modifyBlock, testClass);
		}
		
		for (String r : objectsInHeap.keySet()) {
			FieldDecl field = new FieldDecl();
			field.setName(r);
			field.setAccess(objectsInHeap.get(r));
			testClass.addField(field);
		}
		
	}
	
	void makeSetStatements(
			String testName,
			Set<String> heapNames,
			Map<ABSRef, ABSObject> initialHeap, 
			Map<String, Access> objectsInHeap, 
			ABSRef ref, ABSObject state, Block block, 
			ClassDecl testClass) {
		
		String rn = heapRefBuilder.heapReferenceForTest(testName, getABSDataValue(ref)); 
		String concreteTypeName = getABSObjectType(state);
		
		ClassDecl concreteType = 
				getDecl(model, ClassDecl.class, 
						new DeclNamePredicate<ClassDecl>(concreteTypeName));
		
		if (concreteType == null) {
			throw new IllegalStateException("Cannot find class: "+concreteTypeName);
		}
		
		Map<String,ABSData> fields = getABSObjectFields(state);
		abs.frontend.ast.List<ParamDecl> params = concreteType.getParamList();
		PureExp[] constructorArgs = new PureExp[params.getNumChild()];
		for (int i=0; i < params.getNumChild(); i++) {
			ParamDecl param = params.getChild(i);
			String name = param.getName();
			assert fields.containsKey(name);
			ABSData d = fields.remove(name);
			objectsInHeap.putAll(getTypesFromABSData(testName, d));
			PureExp exp = pureExpBuilder.createPureExpression(testName, heapNames, d);
			constructorArgs[i] = exp;
		}
		
		block.addStmt(getVAssign(rn, newObj(concreteType, false, constructorArgs)));
		
		for (String fn : fields.keySet()) {
			SyncCall syncCall = new SyncCall();
			syncCall.setCallee(new VarUse(rn));
			syncCall.setMethod(testCaseNameBuilder.setterMethodName(fn));
			
			ABSData d = fields.get(fn);
			objectsInHeap.putAll(getTypesFromABSData(testName, d));

			PureExp exp = pureExpBuilder.createPureExpression(testName, heapNames, d);
			syncCall.addParam(exp);
			block.addStmt(getExpStmt(syncCall));
		}
		
		//ADD getter and setter
		deltaBuilder.updateDelta(getDecl(model, ClassDecl.class, 
				new DeclNamePredicate<ClassDecl>(getABSObjectType(state))));
		
	}
	
	void makeGetAndAssertStatements(
			String testMethodName,
			Set<String> heapNames,
			ClassDecl testClass, 
			Map<ABSRef,ABSObject> finalHeap, 
			Block testMethodBlock) {
		
		String testClassName = testClass.getName();
		DeltaDecl delta = deltaBuilder.getDeltaFor(testClassName);
		
		if (delta == null) {
			delta = deltaBuilder.createDeltaFor(testClassName);
		}
		
		ModifyClassModifier modifier =
				findClassOrIfaceModifier(delta, ModifyClassModifier.class, 
						new ModifyClassModifierNamePredicate(testClassName));
		
		if (modifier == null) {
			modifier = new ModifyClassModifier();
			modifier.setName(testClassName);
			delta.addModuleModifier(modifier);
		}
		
		String assertMethodForTest =
				testCaseNameBuilder.assertTestMethodName(testMethodName);
		
		MethodSig sig = new MethodSig();
		sig.setName(assertMethodForTest);
		sig.setReturnType(getUnit());
		
		//add an empty method to be modified
		MethodImpl assertMethodForObjectImpl = new MethodImpl(sig, new Block());
		testClass.addMethod(assertMethodForObjectImpl);
		
		ModifyMethodModifier mmm = new ModifyMethodModifier(assertMethodForObjectImpl.fullCopy());
		Block modifyBlock = mmm.getMethodImpl().getBlock();
		modifier.addModifier(mmm);
		
		SyncCall call = new SyncCall();
		call.setCallee(new VarUse("this"));
		call.setMethod(sig.getName());
		testMethodBlock.addStmt(getExpStmt(call));

		for (ABSRef r : finalHeap.keySet()) {
			makeGetAndAssertStatementsForHeapRef(testMethodName, heapNames, finalHeap, r, finalHeap.get(r), modifyBlock);
		}
		
	}
	
	void makeGetAndAssertStatementsForHeapRef(
			String testName,
			Set<String> heapNames,
			Map<ABSRef, ABSObject> finalHeap,
			ABSRef ref, 
			ABSObject state, 
			Block block) {
		String rn = heapRefBuilder.heapReferenceForTest(testName, getABSDataValue(ref)); 
		
		Map<String,ABSData> fields = getABSObjectFields(state);
		
		ClassDecl clazz = 
			getDecl(model, ClassDecl.class, 
				new DeclNamePredicate<ClassDecl>(getABSObjectType(state)));
		
		abs.frontend.ast.List<FieldDecl> fieldDecls = clazz.getFieldList();
		for (int i=0; i<fieldDecls.getNumChild(); i++) {
			FieldDecl field = fieldDecls.getChild(i);
			String fn = field.getName();
			if (fields.containsKey(fn)) {
				ABSData d = fields.get(fn);
				SyncCall syncCall = new SyncCall();
				syncCall.setCallee(new VarUse(rn));
				syncCall.setMethod(testCaseNameBuilder.getterMethodName(fn));
				block.addStmt(getVarDecl(testCaseNameBuilder.resultOfGetterMethodName(fn), 
						(Access) field.getAccess().fullCopy(), syncCall));
				makeOracle(testName, heapNames, finalHeap, testCaseNameBuilder.resultOfGetterMethodName(fn),
						(Access) field.getAccess().fullCopy(), d,block);
			}
		}
	}

	void makeOracle(String testName, Set<String> heapNames, 
			Map<ABSRef,ABSObject> finalHeap, 
			String actual, Access access, ABSData data, Block block) {
		
		//TODO handle object comparison!
		block.addStmt(
			getExpStmt(getCall(
					new VarUse(ASSERT_HELPER), "assertTrue", true, new VarUse(actual), 
					pureExpBuilder.createPureExpression(testName, heapNames, data))));
	}

	abstract Exp makeTestExecution(String testName, Set<String> heap, 
			String functionName, List<ABSData> inArgs);
	
}
