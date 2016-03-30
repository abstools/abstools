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
import static apet.absunit.ABSUnitTestCaseTranslatorConstants.NULL;
import static apet.testCases.ABSTestCaseExtractor.getABSDataType;
import static apet.testCases.ABSTestCaseExtractor.getABSDataValue;
import static apet.testCases.ABSTestCaseExtractor.getABSObjectFields;
import static apet.testCases.ABSTestCaseExtractor.getABSObjectType;
import static apet.testCases.ABSTestCaseExtractor.getABSTermArgs;
import static apet.testCases.ABSTestCaseExtractor.getAfterState;
import static apet.testCases.ABSTestCaseExtractor.getInitialState;
import static apet.testCases.ABSTestCaseExtractor.getInputArgs;
import static apet.testCases.ABSTestCaseExtractor.getPreviousCalls;
import static apet.testCases.ABSTestCaseExtractor.getReturnData;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import abs.backend.tests.AbsASTBuilderUtil.DeclNamePredicate;
import abs.backend.tests.AbsASTBuilderUtil.ModifyClassModifierNamePredicate;
import abs.frontend.ast.Access;
import abs.frontend.ast.AddFieldModifier;
import abs.frontend.ast.Block;
import abs.frontend.ast.Call;
import abs.frontend.ast.ClassDecl;
import abs.frontend.ast.DataTypeUse;
import abs.frontend.ast.DeltaDecl;
import abs.frontend.ast.EqExp;
import abs.frontend.ast.Exp;
import abs.frontend.ast.FieldDecl;
import abs.frontend.ast.InterfaceDecl;
import abs.frontend.ast.InterfaceTypeUse;
import abs.frontend.ast.MethodImpl;
import abs.frontend.ast.MethodSig;
import abs.frontend.ast.Model;
import abs.frontend.ast.ModifyClassModifier;
import abs.frontend.ast.ModifyMethodModifier;
import abs.frontend.ast.ModuleModifier;
import abs.frontend.ast.NullExp;
import abs.frontend.ast.ParamDecl;
import abs.frontend.ast.PureExp;
import abs.frontend.ast.RemoveFieldModifier;
import abs.frontend.ast.Stmt;
import abs.frontend.ast.VarUse;
import apet.testCases.ABSData;
import apet.testCases.ABSObject;
import apet.testCases.ABSRef;
import apet.testCases.ABSTerm;
import apet.testCases.PreviousCall;
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

	Map<String, InterfaceTypeUse> getTypesFromABSObject(
			String testName, ABSObject obj) {
		Map<String, InterfaceTypeUse> map = new HashMap<String, InterfaceTypeUse>();
		for (ABSData data : getABSObjectFields(obj).values()) {
			map.putAll(getTypesFromABSData(testName, data));
		}
		return map;
	}

	Map<String, InterfaceTypeUse> getTypesFromABSData(String testName, ABSData data) {
		Map<String, InterfaceTypeUse> map = new HashMap<String, InterfaceTypeUse>();
		if (data instanceof ABSRef) {
			String type = getABSDataType(data);
			String value = getABSDataValue(data);

			if (! value.equals(NULL)) {
				map.put(heapRefBuilder.heapReferenceForTest(testName, value),
						new InterfaceTypeUse(type, new abs.frontend.ast.List<abs.frontend.ast.Annotation>()));
			}

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

		Map<String,InterfaceTypeUse> typesOfObjectInHeap = new HashMap<String, InterfaceTypeUse>();
		for (ABSData d : inputArguments) {
			typesOfObjectInHeap.putAll(getTypesFromABSData(testName, d));
		}

		Map<ABSRef,ABSObject> initial = getInitialState(testCase);
		for (ABSObject obj : initial.values()) {
			typesOfObjectInHeap.putAll(getTypesFromABSObject(testName, obj));
		}

		Set<String> initialHeapNames = referenceNames(initial.keySet());
		createObjectsInHeap(testName, initialHeapNames, typesOfObjectInHeap,
				testClass, initial, block);

		List<PreviousCall> calls = getPreviousCalls(testCase);
		List<Exp> previous = makePreviousCalls(testName, initialHeapNames, calls);
		for (Exp pc : previous) {
			block.addStmtNoTransform(getExpStmt(pc)); //does not care about return value
		}

		//test execution
		Exp test = makeTestExecution(testName, initialHeapNames, unitUnderTest, inputArguments);

		final boolean hasReturnValue;
		if (access instanceof DataTypeUse &&
			((DataTypeUse) access).getName().equals("Unit")) {
			block.addStmtNoTransform(getExpStmt(test)); //no return value
			hasReturnValue = false;
		} else {
			block.addStmtNoTransform(getVarDecl("returnValue", access.treeCopyNoTransform(), test));
			hasReturnValue = true;
		}

		Map<ABSRef,ABSObject> finalHeap = getAfterState(testCase);
		if (finalHeap.isEmpty()) {
			//the method under test is side-effect free?
			//use the initial heap as oracle
			finalHeap = initial;
		}
		Set<String> finalHeapNames = referenceNames(finalHeap.keySet());

		//need to remember which objects in the heap we have already handled.
		Set<String> visited = new HashSet<String>();

		//check return value
		//only look at reference and data values
		//assertions of object states can be done in the heap assertions
		if (hasReturnValue) {
			ABSData rd = getReturnData(testCase);
			PureExp exp = pureExpBuilder.createPureExpression(testName, finalHeapNames, rd);
			block.addStmtNoTransform(getExpStmt(getCall(
				new VarUse(ASSERT_HELPER), "assertTrue", true,
				new EqExp(new VarUse("returnValue"), exp))));
		}

		//check return value (using deltas)
		makeGetAndAssertStatements(testName, finalHeapNames, testClass, finalHeap, visited, block);
	}

	/**
	 * Initialise (create if necessary) a delta to modify the given test class.
	 * In particular it ensures the delta contains a class modifier for the
	 * given test class and within that modifier, a method modifier for the given
	 * method name.
	 *
	 * @param testClass
	 * @param setOrAssertMethodForTest
	 * @return the method block of the method modifier.
	 */
	Block initialiseDeltaForTestClass(ClassDecl testClass,
			String setOrAssertMethodForTest) {

		String testClassName = testClass.getName();
		DeltaDecl delta = deltaBuilder.getDeltaFor(testClassName);

		if (delta == null) {
			delta = deltaBuilder.createDeltaFor(testClass);
		}

		ModifyClassModifier modifier =
				findClassOrIfaceModifier(delta, ModifyClassModifier.class,
						new ModifyClassModifierNamePredicate(testClassName));

		if (modifier == null) {
			modifier = new ModifyClassModifier();
			modifier.setName(testClassName);
			delta.addModuleModifier(modifier);
		}

		MethodSig sig = new MethodSig();
		sig.setName(setOrAssertMethodForTest);
		sig.setReturnType(getUnit());

		//add an empty method to be modified
		MethodImpl setOrAssertMethodForObjectImpl = new MethodImpl(sig, new Block(), false);
		testClass.addMethod(setOrAssertMethodForObjectImpl);

		ModifyMethodModifier mmm = new ModifyMethodModifier(
				setOrAssertMethodForObjectImpl.treeCopyNoTransform());
		Block modifyBlock = mmm.getMethodImpl().getBlock();
		modifier.addModifier(mmm);

		return modifyBlock;
	}

	void createObjectsInHeap(
			String testMethodName,
			Set<String> heapNames,
			Map<String,InterfaceTypeUse> objectsInHeap,
			ClassDecl testClass,
			Map<ABSRef,ABSObject> initialHeap,
			Block testMethodBlock) {

		String setMethodForTest =
				testCaseNameBuilder.initialTestMethodName(testMethodName);

		Block modifyBlock = initialiseDeltaForTestClass(testClass,
				testCaseNameBuilder.initialTestMethodName(testMethodName));

		testMethodBlock.addStmtNoTransform(
				getExpStmt(getCall(getThis(), setMethodForTest, true)));

		Map<String, String> typeHierarchy = new HashMap<String, String>();
		Map<String, List<Stmt>> initialisations = new HashMap<String, List<Stmt>>();
		List<String> initialisationsOrders = new ArrayList<String>();
		for (ABSRef r : initialHeap.keySet()) {
			makeSetStatements(
					typeHierarchy,
					initialisations,
					initialisationsOrders,
					testMethodName, heapNames, initialHeap,
					objectsInHeap, r, initialHeap.get(r),
					testClass);
		}

		for (String ref : initialisationsOrders) {
			for (Stmt s : initialisations.get(ref)) {
				modifyBlock.addStmtNoTransform(s);
			}
		}

		String testClassName = testClass.getName();
		DeltaDecl delta = deltaBuilder.getDeltaFor(testClassName);
		ModifyClassModifier cm = null;
		for (ModuleModifier m : delta.getModuleModifiers()) {
			if (m.getName().equals(testClassName)) {
				cm = (ModifyClassModifier) m;
				break;
			}
		}

		for (String r : objectsInHeap.keySet()) {
			FieldDecl field = new FieldDecl();
			field.setName(r);

			InterfaceTypeUse inf = objectsInHeap.get(r);
			field.setAccess(inf);
			testClass.addField(field);

			//allow access of subtype information
			cm.addModifier(new RemoveFieldModifier(field.treeCopyNoTransform()));
			FieldDecl newField = new FieldDecl();
			newField.setName(r);
			newField.setAccess(new InterfaceTypeUse(typeHierarchy.get(inf.getName()), new abs.frontend.ast.List<abs.frontend.ast.Annotation>()));
			cm.addModifier(new AddFieldModifier(newField));
		}

	}

	void makeSetStatements(
			Map<String, String> typeHierarchy,
			Map<String, List<Stmt>> initialisations,
			List<String> initialisationsOrders,
			String testName,
			Set<String> heapNames,
			Map<ABSRef, ABSObject> initialHeap,
			Map<String, InterfaceTypeUse> objectsInHeap,
			ABSRef ref, ABSObject state,
			ClassDecl testClass) {

		String rn = heapRefBuilder.heapReferenceForTest(testName, getABSDataValue(ref));
		String concreteTypeName = getABSObjectType(state);

		ClassDecl concreteType =
				getDecl(model, ClassDecl.class,
						new DeclNamePredicate<ClassDecl>(concreteTypeName));

		if (concreteType == null) {
			throw new IllegalStateException("Cannot find class: "+concreteTypeName);
		}

		List<Stmt> statements = new ArrayList<Stmt>();
		initialisations.put(rn, statements);
		if (! initialisationsOrders.contains(rn)) {
			initialisationsOrders.add(rn);
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
			PureExp exp = pureExpBuilder.createPureExpression(rn, initialisationsOrders, testName, heapNames, d);
			constructorArgs[i] = exp;
		}

		statements.add(getVAssign(rn, newObj(concreteType, false, constructorArgs)));

		for (String fn : fields.keySet()) {
			ABSData d = fields.get(fn);
			objectsInHeap.putAll(getTypesFromABSData(testName, d));
			PureExp exp = pureExpBuilder.createPureExpression(rn, initialisationsOrders, testName, heapNames, d);
			Call call = getCall(new VarUse(rn), testCaseNameBuilder.setterMethodName(fn), true, exp);
			statements.add(getExpStmt(call));
		}

		//ADD getter and setter
		deltaBuilder.updateDelta(
				typeHierarchy,
				objectsInHeap.get(rn),
				getDecl(model, ClassDecl.class,
				new DeclNamePredicate<ClassDecl>(concreteTypeName)));

	}

	void makeGetAndAssertStatements(
			String testMethodName,
			Set<String> heapNames,
			ClassDecl testClass,
			Map<ABSRef,ABSObject> finalHeap,
			Set<String> visited,
			Block testMethodBlock) {

		String assertMethodForTest =
				testCaseNameBuilder.assertTestMethodName(testMethodName);

		Block modifyBlock = initialiseDeltaForTestClass(testClass, assertMethodForTest);

		testMethodBlock.addStmtNoTransform(
				getExpStmt(getCall(getThis(), assertMethodForTest, true)));

		for (ABSRef r : finalHeap.keySet()) {
			makeGetAndAssertStatementsForHeapRef(testMethodName, heapNames, finalHeap, r, finalHeap.get(r), visited, modifyBlock);
		}

	}

	void makeGetAndAssertStatementsForHeapRef(
			String testName,
			Set<String> heapNames,
			Map<ABSRef, ABSObject> finalHeap,
			ABSRef ref,
			ABSObject state,
			Set<String> visited,
			Block block) {
		String rn = heapRefBuilder.heapReferenceForTest(testName, getABSDataValue(ref));

		if (! visited.add(rn)) {
			return;
		}

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
				block.addStmtNoTransform(getVarDecl(testCaseNameBuilder.resultOfGetterMethodName(fn),
						field.getAccess().treeCopyNoTransform(),
						getCall(new VarUse(rn), testCaseNameBuilder.getterMethodName(fn), true)));
				makeOracle(testName, heapNames, finalHeap,
						testCaseNameBuilder.resultOfGetterMethodName(fn),
						field.getAccess().treeCopyNoTransform(), d, visited, block);
			}
		}
	}

	void makeOracle(String testName, Set<String> heapNames,
			Map<ABSRef,ABSObject> finalHeap,
			String actual, Access access, ABSData data, Set<String> visited, Block block) {

		InterfaceDecl inf = null;
		if (access instanceof DataTypeUse) {
			inf = getDecl(model, InterfaceDecl.class,
					new DeclNamePredicate<InterfaceDecl>(((DataTypeUse) access).getName()));
		}

		PureExp exp = pureExpBuilder.createPureExpression(testName, heapNames, data);
		block.addStmtNoTransform(getExpStmt(getCall(
				new VarUse(ASSERT_HELPER), "assertTrue", true,
				new EqExp(new VarUse(actual), exp))));

		if (inf != null && ! (exp instanceof NullExp)) {
			String ref = getABSDataValue(data);
			for (ABSRef r : finalHeap.keySet()) {
				if (getABSDataValue(r).equals(ref)) {
					makeGetAndAssertStatementsForHeapRef(
							testName, heapNames, finalHeap, r, finalHeap.get(r), visited, block);
					break;
				}
			}
		}

	}

	abstract List<Exp> makePreviousCalls(String testName, Set<String> heapNames, List<PreviousCall> calls);

	abstract Exp makeTestExecution(String testName, Set<String> heap,
			String functionName, List<ABSData> inArgs);

}
