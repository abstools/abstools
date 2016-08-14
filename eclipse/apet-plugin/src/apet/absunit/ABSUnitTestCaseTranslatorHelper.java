package apet.absunit;

import static abs.backend.tests.AbsASTBuilderUtil.createInterface;
import static abs.backend.tests.AbsASTBuilderUtil.createMethodImpl;
import static abs.backend.tests.AbsASTBuilderUtil.createMethodSig;
import static abs.backend.tests.AbsASTBuilderUtil.getDecl;
import static abs.backend.tests.AbsASTBuilderUtil.getUnit;
import static abs.backend.tests.AbsASTBuilderUtil.getVAssign;
import static abs.backend.tests.AbsASTBuilderUtil.newObj;
import static apet.absunit.ABSUnitTestCaseTranslatorConstants.ASSERT_HELPER;
import abs.backend.tests.AbsASTBuilderUtil.DeclNamePredicate;
import abs.frontend.ast.Annotation;
import abs.frontend.ast.CaseBranchStmt;
import abs.frontend.ast.ClassDecl;
import abs.frontend.ast.DataConstructor;
import abs.frontend.ast.DataConstructorExp;
import abs.frontend.ast.Decl;
import abs.frontend.ast.FieldDecl;
import abs.frontend.ast.InitBlock;
import abs.frontend.ast.InterfaceDecl;
import abs.frontend.ast.InterfaceTypeUse;
import abs.frontend.ast.MethodImpl;
import abs.frontend.ast.MethodSig;
import abs.frontend.ast.Model;
import abs.frontend.ast.Opt;
import abs.frontend.ast.ParamDecl;
import abs.frontend.ast.ParametricDataTypeDecl;
import abs.frontend.ast.PureExp;

/**
 *
 * @author woner
 *
 */
class ABSUnitTestCaseTranslatorHelper {

	private static final String ignore = "AbsUnit.Ignored";
	private static final String test = "AbsUnit.Test";
	private static final String dataPoint = "AbsUnit.DataPoint";

	private static final String suite = "AbsUnit.Suite";
	private static final String fixture = "AbsUnit.Fixture";

	private final TestCaseNamesBuilder testCaseNameBuilder = new TestCaseNamesBuilder();

	private DataConstructor ignoreType;
	private DataConstructor testType;
	private DataConstructor dataPointType;

	private DataConstructor suiteType;
	private DataConstructor fixtureType;

	private boolean hasABSUnit = false;
	private ClassDecl absAssertImpl;

	private Annotation getTestAnnotation(DataConstructor c) {
		return new Annotation(new DataConstructorExp(
				c.getName(),
				new abs.frontend.ast.List<PureExp>()));
	}

	/**
	 * Create a method signature for testing method with the given method name.
	 *
	 * @param methodName
	 * @param decls
	 * @return
	 */
	MethodSig createTestMethodSig(String methodName,
			ParamDecl... decls) {
		MethodSig methodSig = createMethodSig(methodName, getUnit(), decls);
		methodSig.addAnnotation(getTestAnnotation(testType));
		return methodSig;
	}

	InterfaceDecl createTestInterface(String interfaceName) {
		InterfaceDecl ti = createInterface(interfaceName);
		ti.addAnnotation(getTestAnnotation(fixtureType));
		return ti;
	}

	ClassDecl createTestClass(InterfaceDecl inf) {
		ClassDecl ct = new ClassDecl(testCaseNameBuilder.className(inf.getName()),
				new abs.frontend.ast.List<Annotation>(),
				new abs.frontend.ast.List<ParamDecl>(),
				new abs.frontend.ast.List<InterfaceTypeUse>(),
				new Opt<InitBlock>(),
                                new abs.frontend.ast.List<CaseBranchStmt>(),
				new abs.frontend.ast.List<FieldDecl>(),
				new abs.frontend.ast.List<MethodImpl>());

		ct.addAnnotation(getTestAnnotation(suiteType));
		ct.addImplementedInterfaceUse(new InterfaceTypeUse(inf.getName(), new abs.frontend.ast.List<abs.frontend.ast.Annotation>()));

		for (MethodSig m : inf.getAllMethodSigs()) {
			ct.addMethod(createMethodImpl(m));
		}

		FieldDecl assertImpl = new FieldDecl();
		assertImpl.setName(ASSERT_HELPER);
		assertImpl.setAccess(absAssertImpl.getImplementedInterfaceUse(0).treeCopyNoTransform());
		ct.addField(assertImpl);

		InitBlock block = new InitBlock();
		block.addStmtNoTransform(getVAssign(ASSERT_HELPER, newObj(absAssertImpl, false)));
		ct.setInitBlock(block);

		return ct;
	}

	boolean gatherABSUnitAnnotations(Model model) {
		for (Decl decl : model.getDecls()) {
			if (decl instanceof ParametricDataTypeDecl) {
				if (decl.getType().getQualifiedName().equals(test)) {
					testType = ((ParametricDataTypeDecl) decl)
							.getDataConstructor(0);
				} else if (decl.getType().getQualifiedName().equals(fixture)) {
					fixtureType = ((ParametricDataTypeDecl) decl)
							.getDataConstructor(0);
				} else if (decl.getType().getQualifiedName().equals(suite)) {
					suiteType = ((ParametricDataTypeDecl) decl)
							.getDataConstructor(0);
				} else if (decl.getType().getQualifiedName().equals(dataPoint)) {
					dataPointType = ((ParametricDataTypeDecl) decl)
							.getDataConstructor(0);
				} else if (decl.getType().getQualifiedName().equals(ignore)) {
					ignoreType = ((ParametricDataTypeDecl) decl)
							.getDataConstructor(0);
				}
			}
		}

		hasABSUnit =
			! (ignoreType == null || testType == null || dataPointType == null ||
			   suiteType == null || fixtureType == null);

		return hasABSUnit;
	}

	void setABSAssertImpl(Model model) {
		this.absAssertImpl =
				getDecl(model, ClassDecl.class,
					new DeclNamePredicate<ClassDecl>("ABSAssertImpl"));

		if (this.absAssertImpl == null)
			throw new IllegalArgumentException("Cannot find ABSAssertImpl");
	}

	boolean hasABSUnit() {
		return hasABSUnit;
	}


}
