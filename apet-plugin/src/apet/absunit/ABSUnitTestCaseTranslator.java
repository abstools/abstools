package apet.absunit;

import static abs.backend.tests.AbsASTBuilderUtil.findMethodImpl;
import static abs.backend.tests.AbsASTBuilderUtil.findMethodSig;
import static abs.backend.tests.AbsASTBuilderUtil.getDecl;
import static apet.absunit.ABSUnitTestCaseTranslatorConstants.CONFIGURATION_NAME;
import static apet.absunit.ABSUnitTestCaseTranslatorConstants.FEATURE_NAME;
import static apet.absunit.ABSUnitTestCaseTranslatorConstants.MAIN;
import static apet.absunit.ABSUnitTestCaseTranslatorConstants.PRODUCT_NAME;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.PrintStream;
import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import abs.backend.prettyprint.DefaultABSFormatter;
import abs.backend.tests.AbsASTBuilderUtil;
import abs.backend.tests.AbsASTBuilderUtil.DeclNamePredicate;
import abs.backend.tests.AbsASTBuilderUtil.MethodNamePredicate;
import abs.backend.tests.AbsASTBuilderUtil.MethodSigNamePredicate;
import abs.backend.tests.AbsASTBuilderUtil.Predicate;
import abs.common.StringUtils;
import abs.frontend.analyser.SemanticError;
import abs.frontend.analyser.SemanticErrorList;
import abs.frontend.ast.ASTNode;
import abs.frontend.ast.Access;
import abs.frontend.ast.AppCond;
import abs.frontend.ast.AppCondFeature;
import abs.frontend.ast.ClassDecl;
import abs.frontend.ast.CompilationUnit;
import abs.frontend.ast.DeltaAccess;
import abs.frontend.ast.DeltaClause;
import abs.frontend.ast.DeltaDecl;
import abs.frontend.ast.DeltaID;
import abs.frontend.ast.Deltaspec;
import abs.frontend.ast.Feature;
import abs.frontend.ast.FunctionDecl;
import abs.frontend.ast.InterfaceDecl;
import abs.frontend.ast.InterfaceTypeUse;
import abs.frontend.ast.MethodImpl;
import abs.frontend.ast.MethodSig;
import abs.frontend.ast.Model;
import abs.frontend.ast.ModuleDecl;
import abs.frontend.ast.Product;
import abs.frontend.ast.ProductLine;
import abs.frontend.ast.StarExport;
import abs.frontend.ast.StarImport;
import abs.frontend.tests.ABSFormatter;
import apet.absunit.DeltaForGetSetFieldsBuilder.DeltaWrapper;
import apet.console.ConsoleHandler;
import apet.testCases.ApetTestSuite;
import apet.testCases.TestCase;

/**
 *
 * @author pwong
 *
 */
public class ABSUnitTestCaseTranslator {

	private final Model model;
	private final ModuleDecl module;
	private final Set<DeltaWrapper> deltas;
	private final Set<String> importModules;
	private ProductLine productline;
	private Product product;

	private File outputFile;

	private final boolean verbose;

	private final TestCaseNamesBuilder testCaseNameBuilder = new TestCaseNamesBuilder();
	private final ABSUnitTestCaseTranslatorHelper translatorHelper =  new ABSUnitTestCaseTranslatorHelper();
	private final PureExpressionBuilder pureExpBuilder;
	private final DeltaForGetSetFieldsBuilder deltaBuilder;
	private final MethodTestCaseBuilder methodBuilder;
	private final FunctionTestCaseBuilder functionBuilder;

	public ABSUnitTestCaseTranslator(Model model, File outputFile, boolean verbose) {
		if (model == null)
			throw new IllegalArgumentException("Model cannot be null!");

		this.model = model;
		this.module = new ModuleDecl();
		this.module.setName(MAIN);
		this.deltas = new HashSet<DeltaWrapper>();
		this.importModules = new HashSet<String>();
		this.pureExpBuilder = new PureExpressionBuilder(this.model, importModules);
		this.deltaBuilder = new DeltaForGetSetFieldsBuilder(deltas);
		this.methodBuilder = new MethodTestCaseBuilder(pureExpBuilder, deltaBuilder, this.model);
		this.functionBuilder = new FunctionTestCaseBuilder(pureExpBuilder, deltaBuilder, this.model);
		this.verbose = verbose;

		console("Gathering ABSUnit annotations");

		/*
		 * Do not search for test class definitions if this model does not
		 * contain the necessary ABSUnit annotations
		 */
		if (! translatorHelper.gatherABSUnitAnnotations(this.model)) {
			return;
		}

		this.outputFile = outputFile;
		this.translatorHelper.setABSAssertImpl(model);
	}

	public boolean hasABSUnit() {
		return translatorHelper.hasABSUnit();
	}

	/**
	 * Generates an ABS module {@link ModuleDecl} that defines the
	 * given test suite.
	 *
	 * @param suite
	 * @param validate
	 * @return
	 */
	@SuppressWarnings("rawtypes")
	public ModuleDecl generateABSUnitTests(ApetTestSuite suite, boolean validate) {
		console("Add basic imports...");

		for (String key : suite.keySet()) {
			console("Generating test suite for "+key+"...");
			generateABSUnitTest(suite.get(key), key);
		}

		Set<DeltaDecl> deltaDecls = new HashSet<DeltaDecl>();
		for (DeltaWrapper w : deltas) {
			DeltaDecl delta = w.getDelta();
			deltaDecls.add(delta);
			abs.frontend.ast.List<DeltaAccess> access = delta.getDeltaAccesss();
			if (access.hasChildren()) {
				String use = access.getChild(0).getModuleName();
				importModules.add(use);
			}
		}

		addImports(module);

		buildProductLine(module);
		console("Pretty printing ABSUnit tests...");

		List<ASTNode<ASTNode>> nodes =
				new ArrayList<ASTNode<ASTNode>>();

		nodes.add(module);
		nodes.addAll(deltaDecls);
		nodes.add(productline);
		nodes.add(product);

		printToFile(nodes, outputFile);
		if (validate) {
			console("Validating ABSUnit tests...");
			validateOutput();
		}

		console("ABSUnit tests generation successful");
		return module;
	}

	void buildProductLine(ModuleDecl module) {

		if (deltas.isEmpty()) {
			return;
		}

		console("Generating product line description...");
		productline = new ProductLine();
		productline.setName(CONFIGURATION_NAME);
		Feature feature = new Feature();
		feature.setName(FEATURE_NAME);
		productline.addFeature(feature);
		AppCond ac = new AppCondFeature(FEATURE_NAME);

		Set<String> applicationConditions = new HashSet<String>();
		DeltaClause lastClause = null;
		for (DeltaWrapper d : deltas) {
			DeltaClause clause = new DeltaClause();
			Deltaspec spec = new Deltaspec();
			String name = d.getDelta().getName();
			spec.setDeltaID(name);
			clause.setDeltaspec(spec);
			clause.setAppCond(ac);

			if (d.isLast()) {
				lastClause = clause;
			} else {
				applicationConditions.add(name);
			}

			productline.addDeltaClause(clause);
		}

		if (lastClause != null) {
			for (String n : applicationConditions) {
				lastClause.addAfterDeltaID(new DeltaID(n));
			}
		}

		product = new Product();
		product.setName(PRODUCT_NAME);
		product.addFeature(feature);

	}

	void generateABSUnitTest(List<TestCase> cs, String mn) {

		String[] sp = mn.split("\\.");
		final String methodName;
		final String className;
		if (sp.length == 2) {
			className = sp[0];
			methodName = sp[1];
		} else if (sp.length == 1) {
			className = null;
			methodName = mn;
		} else {
			throw new IllegalArgumentException();
		}

		InterfaceDecl ti = createTestFixture(cs.size(), className, methodName);

		if (className == null) {
			createTestSuiteForFunction(cs, ti, methodName);
		} else {
			createTestSuiteForClassMethod(cs, ti, className, methodName);
		}
	}

	void console(String txt) {
		console(txt, false);
	}

	void console(String txt, boolean force) {
		if (verbose || force) {
			ConsoleHandler.write(txt);
		}
	}

	private void validateOutput() {
		CompilationUnit unit = new CompilationUnit();
		ModuleDecl cm = module.fullCopy();
		unit.addModuleDecl(cm);
		for (DeltaWrapper d : deltas) {
			unit.addDeltaDecl(d.getDelta().fullCopy());
		}
		unit.setProductLine(productline.fullCopy());
		unit.addProduct(product.fullCopy());

		Model copy = model.fullCopy();
		copy.addCompilationUnit(unit);

		validateOutput(copy.fullCopy(), null);
		validateOutput(copy.fullCopy(), module.getName().concat(".").concat(PRODUCT_NAME));
	}

	private void validateOutput(Model model, String product) {
		Model copy = model.fullCopy();
		if (product != null) {
            try {
				copy.flattenForProduct(product);
			} catch (Exception e) {
				throw new IllegalStateException("Cannot select product "+product, e);
			}
		}

        SemanticErrorList typeerrors = copy.typeCheck();
        for (SemanticError se : typeerrors) {
            System.err.println(se.getHelpMessage());
        }
	}

	private void addImports(ModuleDecl module) {
		//export *;
		//import * from AbsUnit;
		//import * from AbsUnit.Hamcrest;
		//import * from AbsUnit.Hamcrest.Core;
		module.addExport(new StarExport());
		module.addImport(new StarImport("AbsUnit"));
		module.addImport(new StarImport("AbsUnit.Hamcrest"));
		module.addImport(new StarImport("AbsUnit.Hamcrest.Core"));

		for (String ip : importModules) {
			module.addImport(new StarImport(ip));
		}
	}

	@SuppressWarnings("rawtypes")
	private void printToFile(List<ASTNode<ASTNode>> nodes, File file) {
        try {
			PrintStream stream = new PrintStream(file);
			PrintWriter writer = new PrintWriter(stream, true);
	        ABSFormatter formatter = new DefaultABSFormatter(writer);
	        for (ASTNode<ASTNode> n : nodes) {
		        n.doPrettyPrint(writer, formatter);
	        }
		} catch (FileNotFoundException e) {
			e.printStackTrace(new PrintStream(
				ConsoleHandler.getDefault().newMessageStream()));
		}
	}

	/**
	 * Create a test suite for testing a function.
	 *
	 * @param testCases
	 * @param testInterface
	 * @param className
	 * @param functionName
	 */
	private void createTestSuiteForFunction(
			List<TestCase> testCases,
			InterfaceDecl testInterface,
			String functionName) {

		//create test class ([Suite])
		final ClassDecl testClass = translatorHelper.createTestClass(testInterface);
		module.addDecl(testClass);

		//find function under test
		FunctionDecl functionUnderTest = getDecl(model, FunctionDecl.class,
				new DeclNamePredicate<FunctionDecl>(functionName));

		final Access access = functionUnderTest.getTypeUse();

		importModules.add(functionUnderTest.getModuleDecl().getName());

		/*
		 * Test methods and Test cases are ordered that is,
		 * test case 1 is implemented by test method 1 and so on...
		 */
		for (int i=0; i<testCases.size(); i++) {
			console("Generating test case "+i+"...");
			TestCase testCase = testCases.get(i);
			MethodImpl method = testClass.getMethod(i);
			functionBuilder.buildTestCase(testCase, testClass,
					method, access, functionName);
		}

	}

	/**
	 * Find an interface the given class implements that exposes the given method.
	 *
	 * @param methodName
	 * @param classUnderTest
	 * @return
	 */
	InterfaceDecl findInterfaceUnderTest(String methodName, ClassDecl classUnderTest) {

		Predicate<MethodSig> mp = new MethodSigNamePredicate(methodName);
		for (InterfaceTypeUse iu : classUnderTest.getImplementedInterfaceUseList()) {
			InterfaceDecl inf = getDecl(model, InterfaceDecl.class,
					new DeclNamePredicate<InterfaceDecl>(iu.getName()));

			if (findMethodSig(inf, mp) != null) {
				return inf;
			}
		}

		//no interface exposed the given method.
		return null;
	}

	/**
	 * Create a test suite for testing a method.
	 *
	 * @param testCases
	 * @param testInterface
	 * @param className
	 * @param methodName
	 */
	private void createTestSuiteForClassMethod(
			List<TestCase> testCases,
			InterfaceDecl testInterface,
			String className,
			String methodName) {

		//create test class ([Suite])
		final ClassDecl testClass = translatorHelper.createTestClass(testInterface);
		module.addDecl(testClass);

		//find class under test.
		ClassDecl classUnderTest = getDecl(model, ClassDecl.class,
				new DeclNamePredicate<ClassDecl>(className));

		assert classUnderTest != null :
			"It should not be possible to not " +
			"find class under test";

		//find method under test.
		MethodImpl methodUnderTest =
				findMethodImpl(classUnderTest, new MethodNamePredicate(methodName));

		assert methodUnderTest != null :
			"It should not be possible to not " +
			"find method under test";

		//find interface of class under test.
		InterfaceDecl interfaceOfClassUnderTest =
				findInterfaceUnderTest(methodName, classUnderTest);

		if (interfaceOfClassUnderTest == null) {
			//this method is not exposed by any interface!

		}

		//return type
		MethodSig signature = methodUnderTest.getMethodSig();
		final Access access = signature.getReturnType();

		//add imports of class/interface under test
		importModules.add(classUnderTest.getModuleDecl().getName());
		importModules.add(interfaceOfClassUnderTest.getModuleDecl().getName());

		/*
		 * Test methods and Test cases are ordered that is,
		 * test case 1 is implemented by test method 1 and so on...
		 */
		for (int i=0; i<testCases.size(); i++) {
			console("Generating test case "+i+"...");
			TestCase testCase = testCases.get(i);
			MethodImpl method = testClass.getMethod(i);
			methodBuilder.buildTestCase(testCase, testClass,
					method, access, methodName);
		}

	}

	/**
	 * Create Test Fixture for a given Class method or a function.
	 *
	 * @param testCaseSize
	 * @param className
	 * @param methodName
	 * @return
	 */
	private InterfaceDecl createTestFixture(int testCaseSize, String className,
			String methodName) {

		String capMethodName = StringUtils.capitalize(methodName);

		if (className == null) {
			className = testCaseNameBuilder.functionClassName(capMethodName);
		}

		final String testInterfaceName = testCaseNameBuilder.testInterfaceName(className, capMethodName);

		//create fixture
		InterfaceDecl testInterface = getDecl(module, InterfaceDecl.class,
				AbsASTBuilderUtil.<InterfaceDecl>namePred(testInterfaceName));

		if (testInterface == null) {
			testInterface = translatorHelper.createTestInterface(testInterfaceName);
			module.addDecl(testInterface);
		}

		for (int i=1; i<=testCaseSize; i++) {
			testInterface.addBody(
					translatorHelper.createTestMethodSig(testCaseNameBuilder.testMethodName(capMethodName,
					Integer.valueOf(i).toString())));
		}

		return testInterface;
	}

}
