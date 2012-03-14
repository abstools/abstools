package apet.absunit;

import static abs.backend.tests.AbsASTBuilderUtil.*;
import static apet.testCases.ABSTestCaseExtractor.*;

import java.io.File;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import abs.backend.tests.AbsASTBuilderUtil;
import abs.backend.tests.AbsASTBuilderUtil.DeclNamePredicate;
import abs.backend.tests.AbsASTBuilderUtil.MethodNamePredicate;
import abs.backend.tests.AbsASTBuilderUtil.MethodSigNamePredicate;
import abs.backend.tests.AbsASTBuilderUtil.Predicate;
import abs.common.StringUtils;
import abs.frontend.ast.Access;
import abs.frontend.ast.AddInterfaceModifier;
import abs.frontend.ast.AddMethodModifier;
import abs.frontend.ast.Annotation;
import abs.frontend.ast.Block;
import abs.frontend.ast.ClassDecl;
import abs.frontend.ast.DataConstructor;
import abs.frontend.ast.DataConstructorExp;
import abs.frontend.ast.DataTypeUse;
import abs.frontend.ast.Decl;
import abs.frontend.ast.DeltaClause;
import abs.frontend.ast.DeltaDecl;
import abs.frontend.ast.Deltaspec;
import abs.frontend.ast.Export;
import abs.frontend.ast.Feature;
import abs.frontend.ast.FieldDecl;
import abs.frontend.ast.FieldUse;
import abs.frontend.ast.Import;
import abs.frontend.ast.InitBlock;
import abs.frontend.ast.IntLiteral;
import abs.frontend.ast.InterfaceDecl;
import abs.frontend.ast.InterfaceTypeUse;
import abs.frontend.ast.MethodImpl;
import abs.frontend.ast.MethodSig;
import abs.frontend.ast.Model;
import abs.frontend.ast.ModifyClassModifier;
import abs.frontend.ast.ModuleDecl;
import abs.frontend.ast.Opt;
import abs.frontend.ast.ParamDecl;
import abs.frontend.ast.ParametricDataTypeDecl;
import abs.frontend.ast.Product;
import abs.frontend.ast.ProductLine;
import abs.frontend.ast.PureExp;
import abs.frontend.ast.RemoveMethodModifier;
import abs.frontend.ast.ReturnStmt;
import abs.frontend.ast.SyncCall;
import abs.frontend.ast.VarUse;
import apet.testCases.ABSData;
import apet.testCases.ABSObject;
import apet.testCases.ABSRef;
import apet.testCases.ABSTerm;
import apet.testCases.ABSTestCaseExtractor;
import apet.testCases.ApetTestSuite;
import apet.testCases.TestCase;


public class ABSUnitTestCaseTranslator {
	
	private static final String SETTER_PREFIX = "setForTest";
	private static final String GETTER_PREFIX = "getForTest";
	private static final String RUN_METHOD = "run";
	private static final String CLASS_SUFFIX = "Impl";
	private static final String INTERFACE_SUFFIX = "Test";
	private static final String METHOD_PREFIX = "test";
	private static final String DELTA_SUFFIX = "Delta";
	
	private static final String MAIN = "AbsUnit.TestCase";

	private static final String ignore = "AbsUnit.Ignored";
	private static final String test = "AbsUnit.Test";
	private static final String dataPoint = "AbsUnit.DataPoint";

	private static final String suite = "AbsUnit.Suite";
	private static final String fixture = "AbsUnit.Fixture";

	private DataConstructor ignoreType;
	private DataConstructor testType;
	private DataConstructor dataPointType;

	private DataConstructor suiteType;
	private DataConstructor fixtureType;

	private final Model model;
	private final ModuleDecl output;
	
	public ABSUnitTestCaseTranslator(Model model, File outputDir) {
		if (model == null)
			throw new IllegalArgumentException("Model cannot be null!");

		this.model = model;
		this.output = new ModuleDecl();
		this.output.setName(MAIN);

		gatherABSUnitAnnotations();

		/*
		 * Do not search for test class definitions if this model does not
		 * contain the necessary ABSUnit annotations
		 */
		if (ignoreType == null || testType == null || dataPointType == null ||
			suiteType == null || fixtureType == null) {
			return;
		}
		
		if (outputDir.isFile())
			throw new IllegalArgumentException(outputDir.toString()+" is a file!");
		
		if (! outputDir.exists())
			outputDir.mkdirs();
		
	}
	
	public boolean hasABSUnit() {
		return testType != null;
	}
	
	public void generateABSUnitTests(ApetTestSuite suite) {

		for (String key : suite.keySet()) {
			generateABSUnitTest(suite.get(key), key);
		}
		
		Set<String> dn = new HashSet<String>(); 
		for (Decl d : output.getDeclList()) {
			if (d instanceof DeltaDecl) {
				dn.add(d.getName());
			}
		}
		
		ProductLine productline = new ProductLine();
		productline.setName("ABSUnitConfiguration");
		Feature feature = new Feature();
		feature.setName("F");
		productline.addOptionalFeature(feature);
		
		for (String d : dn) {
			DeltaClause clause = new DeltaClause();
			Deltaspec spec = new Deltaspec();
			spec.setName(d);
			clause.setDeltaspec(spec);
			clause.addFeature(feature);
			productline.addDeltaClause(clause);
		}
		
		Product product = new Product();
		product.setName("ABSUnitProduct");
		product.addFeature(feature);
		
		output.setProductLine(productline);
		output.addProduct(product);
	}
	
	private Annotation getTestAnnotation(DataConstructor c) {
		return new Annotation(new DataConstructorExp(
				c.getName(), 
				new abs.frontend.ast.List<PureExp>()));
	}
	
	private RemoveMethodModifier removeRun() {
		RemoveMethodModifier modifier = 
			new RemoveMethodModifier(createMethodSig(RUN_METHOD, getUnit()));
		return modifier;
	}
	
	/**
	 * Add an add method modifier
	 * @param field
	 * @param exp
	 * @param decl
	 * @return
	 */
	private AddMethodModifier addSetter(
			String field, Access type) {
		MethodSig sig = new MethodSig(SETTER_PREFIX+field, 
				new abs.frontend.ast.List<Annotation>(),
				getUnit(),  
				new abs.frontend.ast.List<ParamDecl>());
		
		sig.addParam(new ParamDecl("v", type, new abs.frontend.ast.List<Annotation>()));
		Block block = new Block();
		block.addStmt(getVAssign(new FieldUse(field), new VarUse("v")));
		MethodImpl method = new MethodImpl(sig, block);
		AddMethodModifier modifier = new AddMethodModifier(method);
		return modifier;
	}
	
	private AddMethodModifier addGetter(String field, Access returnType) {
		MethodSig sig = new MethodSig(GETTER_PREFIX+field, 
				new abs.frontend.ast.List<Annotation>(),
				returnType,  
				new abs.frontend.ast.List<ParamDecl>());
		
		Block block = new Block();
		ReturnStmt rs = new ReturnStmt();
		rs.setRetExp(new FieldUse(field));
		block.addStmt(rs);
		MethodImpl method = new MethodImpl(sig, block);
		AddMethodModifier modifier = new AddMethodModifier(method);
		return modifier;
	}
	
	private MethodSig createTestMethodSig(String methodName, 
			ParamDecl... decls) {
		MethodSig methodSig = createMethodSig(methodName, getUnit(), decls);
		methodSig.addAnnotation(getTestAnnotation(testType));
		return methodSig;
	}
	
	private MethodSig createMethodSig(String methodName, 
			Access returnType,
			ParamDecl... decls) {
		
		abs.frontend.ast.List<ParamDecl> dl = 
			new abs.frontend.ast.List<ParamDecl>();
		
		for (ParamDecl d : decls) {
			dl.add(d);
		}
		
		MethodSig method = new MethodSig(methodName,
				new abs.frontend.ast.List<Annotation>(),
				returnType, dl);
		
		return method;
	}
	
	private InterfaceDecl createInterface(String interfaceName) {
		return new InterfaceDecl(interfaceName, 
				new abs.frontend.ast.List<Annotation>(), 
				new abs.frontend.ast.List<InterfaceTypeUse>(), 
				new abs.frontend.ast.List<MethodSig>());
	}
	
	private InterfaceDecl createTestInterface(String interfaceName) {
		InterfaceDecl ti = createInterface(interfaceName);
		ti.addAnnotation(getTestAnnotation(fixtureType)); 
		return ti;
	}
	
	private abs.frontend.ast.List<ParamDecl> getParams(ParamDecl... decls) {
		abs.frontend.ast.List<ParamDecl> dl = 
			new abs.frontend.ast.List<ParamDecl>();
		
		for (ParamDecl d : decls) {
			dl.add(d);
		}
		
		return dl;
	}
	
	private InterfaceDecl getOrCreateTestInterfaceDecl(String testInterface) {
		//check if we already have defined this.
		InterfaceDecl ti = getDecl(output, InterfaceDecl.class, 
				AbsASTBuilderUtil.<InterfaceDecl>namePred(testInterface));
		
		if (ti == null) {
			ti = createTestInterface(testInterface);
			output.addDecl(ti);
		}
		
		return ti;
	}
	
	private MethodImpl createTestMethodImpl(MethodSig method) {
		MethodImpl methodImpl = new MethodImpl(method.copy(), new Block());
		return methodImpl;
	}
	
	private String className(String interfaceName) {
		return interfaceName + CLASS_SUFFIX;
	}
	
	private ClassDecl createTestClass(InterfaceDecl inf) {
		ClassDecl ct = new ClassDecl(className(inf.getName()), 
				new abs.frontend.ast.List<Annotation>(), 
				new abs.frontend.ast.List<ParamDecl>(),
				new abs.frontend.ast.List<InterfaceTypeUse>(), 
				new Opt<InitBlock>(),
				new abs.frontend.ast.List<FieldDecl>(),
				new abs.frontend.ast.List<MethodImpl>());
		
		ct.addAnnotation(getTestAnnotation(suiteType)); 
		ct.addImplementedInterfaceUse(new InterfaceTypeUse(inf.getName()));
		
		for (MethodSig m : inf.getAllMethodSigs()) {
			ct.addMethod(createTestMethodImpl(m));
		}
		
		return ct;
	}
	
	private MethodImpl findMethodImpl(ClassDecl clazz, Predicate<MethodImpl> pred) {
		for (MethodImpl m : clazz.getMethodList()) {
			if (pred.predicate(m)) {
				return m;
			}
		}
		return null;
	}
	
	private MethodSig findMethodSig(InterfaceDecl inf, Predicate<MethodSig> pred) {
		for (MethodSig m : inf.getAllMethodSigs()) {
			if (pred.predicate(m)) {
				return m;
			}
		}
		return null;
	}
	
	private void getOrCreateTestSuiteForClassMethod(
			List<TestCase> cs,
			InterfaceDecl testInterface, 
			String className,
			String methodName) {
	
		//create suite
		final ClassDecl testClass = createTestClass(testInterface);

		//find class under test.
		ClassDecl cut = getDecl(model, ClassDecl.class, 
				new DeclNamePredicate<ClassDecl>(className));
		
		//find method under test.
		MethodImpl mut = findMethodImpl(cut, new MethodNamePredicate(methodName));
		
		//find interface of class under test.
		InterfaceDecl inf = null;
		for (InterfaceTypeUse iu : cut.getImplementedInterfaceUseList()) {
			InterfaceDecl infTemp = getDecl(model, InterfaceDecl.class, 
					new DeclNamePredicate<InterfaceDecl>(iu.getName()));
			
			if (findMethodSig(infTemp, new MethodSigNamePredicate(methodName)) != null) {
				inf = infTemp;
				break;
			}
		}
		
		//add imports of class/interface under test
		output.addImport(generateImportAST(cut));
		output.addImport(generateImportAST(inf));
		
		String minf = "ModifierFieldsOf"+className+"ForTest";
		
		for (int i=0; i<cs.size(); i++) {
			TestCase c = cs.get(i);

			//initial arg
			List<ABSData> ia = ABSTestCaseExtractor.getInputArgs(c);
			Block block = testClass.getMethod(i).getBlock();
			
			//first initial arg is the reference of sut
			String out = ABSTestCaseExtractor.getABSData(ia.get(0));
			block.addStmt(newObj(inf, cut, out, false));
			
			//TODO initial states' heap contains more than one object.
			Map<ABSRef,ABSObject> is = ABSTestCaseExtractor.getInitialState(c);
			assert is.size() == 1;
			for (ABSRef r : is.keySet()) {
				makeSetStatements(r, is.get(r), block);
			}
			
			//test execution
			SyncCall test = makeTestExecutionForMethod(mut.getMethodSig(), ia);
			
			Access access = mut.getMethodSig().getReturnType();
			if (access instanceof DataTypeUse &&
				((DataTypeUse) access).getName().equals("Unit")) {
				block.addStmt(getExpStmt(test)); //no return value
			} else {
				block.addStmt(getVarDecl("returnValue", access, test));
			}
			
			//check return value
			ABSData rd = ABSTestCaseExtractor.getReturnData(c);
			
			//check return value
			Map<ABSRef,ABSObject> af = ABSTestCaseExtractor.getAfterState(c);
		}
		
		output.addDecl(testClass);
	}
	
	private void updateDelta(ClassDecl clazz) {
		String className = clazz.getName();
		
		DeltaDecl dd = getDecl(output, DeltaDecl.class, 
				new DeclNamePredicate<DeltaDecl>(className + DELTA_SUFFIX));
		
		if (dd == null) {
			dd = new DeltaDecl();
			dd.setName(className + DELTA_SUFFIX);
			
			//add Setters and Getters
			ModifyClassModifier mcm = new ModifyClassModifier();
			InterfaceDecl ai = new InterfaceDecl();
			ai.setName("ModifierFieldsOf"+className+"ForTest");
			mcm.addImplementedInterfaceUse(new InterfaceTypeUse(ai.getName()));
			for (FieldDecl fd : clazz.getFieldList()) {
				AddMethodModifier smm = addSetter(fd.getName(), fd.getAccess());
				AddMethodModifier gmm = addGetter(fd.getName(), fd.getAccess());
				mcm.addModifier(smm);
				mcm.addModifier(gmm);
				ai.addBody(smm.getMethodImpl().getMethodSig());
				ai.addBody(gmm.getMethodImpl().getMethodSig());
			}
			
			dd.addClassOrIfaceModifier(new AddInterfaceModifier(ai));
			dd.addClassOrIfaceModifier(mcm);
			
			output.addDecl(dd);
		}
	}
	
	private void makeSetStatements(ABSRef ref, ABSObject state, Block block) {
		String rn = getABSData(ref); 
		
		Map<String,ABSData> fields = getABSObjectFields(state);
		for (String fn : fields.keySet()) {
			SyncCall syncCall = new SyncCall();
			syncCall.setCallee(new VarUse(rn));
			syncCall.setMethod(SETTER_PREFIX+fn);
			ABSData d = fields.get(fn);
			
			//TODO references!
			if (d instanceof ABSTerm) {
				
			}
			
			block.addStmt(getExpStmt(syncCall));
		}
		
		updateDelta(getDecl(output, ClassDecl.class, 
				new DeclNamePredicate<ClassDecl>(getABSObjectType(state))));
		
//		block.addStmt(getVarDecl(getABSData(r), new InterfaceTypeUse(minf), 
//				new VarUse(out)));
		
	}
	
	private SyncCall makeTestExecutionForMethod(MethodSig method, List<ABSData> inArgs) {
		abs.frontend.ast.List<PureExp> ps = new abs.frontend.ast.List<PureExp>();
		SyncCall syncCall = new SyncCall();
		
		if (inArgs.size() == 0) {
			throw new IllegalStateException("Inputs for a method must at least have a reference");
		}
		
		ABSData r = inArgs.get(0);
		if (! (r instanceof ABSRef)) {
			throw new IllegalStateException("Inputs for a method must at least have a reference");
		}
		
		String rn = ABSTestCaseExtractor.getABSData(r);
		syncCall.setCallee(new VarUse(rn));
		syncCall.setMethod(method.getName());
		syncCall.setParamList(ps);
		
		for (int i=1; i<inArgs.size(); i++) {
			ABSData d = inArgs.get(i);
			String sv = ABSTestCaseExtractor.getABSData(d);
			final PureExp exp;
			if (d instanceof ABSRef) {
				exp = new VarUse(sv);
			} else if (d instanceof ABSTerm) {
				//TODO need to differentiate terms
				//Access type = method.getParam(i).getAccess();
				exp = new IntLiteral(sv);
			} else {
				throw new IllegalStateException("");
			}
			syncCall.setParam(exp,i);
		}
		return syncCall;
	}
	
	public InterfaceDecl getOrCreateTestFixtureForClassMethod(int testCaseSize, String className, 
			String methodName) {
		
		String cm = StringUtils.capitalize(methodName);
		final String testInterfaceName = className + INTERFACE_SUFFIX + "For" + cm;
		final String testMethodNamePrefix = METHOD_PREFIX+ cm ;
		
		//create fixture
		final InterfaceDecl testInterface = 
			getOrCreateTestInterfaceDecl(testInterfaceName);
		
		for (int i=1; i<=testCaseSize; i++) {
			testInterface.addBody(createTestMethodSig(testMethodNamePrefix + i));
		}
		
		return testInterface;
	}
	
	public void generateABSUnitTest(List<TestCase> cs, String mn) {
		
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

		if (className != null) {
			InterfaceDecl ti = getOrCreateTestFixtureForClassMethod(cs.size(), className, methodName);
			getOrCreateTestSuiteForClassMethod(cs, ti, className, methodName);
		}
		
	}
	
	private void gatherABSUnitAnnotations() {
		for (Decl decl : this.model.getDecls()) {
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
	}

}
