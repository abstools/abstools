package apet.absunit;

import static abs.backend.tests.AbsASTBuilderUtil.generateImportAST;
import static abs.backend.tests.AbsASTBuilderUtil.getDecl;
import static abs.backend.tests.AbsASTBuilderUtil.namePred;
import static apet.testCases.ABSTestCaseExtractor.getABSDataType;
import static apet.testCases.ABSTestCaseExtractor.getABSDataValue;
import static apet.testCases.ABSTestCaseExtractor.getABSTermArgs;
import static apet.testCases.ABSTestCaseExtractor.getABSTermFunctor;
import static apet.testCases.ABSTestCaseExtractor.getABSTermTypeName;

import java.util.List;
import java.util.Set;

import abs.frontend.ast.DataConstructorExp;
import abs.frontend.ast.DataTypeDecl;
import abs.frontend.ast.Decl;
import abs.frontend.ast.IntLiteral;
import abs.frontend.ast.InterfaceDecl;
import abs.frontend.ast.Model;
import abs.frontend.ast.ModuleDecl;
import abs.frontend.ast.NullExp;
import abs.frontend.ast.PureExp;
import abs.frontend.ast.StringLiteral;
import abs.frontend.ast.TypeSynDecl;
import abs.frontend.ast.VarUse;
import apet.testCases.ABSData;
import apet.testCases.ABSRef;
import apet.testCases.ABSTerm;

/**
 * 
 * @author woner
 *
 */
final class PureExpressionBuilder {
	
	private final String INT = "Int";
	private final String STRING = "String";
	
	private final Model model;
	private final ModuleDecl output;
	private final HeapReferenceBuilder heapRefBuilder = new HeapReferenceBuilder();
	
	PureExpressionBuilder(Model input, ModuleDecl output) {
		this.model = input;
		this.output = output;
	}
	
	/**
	 * @param heapNames 
	 * @param dataValue
	 * @return
	 */
	PureExp createPureExpression(String testName, Set<String> heapNames, ABSData dataValue) {
		String type = getABSDataType(dataValue);
		String value = getABSDataValue(dataValue);
		
		if (type.contains("(") && dataValue instanceof ABSTerm) {
			ABSTerm term = (ABSTerm) dataValue;
			type = getABSTermTypeName(term);
		}
		
		//import type
		Decl decl = getDecl(model, Decl.class, namePred(type));
		output.addImport(generateImportAST(decl));
		
		if (dataValue instanceof ABSTerm) {
			ABSTerm term = (ABSTerm) dataValue;
			return makeDataTermValue(testName, heapNames, term, decl);
		} else if (dataValue instanceof ABSRef) {
			if (heapNames.contains(value)) {
				return new VarUse(heapRefBuilder.heapReferenceForTest(testName, value));
			} else {
				return new NullExp();
			}
		} else {
			throw new IllegalStateException("Cannot handle ABSData that is not " +
					"either a ABSRef or a ABSTerm");
		}
	}
	
	/**
	 * Construct a pure expression that is either a primitive literal
	 * such as Int and String, or a value of a data type.
	 * @param heap 
	 * @param testName 
	 * 
	 * @param value
	 * @param decl 
	 * @return
	 */
	PureExp makeDataTermValue(String testName, Set<String> heap, 
			ABSTerm term, Decl decl) {
		if (decl instanceof TypeSynDecl) {
			return parseValue(testName, heap, term);
		} else if (decl instanceof DataTypeDecl) {
			if (STRING.equals(decl.getName())) {
				return new StringLiteral(getABSDataValue(term));
			} else if (INT.equals(decl.getName())) {
				return new IntLiteral(getABSDataValue(term));
			} else {
				return parseValue(testName, heap, term);
			}
		} else if (decl instanceof InterfaceDecl) {
			String value = getABSDataValue(term);
			if (heap.contains(value)) {
				return new VarUse(heapRefBuilder.heapReferenceForTest(testName, value));
			} else {
				return new NullExp();
			}
		} else {
			throw new IllegalStateException("Cannot handle declaration type "+decl);
		}
	}
	
	DataConstructorExp parseValue(String testName, Set<String> heap, ABSTerm term) {
		
		final DataConstructorExp result = new DataConstructorExp();
		String fn = getABSTermFunctor(term);
		result.setConstructor(fn);
		result.setParamList(new abs.frontend.ast.List<PureExp>());
		List<ABSData> vs = getABSTermArgs(term);
		for (int i=0; i<vs.size(); i++) {
			result.setParam(createPureExpression(testName, heap, vs.get(i)),i);
		}
		return result;
	}
}
