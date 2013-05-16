package apet.absunit;

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
import abs.frontend.ast.DataTypeUse;
import abs.frontend.ast.Decl;
import abs.frontend.ast.IntLiteral;
import abs.frontend.ast.InterfaceDecl;
import abs.frontend.ast.Model;
import abs.frontend.ast.NullExp;
import abs.frontend.ast.ParametricDataTypeDecl;
import abs.frontend.ast.PureExp;
import abs.frontend.ast.StringLiteral;
import abs.frontend.ast.TypeParameterDecl;
import abs.frontend.ast.TypeSynDecl;
import abs.frontend.ast.TypeUse;
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
	private final HeapReferenceBuilder heapRefBuilder = new HeapReferenceBuilder();
	private final Set<String> importModules;
	
	PureExpressionBuilder(Model input, Set<String> importModules) {
		this.model = input;
		this.importModules = importModules;
	}
	
	void updateInitialisationOrder(List<String> initialisationsOrders, String cr, String nr) {
		int cpos = initialisationsOrders.indexOf(cr);
		int npos = initialisationsOrders.indexOf(nr);
		if (npos < 0 || npos > cpos) {
			initialisationsOrders.add(cpos, nr);
		}
	}
	
	PureExp createPureExpression(
			String testName, Set<String> heapNames, ABSData dataValue) {
		return createPureExpression(null, null, testName, heapNames, dataValue);
	}
	
	/**
	 * @param currentHeapReference 
	 * @param initialisationsOrders 
	 * @param heapNames 
	 * @param dataValue
	 * @return
	 */
	PureExp createPureExpression(
			String currentHeapReference, 
			List<String> initialisationsOrders, 
			String testName, Set<String> heapNames, ABSData dataValue) {
		
		String type = getABSDataType(dataValue);
		String value = getABSDataValue(dataValue);
		
		if (type.contains("(") && dataValue instanceof ABSTerm) {
			ABSTerm term = (ABSTerm) dataValue;
			type = getABSTermTypeName(term);
		}
		
		//import type
		Decl decl = getDecl(model, Decl.class, namePred(type));
		importType(decl);
		
		if (dataValue instanceof ABSTerm) {
			ABSTerm term = (ABSTerm) dataValue;
			return makeDataTermValue(currentHeapReference, 
					initialisationsOrders, testName, heapNames, term, decl);
		} else if (dataValue instanceof ABSRef) {
			if (heapNames.contains(value)) {	
				String ref = heapRefBuilder.heapReferenceForTest(testName, value);
				if (currentHeapReference != null) {
					//we need to consider initialisation order!
					updateInitialisationOrder(initialisationsOrders, currentHeapReference, ref);
				}
				return new VarUse(ref);
			} else {
				return new NullExp();
			}
		} else {
			throw new IllegalStateException("Cannot handle ABSData that is not " +
					"either a ABSRef or a ABSTerm");
		}
	}
	
	/**
	 * Resolve the type synonyms to its raw type (interface or data type)
	 * @param d
	 * @return
	 */
	Decl resolveTypeSynonym(TypeSynDecl d) {
		TypeUse use = d.getValue();
		Decl decl = getDecl(model, Decl.class, namePred(use.getName()));
		if (decl instanceof TypeSynDecl) {
			return resolveTypeSynonym((TypeSynDecl) decl);
		}
		return decl;
	}
	
	void importType(Decl decl) {
		importModules.add(decl.getModuleDecl().getName());
		
		if (decl instanceof TypeSynDecl) {
			decl = resolveTypeSynonym((TypeSynDecl) decl);
			importModules.add(decl.getModuleDecl().getName());
		} 
		
		//import type parameters
		if (decl instanceof ParametricDataTypeDecl) {
			for (TypeParameterDecl t : ((ParametricDataTypeDecl) decl).getTypeParameters()) {
				Decl type = getDecl(model, Decl.class, namePred(t.getName()));
				if (type == null) {
					//most likely a generic type
					continue;
				}
				importType(type);
			}
		}
	}
	
	/**
	 * Construct a pure expression that is either a primitive literal
	 * such as Int and String, or a value of a data type.
	 * @param currentHeapReference 
	 * 
	 * @param initialisationsOrders 
	 * @param heap 
	 * @param testName 
	 * @param value
	 * @param decl
	 *  
	 * @return
	 */
	PureExp makeDataTermValue(String currentHeapReference, 
			List<String> initialisationsOrders, 
			String testName, Set<String> heap, 
			ABSTerm term, Decl decl) {
		
		if (decl instanceof TypeSynDecl) {
			decl = resolveTypeSynonym((TypeSynDecl) decl);
		}
		
		if (decl instanceof DataTypeDecl) {
			if (STRING.equals(decl.getName())) {
				return new StringLiteral(getABSDataValue(term));
			} else if (INT.equals(decl.getName())) {
				return new IntLiteral(getABSDataValue(term));
			} else {
				return parseValue(currentHeapReference, 
						initialisationsOrders, testName, heap, term);
			}
		} else if (decl instanceof InterfaceDecl) {
			String value = getABSDataValue(term);
			if (heap.contains(value)) {
				String ref = heapRefBuilder.heapReferenceForTest(testName, value);
				if (currentHeapReference != null) {
					//we need to consider initialisation order!
					updateInitialisationOrder(initialisationsOrders, currentHeapReference, ref);
				}
				return new VarUse(ref);
			} else {
				return new NullExp();
			}
		} else {
			throw new IllegalStateException("Cannot handle declaration type "+decl);
		}
	}
	
	DataConstructorExp parseValue(String currentHeapReference, 
			List<String> initialisationsOrders, 
			String testName, Set<String> heap, ABSTerm term) {
		
		final DataConstructorExp result = new DataConstructorExp();
		String fn = getABSTermFunctor(term);
		result.setConstructor(fn);
		result.setParamList(new abs.frontend.ast.List<PureExp>());
		List<ABSData> vs = getABSTermArgs(term);
		for (int i=0; i<vs.size(); i++) {
			result.setParam(createPureExpression(currentHeapReference, 
					initialisationsOrders, testName, heap, vs.get(i)),i);
		}
		return result;
	}
}
