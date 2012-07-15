package apet.absunit;

import static abs.backend.tests.AbsASTBuilderUtil.*;
import static apet.absunit.ABSUnitTestCaseTranslatorConstants.RUN_METHOD;
import abs.backend.tests.AbsASTBuilderUtil.DeclNamePredicate;
import abs.frontend.ast.Access;
import abs.frontend.ast.AddInterfaceModifier;
import abs.frontend.ast.AddMethodModifier;
import abs.frontend.ast.Annotation;
import abs.frontend.ast.Block;
import abs.frontend.ast.ClassDecl;
import abs.frontend.ast.DeltaDecl;
import abs.frontend.ast.FieldDecl;
import abs.frontend.ast.FieldUse;
import abs.frontend.ast.InterfaceDecl;
import abs.frontend.ast.InterfaceTypeUse;
import abs.frontend.ast.MethodImpl;
import abs.frontend.ast.MethodSig;
import abs.frontend.ast.ModifyClassModifier;
import abs.frontend.ast.ModuleDecl;
import abs.frontend.ast.ParamDecl;
import abs.frontend.ast.RemoveMethodModifier;
import abs.frontend.ast.ReturnStmt;
import abs.frontend.ast.VarUse;

final class DeltaForGetSetFieldsBuilder {

	private final TestCaseNamesBuilder testCaseNameBuilder = new TestCaseNamesBuilder();
	private final ModuleDecl output;
	
	DeltaForGetSetFieldsBuilder(ModuleDecl output) {
		this.output = output;
	}
	
	DeltaDecl getOrCreateDeltaFor(String testClassName) {
		String deltaName = testCaseNameBuilder.deltaOnClass(testClassName);
		DeltaDecl delta = getDecl(output, DeltaDecl.class, 
				new DeclNamePredicate<DeltaDecl>(deltaName));
			
		if (delta == null) {
			delta = new DeltaDecl();
			delta.setName(deltaName);
			output.addDecl(delta);
		}
		
		return delta;
	}
	
	RemoveMethodModifier removeRun() {
		RemoveMethodModifier modifier = 
			new RemoveMethodModifier(createMethodSig(RUN_METHOD, getUnit()));
		return modifier;
	}
	
	/**
	 * Add an add method modifier
	 * @param fieldName
	 * @param exp
	 * @param decl
	 * @return
	 */
	AddMethodModifier addSetter(
			String fieldName, Access type) {
		MethodSig sig = new MethodSig(testCaseNameBuilder.setterMethodName(fieldName), 
				new abs.frontend.ast.List<Annotation>(),
				getUnit(),  
				new abs.frontend.ast.List<ParamDecl>());
		
		sig.addParam(new ParamDecl("v", type, new abs.frontend.ast.List<Annotation>()));
		Block block = new Block();
		block.addStmt(getVAssign(new FieldUse(fieldName), new VarUse("v")));
		MethodImpl method = new MethodImpl(sig, block);
		AddMethodModifier modifier = new AddMethodModifier(method);
		return modifier;
	}
	
	AddMethodModifier addGetter(String fieldName, Access returnType) {
		MethodSig sig = new MethodSig(testCaseNameBuilder.getterMethodName(fieldName), 
				new abs.frontend.ast.List<Annotation>(),
				returnType,  
				new abs.frontend.ast.List<ParamDecl>());
		
		Block block = new Block();
		ReturnStmt rs = new ReturnStmt();
		rs.setRetExp(new FieldUse(fieldName));
		block.addStmt(rs);
		MethodImpl method = new MethodImpl(sig, block);
		AddMethodModifier modifier = new AddMethodModifier(method);
		return modifier;
	}
	
	/**
	 * Add a delta that adds Getters and Setters for clazz.
	 * @param clazz
	 */
	void updateDelta(ClassDecl clazz) {
		String className = clazz.getName();
		
		String deltaOnClassName = 
				testCaseNameBuilder.deltaOnClass(className);
		String interfaceForModifyingClassFieldName = 
				testCaseNameBuilder.interfaceForModifyingFieldOfClass(className);
		
		DeltaDecl dd = getDecl(output, DeltaDecl.class, 
				new DeclNamePredicate<DeltaDecl>(deltaOnClassName));
		
		if (dd == null) {
			dd = new DeltaDecl();
			dd.setName(deltaOnClassName);
			
			//add Setters and Getters
			ModifyClassModifier mcm = new ModifyClassModifier();
			mcm.setName(className);

			InterfaceDecl ai = new InterfaceDecl();
			ai.setName(interfaceForModifyingClassFieldName);
			mcm.addImplementedInterfaceUse(new InterfaceTypeUse(ai.getName()));
			
			for (MethodImpl m : clazz.getMethodList()) {
				if (RUN_METHOD.equals(m.getMethodSig().getName())) {
					mcm.addModifier(removeRun());
					break;
				}
			}
			
			for (FieldDecl fd : clazz.getFieldList()) {
				AddMethodModifier smm = addSetter(fd.getName(), (Access) fd.getAccess().fullCopy());
				AddMethodModifier gmm = addGetter(fd.getName(), (Access) fd.getAccess().fullCopy());
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
	
}
