/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.absmodels.abs.plugin.wizards;

import static org.absmodels.abs.plugin.wizards.WizardUtil.*;
import static org.junit.Assert.*;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import org.absmodels.abs.plugin.builder.AbsNature;
import org.absmodels.abs.plugin.util.InternalASTNode;
import org.eclipse.core.resources.IProject;
import org.junit.Test;

import abs.frontend.ast.ClassDecl;
import abs.frontend.ast.Decl;
import abs.frontend.ast.InterfaceDecl;
import abs.frontend.ast.List;
import abs.frontend.ast.ModuleDecl;


public class WizardUtilsTests {

	@Test
	public void testValidateModule() {
		String valid1 = "A";
		String valid2 = "A.B";
		String valid3 = "A.b";
		String invalid1 = "";
		String invalid2 = "a";
		String invalid3 = "a.b";
		String invalid4 = ";";
		String invalid5 = "A.?";
		String invalid6 = "A.5";
		String invalid7 = "module";
		
		assertTrue(validateModule(valid1).equals(ErrorType.NO_ERROR));
		assertTrue(validateModule(valid2).equals(ErrorType.NO_ERROR));
		assertTrue(validateModule(valid3).equals(ErrorType.NO_ERROR));
		assertTrue(validateModule(invalid1).equals(ErrorType.ERROR_NO_NAME));
		assertTrue(validateModule(invalid2).equals(ErrorType.ERROR_NO_UPPER_CASE));
		assertTrue(validateModule(invalid3).equals(ErrorType.ERROR_NO_UPPER_CASE));
		assertTrue(validateModule(invalid4).equals(ErrorType.ERROR_INVALID_NAME));
		assertTrue(validateModule(invalid5).equals(ErrorType.ERROR_INVALID_NAME));
		assertTrue(validateModule(invalid6).equals(ErrorType.ERROR_INVALID_NAME));
		assertTrue(validateModule(invalid7).equals(ErrorType.ERROR_KEYWORD));
		
	}

	@Test
	public void testValidate() {
		String valid1 = "A";
		String valid2 = "Abc3";
		String valid3 = "ABC3";
		String invalid1 = "";
		String invalid2 = "a";
		String invalid3 = "a.b";
		String invalid4 = ";";
		String invalid5 = "module";
		
		assertTrue(validate(valid1).equals(ErrorType.NO_ERROR));
		assertTrue(validate(valid2).equals(ErrorType.NO_ERROR));
		assertTrue(validate(valid3).equals(ErrorType.NO_ERROR));
		assertTrue(validate(invalid1).equals(ErrorType.ERROR_NO_NAME));
		assertTrue(validate(invalid2).equals(ErrorType.ERROR_NO_UPPER_CASE));
		assertTrue(validate(invalid3).equals(ErrorType.ERROR_NO_UPPER_CASE));
		assertTrue(validate(invalid4).equals(ErrorType.ERROR_INVALID_NAME));
		assertTrue(validate(invalid5).equals(ErrorType.ERROR_KEYWORD));
	}

	@Test
	public void testValidateClass() {
		ModuleDecl mockDecl = mock(ModuleDecl.class);
		List<Decl> list = new List<Decl>();
		
		ClassDecl m1 = mock(ClassDecl.class);
		ClassDecl m2 = mock(ClassDecl.class);
		when(m1.getName()).thenReturn("Class1");
		when(m2.getName()).thenReturn("Class2");
		
		list.add(m1);
		list.add(m2);
		
		when(mockDecl.getDecls()).thenReturn(list);

		String valid1 = "A";
		String valid2 = "Abc3";
		String valid3 = "ABC3";
		String invalid1 = "";
		String invalid2 = "a";
		String invalid3 = "a.b";
		String invalid4 = ";";
		String invalid5 = "module";
		String invalid6 = "Class1";
		String invalid7 = "Class2";
		
		assertTrue(validateClass(valid1,mockDecl).equals(ErrorType.NO_ERROR));
		assertTrue(validateClass(valid2,mockDecl).equals(ErrorType.NO_ERROR));
		assertTrue(validateClass(valid3,mockDecl).equals(ErrorType.NO_ERROR));
		assertTrue(validateClass(invalid1,mockDecl).equals(ErrorType.ERROR_NO_NAME));
		assertTrue(validateClass(invalid2,mockDecl).equals(ErrorType.ERROR_NO_UPPER_CASE));
		assertTrue(validateClass(invalid3,mockDecl).equals(ErrorType.ERROR_NO_UPPER_CASE));
		assertTrue(validateClass(invalid4,mockDecl).equals(ErrorType.ERROR_INVALID_NAME));
		assertTrue(validateClass(invalid5,mockDecl).equals(ErrorType.ERROR_KEYWORD));
		assertTrue(validateClass(invalid6,mockDecl).equals(ErrorType.ERROR_DUPLICATE_NAME));
		assertTrue(validateClass(invalid7,mockDecl).equals(ErrorType.ERROR_DUPLICATE_NAME));		
		
	}

	@Test
	public void testValidateInterface() {
		ModuleDecl mockDecl = mock(ModuleDecl.class);
		List<Decl> list = new List<Decl>();
		
		InterfaceDecl m1 = mock(InterfaceDecl.class);
		InterfaceDecl m2 = mock(InterfaceDecl.class);
		when(m1.getName()).thenReturn("Interface1");
		when(m2.getName()).thenReturn("Interface2");
		
		list.add(m1);
		list.add(m2);
		
		when(mockDecl.getDecls()).thenReturn(list);

		String valid1 = "A";
		String valid2 = "Abc3";
		String valid3 = "ABC3";
		String invalid1 = "";
		String invalid2 = "a";
		String invalid3 = "a.b";
		String invalid4 = ";";
		String invalid5 = "module";
		String invalid6 = "Interface1";
		String invalid7 = "Interface2";
		
		assertTrue(validateInterface(valid1,mockDecl).equals(ErrorType.NO_ERROR));
		assertTrue(validateInterface(valid2,mockDecl).equals(ErrorType.NO_ERROR));
		assertTrue(validateInterface(valid3,mockDecl).equals(ErrorType.NO_ERROR));
		assertTrue(validateInterface(invalid1,mockDecl).equals(ErrorType.ERROR_NO_NAME));
		assertTrue(validateInterface(invalid2,mockDecl).equals(ErrorType.ERROR_NO_UPPER_CASE));
		assertTrue(validateInterface(invalid3,mockDecl).equals(ErrorType.ERROR_NO_UPPER_CASE));
		assertTrue(validateInterface(invalid4,mockDecl).equals(ErrorType.ERROR_INVALID_NAME));
		assertTrue(validateInterface(invalid5,mockDecl).equals(ErrorType.ERROR_KEYWORD));
		assertTrue(validateInterface(invalid6,mockDecl).equals(ErrorType.ERROR_DUPLICATE_NAME));
		assertTrue(validateInterface(invalid7,mockDecl).equals(ErrorType.ERROR_DUPLICATE_NAME));		
	}

	@Test
	public void testGetProjectOfModuleDecl() {
		AbsNature natureMock = mock(AbsNature.class);
		IProject project = mock(IProject.class);
		when(project.getName()).thenReturn("");
		when(natureMock.getProject()).thenReturn(project);

		ModuleDecl moduleMock = mock(ModuleDecl.class);
		
		InternalASTNode<ModuleDecl> node = new InternalASTNode<ModuleDecl>(moduleMock,natureMock);
		
		assertSame(node.getProject(),project);
	}

}
