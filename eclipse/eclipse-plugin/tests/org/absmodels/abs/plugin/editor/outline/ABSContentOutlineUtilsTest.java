/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.absmodels.abs.plugin.editor.outline;

import static org.absmodels.abs.plugin.editor.outline.ABSContentOutlineUtils.*;
import static org.junit.Assert.*;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.util.ArrayList;

import org.absmodels.abs.plugin.builder.AbsNature;
import org.absmodels.abs.plugin.navigator.ModulePath;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.mockito.Mockito;

import abs.frontend.ast.*;

public class ABSContentOutlineUtilsTest {

	
	private ModuleDecl moduleMock;
	private List<Import> impL = new List<Import>();
	private List<Export> expL = new List<Export>();
	
	private ClassDecl classDecl = mock(ClassDecl.class);
	private ClassDecl classDecl2 = mock(ClassDecl.class);
	private MainBlock block = mock(MainBlock.class);
	
	private AbsNature natureMock;
	private TypeUse   tu;
	private ParametricDataTypeUse pdtu;
	
	@Before
	public void setUp() throws Exception {
		natureMock = mock(AbsNature.class);
		natureMock.modelLock = new Object();
		moduleMock = mock(ModuleDecl.class,Mockito.RETURNS_DEEP_STUBS);
		
		when(moduleMock.getNumExport()).thenReturn(1);
		when(moduleMock.getNumImport()).thenReturn(1);
		when(moduleMock.hasBlock()).thenReturn(true);
		when(moduleMock.getName()).thenReturn("A.B");
		
		Import imp1 = mock(Import.class);
		Import imp2 = mock(Import.class);
		Export exp = mock(Export.class);
				
		impL.add(imp1);
		impL.add(imp2);
		expL.add(exp);
		
		when(moduleMock.getImportList()).thenReturn(impL);
		when(moduleMock.getExportList()).thenReturn(expL);
		
		when(moduleMock.getBlock()).thenReturn(block);
		
		List<Decl> declL = new List<Decl>();
		declL.add(classDecl);
		declL.add(classDecl2);
		when(moduleMock.getDeclList()).thenReturn(declL);
		
		tu = mock(TypeUse.class);
		pdtu = mock(ParametricDataTypeUse.class);
		
		when(tu.getName()).thenReturn("Int");
		when(pdtu.getName()).thenReturn("Int");
		when(pdtu.getNumParam()).thenReturn(1);
		
		List<TypeUse> dtuL = new List<TypeUse>();
		DataTypeUse dtu = mock(DataTypeUse.class);
		when(dtu.getName()).thenReturn("Parametric");
		dtuL.add(dtu);
		when(pdtu.getParamList()).thenReturn(dtuL);
	}

	@After
	public void tearDown() throws Exception {
	}

	@Test
	public void testIsImportExportList() {
		List<Import> list = new List<Import>();
		Import imp = mock(Import.class);
		list.add(imp);
		
		assertTrue(isImportList(list));
		assertFalse(isExportList(list));
		
		List<Export> list2 = new List<Export>();
		Export exp = mock(Export.class);
		list2.add(exp);		
		
		assertFalse(isImportList(list2));			
		assertTrue(isExportList(list2));	
	}
	
	@Test
	public void testformatMethodSig(){
		MethodSig sigMock = mock(MethodSig.class);
		when(sigMock.getName()).thenReturn("methodName");
		
		List<ParamDecl> paramDeclL = new List<ParamDecl>();
		ParamDecl pd1 = mock(ParamDecl.class);
		ParamDecl pd2 = mock(ParamDecl.class);
			
		when(pd1.getAccess()).thenReturn(tu);
		when(pd2.getAccess()).thenReturn(pdtu);
		
		paramDeclL.add(pd1);
		paramDeclL.add(pd2);
		
		when(sigMock.getParamList()).thenReturn(paramDeclL);
		
		assertEquals("methodName(Int, Int<Parametric>)",formatMethodSig(sigMock).toString());
		
		MethodSig sigMock2 = mock(MethodSig.class);
		when(sigMock2.getName()).thenReturn("methodName");
		
		List<ParamDecl> paramDeclL2 = new List<ParamDecl>();
		
		when(sigMock.getParamList()).thenReturn(paramDeclL2);
		
		assertEquals("methodName()",formatMethodSig(sigMock).toString());		
		
	}
	
	@Test
	public void testFormatTypedVarOrFieldDecl(){
		
		TypedVarOrFieldDecl tvofd1 = mock(TypedVarOrFieldDecl.class);
		when(tvofd1.getName()).thenReturn("Field1");
		when(tvofd1.getAccess()).thenReturn(tu);
		
		TypedVarOrFieldDecl tvofd2 = mock(TypedVarOrFieldDecl.class);
		when(tvofd2.getName()).thenReturn("Field2");
		when(tvofd2.getAccess()).thenReturn(pdtu);		
		
		assertEquals("Field1 : Int",formatTypedVarOrFieldDecl(tvofd1).toString());
		assertEquals("Field2 : Int<Parametric>",formatTypedVarOrFieldDecl(tvofd2).toString());
		assertTrue(formatTypedVarOrFieldDecl(null) == null);
	}
	
	@Test
	public void testGetChildrenOf(){			
		ArrayList<ASTNode<?>> chld = getChildrenOf(moduleMock);
		
		assertTrue(chld.contains(impL));
		assertTrue(chld.contains(expL));
		assertTrue(chld.contains(classDecl));
		assertTrue(chld.contains(classDecl2));
		assertTrue(chld.contains(block));
		assertTrue(chld.size() == 5);		
	}

	@Test
	public void testGetLabelModulePath() {
		ModulePath moduleMock0 = mock(ModulePath.class);
		when(moduleMock0.getModulePath()).thenReturn("A.A.B");
		
		ModulePath moduleMock1 = mock(ModulePath.class);
		when(moduleMock1.getModulePath()).thenReturn("A.A");
		
		ModulePath moduleMock2 = mock(ModulePath.class);
		when(moduleMock2.getModulePath()).thenReturn("A");
		
		ModulePath moduleMock3 = mock(ModulePath.class);
		when(moduleMock3.getModulePath()).thenReturn("");
		
		assertEquals("B",getLabel(moduleMock0).toString());
		assertEquals("A",getLabel(moduleMock1).toString());
		assertEquals("A",getLabel(moduleMock2).toString());
		assertEquals("",getLabel(moduleMock3).toString());
	}

}
