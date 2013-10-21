/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.absmodels.abs.plugin.util;

import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.util.Arrays;
import java.util.List;

import org.absmodels.abs.plugin.builder.AbsNature;
import org.absmodels.abs.plugin.util.InternalASTNode;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

import abs.frontend.ast.CompilationUnit;
import abs.frontend.ast.Model;
import abs.frontend.ast.ModuleDecl;
import abs.frontend.ast.ClassDecl;

public class InternalASTNodeTests {

	private AbsNature natureMock;
	
	private ModuleDecl moduleDecl1,moduleDecl2,moduleDecl3,moduleDecl4;
	/**
	 * @throws java.lang.Exception
	 */
	@Before
	public void setUp() throws Exception {
		natureMock = mock(AbsNature.class);
		natureMock.modelLock = new Object();
		moduleDecl1 = mock(ModuleDecl.class);
		moduleDecl2 = mock(ModuleDecl.class);
		moduleDecl3 = mock(ModuleDecl.class);
		moduleDecl4 = mock(ModuleDecl.class);
		
		when(moduleDecl1.getName()).thenReturn("A");
		when(moduleDecl2.getName()).thenReturn("A.A");
		when(moduleDecl3.getName()).thenReturn("A.A.A");
		when(moduleDecl4.getName()).thenReturn("A.A.A.A");
		
		Model modelMock = mock(Model.class);
		when(modelMock.getModuleDecls()).thenReturn(Arrays.asList(moduleDecl1,moduleDecl2,moduleDecl3,moduleDecl4));
		when(natureMock.getCompleteModel()).thenReturn(modelMock);
		
	}

	@After
	public void tearDown() throws Exception {
	}

	@Test
	public void testGetASTNode() {
		InternalASTNode<ModuleDecl> internalASTNode = new InternalASTNode<ModuleDecl>(moduleDecl1, natureMock);
		
		try {
			new InternalASTNode<ModuleDecl>(moduleDecl1,null);
			fail("Null argument allowed!");
		}catch(IllegalArgumentException ia){
			//right behaviour here...
		}
		
		try {
			new InternalASTNode<ModuleDecl>(null,natureMock);
			fail("Null argument allowed!");
		}catch(IllegalArgumentException ia){
			//right behaviour here...
		}		
		
		assertSame(internalASTNode.getNature(),natureMock);
		assertSame(internalASTNode.getASTNode(),moduleDecl1);
		
	}

	@Test
	public void testHasASTNodeOfType() {
		InternalASTNode<ModuleDecl> internalASTNode = new InternalASTNode<ModuleDecl>(moduleDecl1, natureMock);
		
		assertTrue(internalASTNode.hasASTNodeOfType(ModuleDecl.class));
		assertTrue(!internalASTNode.hasASTNodeOfType(CompilationUnit.class));
		assertTrue(!internalASTNode.hasASTNodeOfType(ClassDecl.class));
		
	}

	@Test
	public void testWrapASTNodes() {
		final ModuleDecl[] arr = new ModuleDecl[]{moduleDecl1,moduleDecl2,moduleDecl3};
		final List<ModuleDecl> list = Arrays.asList(arr);
		List<InternalASTNode<ModuleDecl>> wrapASTNodes = InternalASTNode.wrapASTNodes(arr, natureMock);
		
		for (InternalASTNode<ModuleDecl> im : wrapASTNodes){
			assertSame(im.getNature(),natureMock);
			assertTrue(list.contains(im.getASTNode()));
			
		}
	}

	@Test
	public void testWrapASTNode() {
		InternalASTNode<ModuleDecl> wrapASTNode1 = new InternalASTNode<ModuleDecl>(moduleDecl1, natureMock);
		
		assertTrue(wrapASTNode1 instanceof InternalASTNode<?>);
		assertTrue(wrapASTNode1.hasASTNodeOfType(ModuleDecl.class));
	}

}
