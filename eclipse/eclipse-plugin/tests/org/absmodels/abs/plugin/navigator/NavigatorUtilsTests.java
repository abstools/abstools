/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.absmodels.abs.plugin.navigator;

import static org.junit.Assert.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.util.Arrays;

import org.absmodels.abs.plugin.builder.AbsNature;
import org.absmodels.abs.plugin.navigator.NavigatorUtils;
import org.absmodels.abs.plugin.util.InternalASTNode;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

import abs.frontend.ast.Model;
import abs.frontend.ast.ModuleDecl;

/**
 * @author Christian
 *
 */
public class NavigatorUtilsTests {
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

	/**
	 * @throws java.lang.Exception
	 */
	@After
	public void tearDown() throws Exception {
	}

	/**
	 * Test method for {@link org.absmodels.abs.plugin.navigator.NavigatorUtils#hasSubModules(abs.frontend.ast.ModuleDecl, org.absmodels.abs.plugin.builder.AbsNature)}.
	 */
	@Test
	public void testHasSubModulesModuleDeclAbsNature() {
		assertTrue(NavigatorUtils.hasSubModules(moduleDecl1, natureMock));
		assertTrue(NavigatorUtils.hasSubModules(moduleDecl2, natureMock));
		assertTrue(NavigatorUtils.hasSubModules(moduleDecl3, natureMock));
		assertTrue(!NavigatorUtils.hasSubModules(moduleDecl4, natureMock));
	}

	/**
	 * Test method for {@link org.absmodels.abs.plugin.navigator.NavigatorUtils#hasSubModules(org.absmodels.abs.plugin.util.InternalASTNode)}.
	 */
	@Test
	public void testHasSubModulesInternalASTNodeOfModuleDecl() {
		assertTrue(NavigatorUtils.hasSubModules(new InternalASTNode<ModuleDecl>(moduleDecl1, natureMock)));
		assertTrue(NavigatorUtils.hasSubModules(new InternalASTNode<ModuleDecl>(moduleDecl2, natureMock)));
		assertTrue(NavigatorUtils.hasSubModules(new InternalASTNode<ModuleDecl>(moduleDecl3, natureMock)));
		assertTrue(!NavigatorUtils.hasSubModules(new InternalASTNode<ModuleDecl>(moduleDecl4, natureMock)));
	}

}

