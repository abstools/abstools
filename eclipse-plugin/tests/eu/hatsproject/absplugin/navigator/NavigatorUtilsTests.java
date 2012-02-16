/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package eu.hatsproject.absplugin.navigator;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.util.ArrayList;
import java.util.Arrays;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;

import abs.frontend.ast.Model;
import abs.frontend.ast.ModuleDecl;
import eu.hatsproject.absplugin.builder.AbsNature;
import eu.hatsproject.absplugin.navigator.ModulePath;
import eu.hatsproject.absplugin.navigator.NavigatorUtils;
import eu.hatsproject.absplugin.util.InternalASTNode;

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
	 * Test method for {@link eu.hatsproject.absplugin.navigator.NavigatorUtils#getParentHierarchyForModuleDecl(abs.frontend.ast.ModuleDecl, eu.hatsproject.absplugin.builder.AbsNature)}.
	 */
	@Test
	public void testGetParentHierarchyForModuleDecl() {

		
		ArrayList<ModulePath> parentHierarchyForModuleDecl1 = NavigatorUtils.getParentHierarchyForModuleDecl(moduleDecl1, natureMock);
		ModulePath[] array2 = NavigatorUtils.getParentHierarchyForModuleDecl(moduleDecl2, natureMock).toArray(new ModulePath[0]);
		ModulePath[] array3 = NavigatorUtils.getParentHierarchyForModuleDecl(moduleDecl3, natureMock).toArray(new ModulePath[0]);
		ModulePath[] array4 = NavigatorUtils.getParentHierarchyForModuleDecl(moduleDecl4, natureMock).toArray(new ModulePath[0]);
		
		assertTrue(parentHierarchyForModuleDecl1.isEmpty());
		
		assertTrue(array2.length == 1);
		assertEquals(array2[0].getModulePath(),"A");
		assertSame(array2[0].getNature(),natureMock);
		
		assertEquals(array3[0].getModulePath(),"A");
		assertSame(array3[0].getNature(),natureMock);
		assertEquals(array3[1].getModulePath(),"A.A");
		assertSame(array3[1].getNature(),natureMock);
		
		assertEquals(array4[0].getModulePath(),"A");
		assertSame(array4[0].getNature(),natureMock);
		assertEquals(array4[1].getModulePath(),"A.A");
		assertSame(array4[1].getNature(),natureMock);
		assertEquals(array4[2].getModulePath(),"A.A.A");
		assertSame(array4[2].getNature(),natureMock);	
		
	}

	/**
	 * Test method for {@link eu.hatsproject.absplugin.navigator.NavigatorUtils#hasSubModules(abs.frontend.ast.ModuleDecl, eu.hatsproject.absplugin.builder.AbsNature)}.
	 */
	@Test
	public void testHasSubModulesModuleDeclAbsNature() {
		assertTrue(NavigatorUtils.hasSubModules(moduleDecl1, natureMock));
		assertTrue(NavigatorUtils.hasSubModules(moduleDecl2, natureMock));
		assertTrue(NavigatorUtils.hasSubModules(moduleDecl3, natureMock));
		assertTrue(!NavigatorUtils.hasSubModules(moduleDecl4, natureMock));
	}

	/**
	 * Test method for {@link eu.hatsproject.absplugin.navigator.NavigatorUtils#hasSubModules(eu.hatsproject.absplugin.util.InternalASTNode)}.
	 */
	@Test
	public void testHasSubModulesInternalASTNodeOfModuleDecl() {
		assertTrue(NavigatorUtils.hasSubModules(new InternalASTNode<ModuleDecl>(moduleDecl1, natureMock)));
		assertTrue(NavigatorUtils.hasSubModules(new InternalASTNode<ModuleDecl>(moduleDecl2, natureMock)));
		assertTrue(NavigatorUtils.hasSubModules(new InternalASTNode<ModuleDecl>(moduleDecl3, natureMock)));
		assertTrue(!NavigatorUtils.hasSubModules(new InternalASTNode<ModuleDecl>(moduleDecl4, natureMock)));
	}

}

