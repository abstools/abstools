/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.absmodels.abs.plugin.navigator;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;

import org.absmodels.abs.plugin.builder.AbsNature;
import org.absmodels.abs.plugin.navigator.ModulePath;
import org.absmodels.abs.plugin.util.InternalASTNode;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import abs.frontend.ast.ClassDecl;
import abs.frontend.ast.Decl;
import abs.frontend.ast.MainBlock;
import abs.frontend.ast.Model;
import abs.frontend.ast.ModuleDecl;

/**
 * @author Christian
 *
 */
public class ModulePathTest {
 		
	@Mock
	private static Model model;
	
	@Mock
	private static ModuleDecl modDeclA, modDeclAAA, modDeclAAB, modDeclAAC, modDeclAAD, modDeclAAE;	
	
	private static ModuleDecl[] declArray;
	
	private static Collection<ModuleDecl> decls = new ArrayList<ModuleDecl>();
	
	private static AbsNature natureMock;
	
	private static ModulePath modPathEmpty, modPathA, modPathAA, modPathAAA, modPathAAB, modPathAAC, modPathAAD, modPathAAE;
	
	/**
	 * @throws java.lang.Exception
	 */
	@Before
	public void setUp() throws Exception {

		//Set up ABS nature
		natureMock = mock(AbsNature.class,Mockito.RETURNS_DEEP_STUBS);
		MainBlock main = mock(MainBlock.class,Mockito.RETURNS_DEEP_STUBS);
		natureMock.modelLock = new Object();
		
		//Set up mocks with @Mock annotation
		MockitoAnnotations.initMocks(this);
		
		declArray = new ModuleDecl[]{modDeclA, modDeclAAA, modDeclAAB, modDeclAAC, modDeclAAD,modDeclAAE};
	
		//Set up module paths
		modPathEmpty = new ModulePath(natureMock, "");      
		modPathA = new ModulePath(natureMock, "A");     
		modPathAA = new ModulePath(natureMock, "A.A");   
		modPathAAA = new ModulePath(natureMock, "A.A.A"); 
		modPathAAB = new ModulePath(natureMock, "A.A.B"); 
		modPathAAC = new ModulePath(natureMock, "A.A.C"); 
		modPathAAD = new ModulePath(natureMock, "A.A.D"); 
		modPathAAE = new ModulePath(natureMock, "A.A.E"); 
		
		
		//Return "fake" module names.
		when(modDeclA.getName()).thenReturn("A");				
		when(modDeclAAA.getName()).thenReturn("A.A.A");		
		when(modDeclAAB.getName()).thenReturn("A.A.B");		
		when(modDeclAAC.getName()).thenReturn("A.A.C");
		when(modDeclAAD.getName()).thenReturn("A.A.D");
		when(modDeclAAE.getName()).thenReturn("A.A.E");
		
		//Setting up child nodes of ModuleDecls
		//moduleDeclA and moduleDeclAAA are empty
		
		//moduleDeclAAB has a main block
		when(declArray[2].hasBlock()).thenReturn(true);
		when(declArray[2].getBlock()).thenReturn(main);
		
		//moduleDeclAAC has a decl (class declaration)
		when(declArray[3].getNumDecl()).thenReturn(1);	
		abs.frontend.ast.List<Decl> declList = new abs.frontend.ast.List<Decl>();
		declList.add((Decl)mock(ClassDecl.class));	
		when(declArray[3].getDeclList()).thenReturn(declList);	
		
		//moduleDeclAAD has an Export
		when(declArray[4].getNumExport()).thenReturn(1);
		
		//moduleDeclAAD has an Import	
		when(declArray[5].getNumImport()).thenReturn(2);		
		
		
		decls = Arrays.asList(declArray);	
		
		when(model.getModuleDecls()).thenReturn(decls);
		when(natureMock.getCompleteModel()).thenReturn(model);
	}

	/**
	 * @throws java.lang.Exception
	 */
	@After
	public void tearDown() throws Exception {
	}

	/**
	 * Test method for {@link org.absmodels.abs.plugin.navigator.ModulePath#getChildModulePathsAndModuleDecls()}.
	 */
	@Test
	public void testGetChildModulePathsAndModuleDecls() {
		ModulePath mPath;
		Object object;
		
		List<Object> paths = modPathEmpty.getChildModulePathsAndModuleDecls();
		assertTrue(paths.size() == 1);
		
		object = paths.get(0);
		assertTrue(object instanceof ModulePath);
		mPath = (ModulePath) object;
		assertEquals(mPath.getModulePath(),"A");
		assertNotNull(mPath.getModuleDecl());
		assertEquals(mPath.getModuleDecl().getASTNode(),modDeclA);
		assertSame(mPath.getModuleDecl().getNature(),modPathEmpty.getNature());
		
		paths = mPath.getChildModulePathsAndModuleDecls();
		assertTrue(paths.size() == 1);
		object = paths.get(0);
		assertTrue(object instanceof ModulePath);
		mPath = (ModulePath) paths.get(0);
		assertEquals(mPath.getModulePath(),"A.A");
		assertSame(mPath.getModuleDecl(), null);
		
		paths = mPath.getChildModulePathsAndModuleDecls();
		assertTrue(paths.size() == 5);
		validateLeafModuleDecl(paths.get(0), "A.A.A", modDeclAAA);
		validateLeafModuleDecl(paths.get(1), "A.A.B", modDeclAAB);
		validateLeafModuleDecl(paths.get(2), "A.A.C", modDeclAAC);
		validateLeafModuleDecl(paths.get(3), "A.A.D", modDeclAAD);
		validateLeafModuleDecl(paths.get(4), "A.A.E", modDeclAAE);

		
		
	}
	
	private final void validateLeafModuleDecl(Object object, String name, ModuleDecl originalDecl){
		InternalASTNode<?> intNode;
		assertTrue(object instanceof InternalASTNode<?>);	
		intNode = (InternalASTNode<?>) object;
		ModuleDecl modDecl = (ModuleDecl) intNode.getASTNode();
		assertEquals(modDecl,originalDecl);
		assertEquals(modDecl.getName(),name);		
	}

	/**
	 * Test method for {@link org.absmodels.abs.plugin.navigator.ModulePath#getRootHierarchy(org.absmodels.abs.plugin.builder.AbsNature)}.
	 */
	@Test
	public void testGetRootHierarchy() {
		List<Object> rootHierarchy = ModulePath.getRootHierarchy(natureMock);
		assertTrue(rootHierarchy.size() == 1);
		
		try {
			ModulePath.getRootHierarchy(null);
			fail("Illegal Argument Exception should be thrown");
		}catch(IllegalArgumentException ia){
			
		}
		
	}

	/**
	 * Test method for {@link org.absmodels.abs.plugin.navigator.ModulePath#clearRootHierarchyCache()}.
	 */
	@Test
	public void testClearRootHierarchyCache() {
		//Get the cached root hierarchy of our nature mock object
		List<Object> rootHierarchy = ModulePath.getRootHierarchy(natureMock);
		//The root hierarchy should only contain one element, namely the ModulePath A
		assertTrue(rootHierarchy.size() == 1);
		ModulePath originalObject = (ModulePath) rootHierarchy.get(0);	

		
		ModulePath.clearRootHierarchyCache();
		
		//Get the new root hierarchy of our nature mock object
		List<Object> newRootHierarchy = ModulePath.getRootHierarchy(natureMock);
		//and check the size again
		assertTrue(rootHierarchy.size() == 1);
		ModulePath newObject = (ModulePath) newRootHierarchy.get(0);
		
		//As the root hierarchy cache was cleared, the elements should not be the same...
		assertTrue(newObject != originalObject);
		//...but contain the same information
		assertEquals(newObject.getModulePath(),originalObject.getModulePath());
		assertEquals(newObject.getModuleDecl(),originalObject.getModuleDecl());
	}

	/**
	 * Test method for {@link org.absmodels.abs.plugin.navigator.ModulePath#getModulesForPrefix()}.
	 */
	@Test
	public void testGetModulesForPrefix() {
		ModuleDecl[] decls = new ModuleDecl[]{modDeclAAA,modDeclAAB,modDeclAAC,modDeclAAD,modDeclAAE};
		
		//Test empty module paths
		assertTrue(!modPathEmpty.getModulesForPrefix().iterator().hasNext());
		assertTrue(!modPathEmpty.getModulesForPrefix().iterator().hasNext());
		
		// Test non-empty module paths
		assertTrue(modPathAA.getModulesForPrefix().containsAll(InternalASTNode.wrapASTNodes(decls, natureMock)));
		
		//Test end of hierarchy
		assertTrue(!modPathAAA.getModulesForPrefix().iterator().hasNext());
		assertTrue(!modPathAAB.getModulesForPrefix().iterator().hasNext());
		assertTrue(!modPathAAC.getModulesForPrefix().iterator().hasNext());
		assertTrue(!modPathAAD.getModulesForPrefix().iterator().hasNext());
		assertTrue(!modPathAAE.getModulesForPrefix().iterator().hasNext());
	}

	/**
	 * Test method for {@link org.absmodels.abs.plugin.navigator.ModulePath#hasModuleWithDecls()}.
	 */
	@Test
	public void testHasModuleWithDecls() {
		assertTrue(!modPathEmpty.hasModuleWithDecls());
		assertTrue(!modPathA.hasModuleWithDecls());
		assertTrue(!modPathAA.hasModuleWithDecls());
		assertTrue(!modPathAAA.hasModuleWithDecls());
		assertTrue(modPathAAB.hasModuleWithDecls());
		assertTrue(modPathAAC.hasModuleWithDecls());
		assertTrue(modPathAAD.hasModuleWithDecls());
		assertTrue(modPathAAE.hasModuleWithDecls());
	}

	/**
	 * Test method for {@link org.absmodels.abs.plugin.navigator.ModulePath#hasModule()}.
	 */
	@Test
	public void testHasModule() {	
		assertTrue(!modPathEmpty.hasModule());
		assertTrue(modPathA.hasModule());
		assertTrue(!modPathAA.hasModule());
		assertTrue(modPathAAA.hasModule());
		assertTrue(modPathAAB.hasModule());
		assertTrue(modPathAAC.hasModule());
	}

	/**
	 * Test method for {@link org.absmodels.abs.plugin.navigator.ModulePath#getModuleDecl()}.
	 */
	@Test
	public void testGetModuleDecl() {
		assertSame(modPathEmpty.getModuleDecl(),null);
		assertNotNull(modPathAAA.getModuleDecl());
		assertNotNull(modPathAAB.getModuleDecl());
		assertNotNull(modPathAAC.getModuleDecl());
		assertNotNull(modPathAAD.getModuleDecl());
		assertNotNull(modPathAAE.getModuleDecl());
	}

}
