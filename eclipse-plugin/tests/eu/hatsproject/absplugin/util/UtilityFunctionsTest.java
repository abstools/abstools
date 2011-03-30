/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package eu.hatsproject.absplugin.util;


import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;
import static org.mockito.Matchers.any;
import static org.mockito.Matchers.anyInt;
import static org.mockito.Mockito.atLeastOnce;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.util.Iterator;
import java.util.Random;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IStatus;
import org.junit.Before;
import org.junit.Test;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.invocation.InvocationOnMock;
import org.mockito.stubbing.Answer;

import beaver.Symbol;

import abs.frontend.ast.*;
import eu.hatsproject.absplugin.builder.AbsNature;
import eu.hatsproject.absplugin.editor.outline.ABSContentOutlineConstants.AnnotationType;
import eu.hatsproject.absplugin.util.Constants;
import eu.hatsproject.absplugin.util.InternalASTNode;
import eu.hatsproject.absplugin.util.UtilityFunctions;
import eu.hatsproject.absplugin.util.UtilityFunctions.EditorPosition;

/**
 * @author Christian
 *
 */
public class UtilityFunctionsTest {

	@Mock
	public ModuleDecl modDecl;
	
	private AbsNature natureMock;
	/**
	 * @throws java.lang.Exception
	 */
	@Before
	public void setUp() throws Exception {
		natureMock = mock(AbsNature.class,Mockito.RETURNS_DEEP_STUBS);
		natureMock.modelLock = new Object();
		
	}
	
	@Test
	public void testCopyFile() throws Exception{
		File file1 = mock(File.class);
		File file2 = mock(File.class);
		
		FileInputStream fis1 = mock(FileInputStream.class);
		FileOutputStream fis2 = mock(FileOutputStream.class);
		
		Answer<Integer> answer = new Answer<Integer>(){
			public boolean first = true;
			
			@Override
			public Integer answer(InvocationOnMock invocation) throws Throwable {
				if (first){
					byte[] arr = (byte[])invocation.getArguments()[0];					
					arr[0] = 0x11;
					arr[1] = 0x22;
					arr[2] = 0x33;
					first = !first;
					return 3;
				}else{
					first = !first;
					return -1;
				}
				
			}
			
		};
		
		when(fis1.read(any(byte[].class))).thenAnswer(answer);		
		
		try {
			UtilityFunctions.copyFile(null, file2);
			UtilityFunctions.copyFile(file1,null);
			UtilityFunctions.copyFile((File) null, null);
			fail();
		}catch(NullPointerException npe){
			//right behaviour
		}catch(IOException io){
			
		}
		
		UtilityFunctions.copyFile(fis1, fis2);
		verify(fis2, atLeastOnce()).write(any(byte[].class), anyInt(), anyInt());
		
		Mockito.doThrow(new IOException()).when(fis2).write(any(byte[].class), anyInt(), anyInt());


		try{
			UtilityFunctions.copyFile(fis1, fis2);
			fail();
		}catch(IOException io){
			//right behaviour...
		}
		
		
		
	}
	
	@Test
	public void testGetMaudeCommand(){
		//pass
		assertEquals(UtilityFunctions.getMaudeCommand(0), Constants.MAUDE_COMMAND1 + '[' + 0 + ']' + Constants.MAUDE_COMMAND2 + Constants.MAUDE_COMMAND_QUIT);
		String maudeCommand = UtilityFunctions.getMaudeCommand(5);
		String maudeCommand2 = Constants.MAUDE_COMMAND1 + '[' + 5 + ']' + Constants.MAUDE_COMMAND2 + Constants.MAUDE_COMMAND_QUIT;
		
		assertEquals(maudeCommand, maudeCommand2);
		
		//fail
		if(UtilityFunctions.getMaudeCommand(-5).equals(Constants.MAUDE_COMMAND1 + '[' + -5 + ']' + Constants.MAUDE_COMMAND2 + Constants.MAUDE_COMMAND_QUIT)){
			fail ("Negative number of steps mak no sense");
		}
	}
	
	@Test
	public void testGetPathOfModuleDecl() throws Exception{
		ModuleDecl md = mock(ModuleDecl.class);
		CompilationUnit cd = mock(CompilationUnit.class);
		AbsNature nature = mock(AbsNature.class);
		InternalASTNode<ModuleDecl> internalASTNode = new InternalASTNode<ModuleDecl>(md,nature);
		
		when(cd.getFileName()).thenReturn("C:/test.abs");
		when(md.getCompilationUnit()).thenReturn(cd);
		
		IPath pathOfModuleDecl = UtilityFunctions.getPathOfModuleDecl(internalASTNode);
		
		assertEquals(pathOfModuleDecl.toString(),"C:/test.abs");
		
		assertSame(UtilityFunctions.getPathOfModuleDecl(null),null);
		
	}
	
	@Test
	public void testGetPosition() throws Exception{
		ModuleDecl md = mock(ModuleDecl.class);
		
		Random r = new Random();

		for(int i = 0; i<1000; i++){
			int a = r.nextInt(100);
			int b = r.nextInt(100);
			int c = r.nextInt(100);
			int d = r.nextInt(100);
			when(md.getStart()).thenReturn(Symbol.makePosition(a, b));
			when(md.getEnd()).thenReturn(Symbol.makePosition(c, d));
			
			EditorPosition position = UtilityFunctions.getPosition(md);
			
			assertTrue(position.getLinestart() == a-1);
			assertTrue(position.getLineend() == c-1);
			assertTrue(position.getColstart() == b-1);
			assertTrue(position.getColend() == d);
			
		}
	}
	
	@Test
	public void testHasClassAnnotation() throws Exception {

		final List<Annotation> cogAnnotationList = setUpAnnotationList(AnnotationType.COG_ANNOTATION);
		final List<Annotation> plainAnnotationList = setUpAnnotationList(AnnotationType.PLAIN_ANNOTATION);
		
		ClassDecl classMock = mock(ClassDecl.class);
		when(classMock.getAnnotationList()).thenReturn(new abs.frontend.ast.List<Annotation>());
		
		ClassDecl classMockCOG = mock(ClassDecl.class);
		when(classMockCOG.getAnnotationList()).thenReturn(cogAnnotationList);
		
		ClassDecl classMockPlain = mock(ClassDecl.class);
		when(classMockPlain.getAnnotationList()).thenReturn(plainAnnotationList);
		
		assertTrue(!UtilityFunctions.hasClassAnnotation(classMock, AnnotationType.COG_ANNOTATION));
		assertTrue(!UtilityFunctions.hasClassAnnotation(classMock, AnnotationType.PLAIN_ANNOTATION));

		assertTrue(!(UtilityFunctions.hasClassAnnotation(classMockPlain, AnnotationType.COG_ANNOTATION)));
		assertTrue((UtilityFunctions.hasClassAnnotation(classMockPlain, AnnotationType.PLAIN_ANNOTATION)));

		assertTrue((UtilityFunctions.hasClassAnnotation(classMockCOG, AnnotationType.COG_ANNOTATION)));
		assertTrue(!(UtilityFunctions.hasClassAnnotation(classMockCOG, AnnotationType.PLAIN_ANNOTATION)));
		
		
		
	}
	
	@Test
	public void testGetABSNature() throws Exception{
		IProject proj = mock(IProject.class);
		IProject proj2 = mock(IProject.class);
		IFile file = mock(IFile.class);
		when(proj.getNature(Constants.NATURE_ID)).thenReturn(natureMock).thenReturn(null).thenThrow(new CoreException(mock(IStatus.class)));
		when(proj.isAccessible()).thenReturn(true,false,true);
		
		when(proj2.getNature(Constants.NATURE_ID)).thenReturn(natureMock).thenReturn(null).thenThrow(new CoreException(mock(IStatus.class)));
		when(proj2.isAccessible()).thenReturn(true,false,true);
				
		assertSame(UtilityFunctions.getAbsNature(proj), natureMock);
		assertSame(UtilityFunctions.getAbsNature(proj), null);
		assertSame(UtilityFunctions.getAbsNature(proj), null);
		assertSame(UtilityFunctions.getAbsNature(proj), null);

		
		when(file.getProject()).thenReturn(proj2);
		
		assertSame(UtilityFunctions.getAbsNature(file), natureMock);
		assertSame(UtilityFunctions.getAbsNature(file), null);	
		assertSame(UtilityFunctions.getAbsNature(proj), null);
		assertSame(UtilityFunctions.getAbsNature(proj), null);

		
	}
	
	@Test
	public void testHasABSFileExtension() throws Exception{
		IFile absFile = mock(IFile.class);
		IFile file    = mock(IFile.class);
		
		when(file.getFileExtension()).thenReturn("java");
		when(absFile.getFileExtension()).thenReturn("abs");
		
		assertTrue(UtilityFunctions.hasABSFileExtension(absFile));
		assertTrue(!UtilityFunctions.hasABSFileExtension(file));
		assertTrue(!UtilityFunctions.hasABSFileExtension(null));
	}
	
	@Test
	public void testGetSuperOfASTNode() throws Exception{
		
		ClassDecl cd = mock(ClassDecl.class);
		ModuleDecl md = mock(ModuleDecl.class);
		CompilationUnit cu = mock(CompilationUnit.class);
		
		when(cd.getParent()).thenReturn(md);
		when(md.getParent()).thenReturn(cu);
		when(cu.getParent()).thenReturn(null);
		
		assertSame(UtilityFunctions.getSuperOfASTNode(cd, ClassDecl.class), cd);
		assertSame(UtilityFunctions.getSuperOfASTNode(cd, ModuleDecl.class), md);
		assertSame(UtilityFunctions.getSuperOfASTNode(cd, CompilationUnit.class), cu);
		assertSame(UtilityFunctions.getSuperOfASTNode(cd, TypedVarOrFieldDecl.class), null);
		
		assertSame(UtilityFunctions.getSuperOfASTNode(md, ModuleDecl.class), md);
		assertSame(UtilityFunctions.getSuperOfASTNode(md, CompilationUnit.class), cu);
		assertSame(UtilityFunctions.getSuperOfASTNode(md, TypedVarOrFieldDecl.class), null);
		

		assertSame(UtilityFunctions.getSuperOfASTNode(cu, CompilationUnit.class), cu);
		assertSame(UtilityFunctions.getSuperOfASTNode(cu, TypedVarOrFieldDecl.class), null);
		
		assertSame(UtilityFunctions.getSuperOfASTNode(null, CompilationUnit.class),null);
		
	}
	
	@Test
	public void testGetCompilationUnitofASTNode() throws Exception{
		ClassDecl cd = mock(ClassDecl.class);
		ClassDecl cdwocomp = mock(ClassDecl.class);
		ModuleDecl md = mock(ModuleDecl.class);
		CompilationUnit cu = mock(CompilationUnit.class);
		
		when(cd.getParent()).thenReturn(md);
		when(md.getParent()).thenReturn(cu);
		when(cu.getParent()).thenReturn(null);
		
		assertSame(UtilityFunctions.getCompilationUnitOfASTNode(cd),cu);
		assertSame(UtilityFunctions.getCompilationUnitOfASTNode(cdwocomp),null);
		assertSame(UtilityFunctions.getCompilationUnitOfASTNode(md),cu);
		assertSame(UtilityFunctions.getCompilationUnitOfASTNode(cu),cu);
		assertSame(UtilityFunctions.getCompilationUnitOfASTNode(null),null);	
	}
	
	
	
	
	@SuppressWarnings("rawtypes")
	private static List<Annotation> setUpAnnotationList(AnnotationType type){
		DataConstructorExp dConst = mock(DataConstructorExp.class);
		when(dConst.getConstructor()).thenReturn(type.getAnnotationString());
		
		final Annotation a = mock(Annotation.class);
		when(a.iterator()).thenReturn(new Iterator<ASTNode>() {
			public boolean first = true;
			
			@Override
			public void remove() {	
			}
			
			@Override
			public ASTNode next() {
				if (first){
					first = !first;
					return a;
				}
				return null;
			}
			
			@Override
			public boolean hasNext() {
				return first;
			}
		});
		
		when(a.getChild(anyInt())).thenReturn(dConst);
		when(a.getNumChild()).thenReturn(1);

		List<Annotation> annotationList = new abs.frontend.ast.List<Annotation>();
		annotationList.add(a);
		return annotationList;
	}
	
	

}
