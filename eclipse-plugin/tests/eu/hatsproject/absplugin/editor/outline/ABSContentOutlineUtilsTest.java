package eu.hatsproject.absplugin.editor.outline;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.util.ArrayList;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.mockito.Mockito;

import abs.frontend.ast.*;
import eu.hatsproject.absplugin.builder.AbsNature;
import eu.hatsproject.absplugin.editor.outline.ABSContentOutlineUtils;
import eu.hatsproject.absplugin.navigator.ModulePath;

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
		
		List<DataTypeUse> dtuL = new List<DataTypeUse>();
		DataTypeUse dtu = mock(DataTypeUse.class);
		when(dtu.getName()).thenReturn("Parametric");
		dtuL.add(dtu);
		when(pdtu.getParamListNoTransform()).thenReturn(dtuL);
	}

	@After
	public void tearDown() throws Exception {
	}

	@Test
	public void testIsImportExportList() {
		List<Import> list = new List<Import>();
		Import imp = mock(Import.class);
		list.add(imp);
		
		assertTrue(ABSContentOutlineUtils.isImportList(list));
		assertTrue(!ABSContentOutlineUtils.isExportList(list));
		
		List<Export> list2 = new List<Export>();
		Export exp = mock(Export.class);
		list2.add(exp);		
		
		assertTrue(!ABSContentOutlineUtils.isImportList(list2));			
		assertTrue(ABSContentOutlineUtils.isExportList(list2));	
	}
	
	@Test
	public void testformatMethodSig(){
		MethodSig sigMock = mock(MethodSig.class);
		when(sigMock.getName()).thenReturn("methodName");
		
		List<ParamDecl> paramDeclL = new List<ParamDecl>();
		ParamDecl pd1 = mock(ParamDecl.class);
		ParamDecl pd2 = mock(ParamDecl.class);
			
		when(pd1.getAccessNoTransform()).thenReturn(tu);
		when(pd2.getAccessNoTransform()).thenReturn(pdtu);
		
		paramDeclL.add(pd1);
		paramDeclL.add(pd2);
		
		when(sigMock.getParamListNoTransform()).thenReturn(paramDeclL);
		
		assertEquals(ABSContentOutlineUtils.formatMethodSig(sigMock).toString(),"methodName(Int, Int<Parametric>)");
		
		MethodSig sigMock2 = mock(MethodSig.class);
		when(sigMock2.getName()).thenReturn("methodName");
		
		List<ParamDecl> paramDeclL2 = new List<ParamDecl>();
		
		when(sigMock.getParamListNoTransform()).thenReturn(paramDeclL2);
		
		assertEquals(ABSContentOutlineUtils.formatMethodSig(sigMock).toString(),"methodName()");		
		
	}
	
	@Test
	public void testFormatTypedVarOrFieldDecl(){
		
		TypedVarOrFieldDecl tvofd1 = mock(TypedVarOrFieldDecl.class);
		when(tvofd1.getName()).thenReturn("Field1");
		when(tvofd1.getAccessNoTransform()).thenReturn(tu);
		
		TypedVarOrFieldDecl tvofd2 = mock(TypedVarOrFieldDecl.class);
		when(tvofd2.getName()).thenReturn("Field2");
		when(tvofd2.getAccessNoTransform()).thenReturn(pdtu);		
		
		assertEquals(ABSContentOutlineUtils.formatTypedVarOrFieldDecl(tvofd1).toString(),"Field1 : Int");
		assertEquals(ABSContentOutlineUtils.formatTypedVarOrFieldDecl(tvofd2).toString(),"Field2 : Int<Parametric>");
		assertTrue(ABSContentOutlineUtils.formatTypedVarOrFieldDecl(null) == null);
	}
	
	@Test
	public void testGetChildrenOf(){			
		ArrayList<ASTNode<?>> chld = ABSContentOutlineUtils.getChildrenOf(moduleMock);
		
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
		
		assertEquals(ABSContentOutlineUtils.getLabel(moduleMock0).toString(),"B");
		assertEquals(ABSContentOutlineUtils.getLabel(moduleMock1).toString(),"A");
		assertEquals(ABSContentOutlineUtils.getLabel(moduleMock2).toString(),"A");
		assertEquals(ABSContentOutlineUtils.getLabel(moduleMock3).toString(),"");
	}

}
