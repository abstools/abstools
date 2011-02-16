package eu.hatsproject.absplugin.wizards;

import static org.junit.Assert.*;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import org.eclipse.core.resources.IProject;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

import abs.frontend.ast.ClassDecl;
import abs.frontend.ast.Decl;
import abs.frontend.ast.InterfaceDecl;
import abs.frontend.ast.List;
import abs.frontend.ast.ModuleDecl;
import eu.hatsproject.absplugin.builder.AbsNature;
import eu.hatsproject.absplugin.util.InternalASTNode;
import eu.hatsproject.absplugin.wizards.WizardUtil;
import eu.hatsproject.absplugin.wizards.WizardUtil.ErrorType;

public class WizardUtilsTests {

	@Before
	public void setUp() throws Exception {
	}

	@After
	public void tearDown() throws Exception {
	}

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
		
		assertTrue(WizardUtil.validateModule(valid1).equals(ErrorType.NO_ERROR));
		assertTrue(WizardUtil.validateModule(valid2).equals(ErrorType.NO_ERROR));
		assertTrue(WizardUtil.validateModule(valid3).equals(ErrorType.NO_ERROR));
		assertTrue(WizardUtil.validateModule(invalid1).equals(ErrorType.ERROR_NO_NAME));
		assertTrue(WizardUtil.validateModule(invalid2).equals(ErrorType.ERROR_NO_UPPER_CASE));
		assertTrue(WizardUtil.validateModule(invalid3).equals(ErrorType.ERROR_NO_UPPER_CASE));
		assertTrue(WizardUtil.validateModule(invalid4).equals(ErrorType.ERROR_INVALID_NAME));
		assertTrue(WizardUtil.validateModule(invalid5).equals(ErrorType.ERROR_INVALID_NAME));
		assertTrue(WizardUtil.validateModule(invalid6).equals(ErrorType.ERROR_INVALID_NAME));
		assertTrue(WizardUtil.validateModule(invalid7).equals(ErrorType.ERROR_KEYWORD));
		
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
		
		assertTrue(WizardUtil.validate(valid1).equals(ErrorType.NO_ERROR));
		assertTrue(WizardUtil.validate(valid2).equals(ErrorType.NO_ERROR));
		assertTrue(WizardUtil.validate(valid3).equals(ErrorType.NO_ERROR));
		assertTrue(WizardUtil.validate(invalid1).equals(ErrorType.ERROR_NO_NAME));
		assertTrue(WizardUtil.validate(invalid2).equals(ErrorType.ERROR_NO_UPPER_CASE));
		assertTrue(WizardUtil.validate(invalid3).equals(ErrorType.ERROR_NO_UPPER_CASE));
		assertTrue(WizardUtil.validate(invalid4).equals(ErrorType.ERROR_INVALID_NAME));
		assertTrue(WizardUtil.validate(invalid5).equals(ErrorType.ERROR_KEYWORD));
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
		
		assertTrue(WizardUtil.validateClass(valid1,mockDecl).equals(ErrorType.NO_ERROR));
		assertTrue(WizardUtil.validateClass(valid2,mockDecl).equals(ErrorType.NO_ERROR));
		assertTrue(WizardUtil.validateClass(valid3,mockDecl).equals(ErrorType.NO_ERROR));
		assertTrue(WizardUtil.validateClass(invalid1,mockDecl).equals(ErrorType.ERROR_NO_NAME));
		assertTrue(WizardUtil.validateClass(invalid2,mockDecl).equals(ErrorType.ERROR_NO_UPPER_CASE));
		assertTrue(WizardUtil.validateClass(invalid3,mockDecl).equals(ErrorType.ERROR_NO_UPPER_CASE));
		assertTrue(WizardUtil.validateClass(invalid4,mockDecl).equals(ErrorType.ERROR_INVALID_NAME));
		assertTrue(WizardUtil.validateClass(invalid5,mockDecl).equals(ErrorType.ERROR_KEYWORD));
		assertTrue(WizardUtil.validateClass(invalid6,mockDecl).equals(ErrorType.ERROR_DUPLICATE_NAME));
		assertTrue(WizardUtil.validateClass(invalid7,mockDecl).equals(ErrorType.ERROR_DUPLICATE_NAME));		
		
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
		
		assertTrue(WizardUtil.validateInterface(valid1,mockDecl).equals(ErrorType.NO_ERROR));
		assertTrue(WizardUtil.validateInterface(valid2,mockDecl).equals(ErrorType.NO_ERROR));
		assertTrue(WizardUtil.validateInterface(valid3,mockDecl).equals(ErrorType.NO_ERROR));
		assertTrue(WizardUtil.validateInterface(invalid1,mockDecl).equals(ErrorType.ERROR_NO_NAME));
		assertTrue(WizardUtil.validateInterface(invalid2,mockDecl).equals(ErrorType.ERROR_NO_UPPER_CASE));
		assertTrue(WizardUtil.validateInterface(invalid3,mockDecl).equals(ErrorType.ERROR_NO_UPPER_CASE));
		assertTrue(WizardUtil.validateInterface(invalid4,mockDecl).equals(ErrorType.ERROR_INVALID_NAME));
		assertTrue(WizardUtil.validateInterface(invalid5,mockDecl).equals(ErrorType.ERROR_KEYWORD));
		assertTrue(WizardUtil.validateInterface(invalid6,mockDecl).equals(ErrorType.ERROR_DUPLICATE_NAME));
		assertTrue(WizardUtil.validateInterface(invalid7,mockDecl).equals(ErrorType.ERROR_DUPLICATE_NAME));		
		
	}

	@Test
	public void testGetProjectOfModuleDecl() {
		AbsNature natureMock = mock(AbsNature.class);
		IProject project = mock(IProject.class);
		when(project.getName()).thenReturn("");
		when(natureMock.getProject()).thenReturn(project);

		ModuleDecl moduleMock = mock(ModuleDecl.class);
		
		InternalASTNode<ModuleDecl> node = new InternalASTNode<ModuleDecl>(moduleMock,natureMock);
		
		assertSame(WizardUtil.getProjectOfModuleDecl(node),project);
		assertSame(WizardUtil.getProjectOfModuleDecl(null),null);
		
	
	}

}
