package abs.javabackend.experiments;

import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.PrintWriter;
import java.util.Collection;
import java.util.Iterator;
import java.util.LinkedList;

import AST.Access;
import AST.ArrayTypeAccess;
import AST.Block;
import AST.BodyDecl;
import AST.BytecodeParser;
import AST.ClassDecl;
import AST.CodeGeneration;
import AST.CompilationUnit;
import AST.Expr;
import AST.ExprStmt;
import AST.ImportDecl;
import AST.JavaParser;
import AST.List;
import AST.MethodAccess;
import AST.MethodDecl;
import AST.Modifier;
import AST.Modifiers;
import AST.Opt;
import AST.ParameterDeclaration;
import AST.Program;
import AST.Stmt;
import AST.StringLiteral;
import AST.TypeDecl;
import AST.VarAccess;
import beaver.Parser.Exception;

public class Experiment {
	public static void main(String[] args) throws IOException, Exception {
		Program.initOptions();
	   Program.setValueForOption("test", "-d");
	   Program.setOption("-verbose");
		
		Program p = new Program();
		Util util = new Util(p);
		BytecodeParser b = new BytecodeParser();
		p.initBytecodeReader(b);
		
		JavaParser parser = new JavaParser() {
         public CompilationUnit parse(java.io.InputStream is, String fileName) throws java.io.IOException, beaver.Parser.Exception {
           parser.JavaParser parser2 = new parser.JavaParser();
           return parser2.parse(is, fileName);
         }
       };

       p.initJavaParser(parser);
       
       String testClass = 
      	 	"package abs.javabackend.experiements.tests;" +
       		"public class TestClass {" +
       		"   public static void main(String[] args) {" +
       		"	     System.out.println(\"Hello World\");" +
       		"   }" +
       		"}"
       	;
      
       InputStream is = new ByteArrayInputStream(testClass.getBytes());
//       CompilationUnit unit2 = parser.parse(is, "TestClass.java");
		
		
	   CompilationUnit unit = new CompilationUnit("abs.javabackend.experiments.tests",
	   	new List<ImportDecl>(),
	   	new List<TypeDecl>().add(
	   		new ClassDecl(
	   			util.publicModifier(),
	   				"TestClass",
	   				new Opt<Access>(p.typeObject().createQualifiedAccess()),
	   				new List<Access>(),
	   				new List<BodyDecl>().add(
	   					new MethodDecl(
	   						util.modifiers("public", "static"),
	   						p.typeVoid().createQualifiedAccess(),
	   						"main",
	   						new List<ParameterDeclaration>().add(
	   							new ParameterDeclaration(
	   								new ArrayTypeAccess(p.typeString().createQualifiedAccess()),
	   									"args")),
	   								new List<Access>(),
	   						new Opt<Block>(
	   							new Block(
	   								 new List<Stmt>().add(
	   									 new ExprStmt(
	   									     util.accessSystem().qualifiesAccess(
	   									 		  new VarAccess("out")).qualifiesAccess(
	   													new MethodAccess("println", 
	   												  	   new List<Expr>().add(
	   														   new StringLiteral("Hello World")
	   															   ))))))))))));
	   
	   p.addCompilationUnit(unit);

	   unit.setFromSource(true);
	   unit.transformation();
	   unit.generateClassfile();
	   
   }
}

class Util {
	final Program p;
	
	Util(Program prog) {
		this.p = prog;
	}
	
	Access accessSystem() {
		return p.lookupType("java.lang", "System").createQualifiedAccess();
	}
	
	Modifiers publicModifier() {
		return new Modifiers(
				new List<Modifier>().add(
						new Modifier("public")));
	}
	
	Modifiers modifiers(String... args) {
		List<Modifier> mods = new List<Modifier>();
		for (String s : args) {
			mods.add(new Modifier(s));
		}
		return new Modifiers(mods);
	}
}