package abs.backend.java;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.PrintStream;
import java.io.PrintWriter;

import org.junit.Test;

import junit.framework.Assert;

import AST.BytecodeParser;
import AST.CompilationUnit;
import AST.JavaParser;
import AST.Program;
import abs.frontend.ast.Model;
import abs.frontend.parser.Main;

public class JavaBackend {
    public static void main(String[] args) throws Exception {
        testCompile("interface I { }");
    }

    public static void testCompile(String javaCode) throws Exception {
        InputStream in = new ByteArrayInputStream(javaCode.getBytes());
        Model model = Main.parse(in);
        ByteArrayOutputStream out = new ByteArrayOutputStream();
        model.generateJava(new PrintStream(out));
        File tmpFile = getTempFile(out.toString());
        JavaCompiler.compile("-classpath","bin","-verbose", "-d", "gen/test", tmpFile.getAbsolutePath());
    }

    public static void test() throws Exception {
        final parser.JavaParser javaParser = new parser.JavaParser();
        Program.initOptions();
        Program.setValueForOption("test", "-d");
        Program.setOption("-verbose");
         
        Program p = new Program();
        BytecodeParser b = new BytecodeParser();
        p.initBytecodeReader(b);
         
        JavaParser parser = new JavaParser() {
             public CompilationUnit parse(java.io.InputStream is, String fileName) throws java.io.IOException, beaver.Parser.Exception {
                 return javaParser.parse(is, fileName);
             }
        };

        p.initJavaParser(parser);

        
        final Model model = 
            Main.parse(new ByteArrayInputStream(testCode().getBytes()));

        ByteArrayOutputStream os = new ByteArrayOutputStream();
        model.generateJava(new PrintStream(os));
        String s = os.toString();
        
        InputStream is = new ByteArrayInputStream(os.toByteArray());
        CompilationUnit unit = parser.parse(is, "TestClass.java");
         
        
        p.addCompilationUnit(unit);

        unit.setFromSource(true);
        unit.transformation();
        unit.generateClassfile();

    }

    private static File getTempFile(String testCode) throws IOException {
        File tmpFile = File.createTempFile("abs", "test");
        PrintWriter p = new PrintWriter(new FileOutputStream(tmpFile));
        p.print(testCode);
        p.close();
        tmpFile.deleteOnExit();
        
        return tmpFile;
    }

    private static String testCode() {
        return "interface A { }\n" +
        	   "class B implements A { } \n" +
        	   "data Void { }\n" +
        	   "def A f() = null\n";
    }
    
}
