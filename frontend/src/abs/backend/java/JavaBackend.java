package abs.backend.java;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.PrintStream;
import java.io.PrintWriter;
import java.util.HashMap;
import java.util.Map;

import org.junit.Test;

import junit.framework.Assert;

import AST.BytecodeParser;
import AST.CompilationUnit;
import AST.JavaParser;
import AST.Program;
import abs.backend.java.lib.ABSBool;
import abs.backend.java.lib.ABSInteger;
import abs.backend.java.lib.ABSString;
import abs.frontend.ast.DataTypeUse;
import abs.frontend.ast.Model;
import abs.frontend.ast.PureExp;
import abs.frontend.parser.Main;

public class JavaBackend {
    private static String testCode() {
        return "data Bool { }\n data Int { }\n" +
               "interface A { }\n" +
               "class B implements A { } \n" +
               "data Void { }\n" +
               "def A f() = null\n" +
               "{ " +
               "  A a; Int i; Bool b; a = null; " +
               "  i = 5;" +
               "  i = i + 4;" +
               "  b = True; "+
               "}";
    }
    
    
    public static void main(String[] args) throws Exception {
        testCompile(testCode());
    }

    public static void testCompile(String javaCode) throws Exception {
        InputStream in = new ByteArrayInputStream(javaCode.getBytes());
        Model model = Main.parse(in);
        ByteArrayOutputStream out = new ByteArrayOutputStream();
        model.generateJava(new PrintStream(out));
        String code = out.toString();
        System.out.println(code);
        File tmpFile = getTempFile(code);
        JavaCompiler.compile("-classpath","bin","-verbose", "-d", "gen/test", tmpFile.getAbsolutePath());
    }

    public static void test() throws Exception {
        final parser.JavaParser javaParser = new parser.JavaParser();
        Program.initOptions();
        Program.setValueForOption("test", "-d");
        //Program.setOption("-verbose");
         
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
    
    private static final Map<String, String> dataTypeMap = initDataTypeMap();
    
    private static Map<String, String> initDataTypeMap() {
        final Map<String, String> res = new HashMap<String,String>();
        res.put("Int",ABSInteger.class.getName());
        res.put("Bool",ABSBool.class.getName());
        res.put("String",ABSString.class.getName());
        return res;
    }


    public static String getJavaType(DataTypeUse absType) {
        String res = dataTypeMap.get(absType.getName());
        if (res != null)
            return res;
        return absType.getName();
    }
    
}
