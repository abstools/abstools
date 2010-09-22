package abs.backend.java;

import java.io.BufferedReader;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.PrintStream;
import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.Arrays;

import junit.framework.Assert;

import abs.ABSTest;
import abs.frontend.analyser.SemanticErrorList;
import abs.frontend.ast.Model;
import abs.frontend.parser.Main;

public class JavaBackendTest extends ABSTest {
    
    private static final boolean DEBUG = false;

    void assertEqual(String absCode, String javaCode) {
        assertEqual(absCode, javaCode,null);
    }
    
    void assertValidStdLib(String absCode) {
        assertValidJava(getJavaCode("module JavaUnitTest; "+absCode, true));
    }
    
    void assertValid(String absCode) {
        assertValidJava(getJavaCode("module JavaUnitTest; "+absCode, false));
    }
    
    protected void assertValidJavaFile(String absFile, boolean useStdLib) {
       Model m = assertParseFileOk(absFile, true, true);
       String javaCode = getJavaCode(m);
       assertValidJava(javaCode);
    }
    
    void assertValidJava(String javaCode) {
        File tmpFile;
        try {
            tmpFile = getTempFile(javaCode);
            JavaCompiler.compile("-classpath","bin", "-d", "gen/test", tmpFile.getAbsolutePath());
        } catch (Exception e) {
           System.out.println(javaCode);
            Assert.fail(e.getMessage());
        }
    }
    
    static class NoTestResultFoundException extends RuntimeException { NoTestResultFoundException() { super("No test result was found!"); }}
    
    
    
    StringBuffer runJava(String javaCode, String... jvmargs) {
        String realCode = "package unittest;"+javaCode;
        File tmpFile;
        StringBuffer output = new StringBuffer();

        try {
            tmpFile = getTempFile(realCode);
            JavaCompiler.compile("-classpath","bin", "-d", "gen/test", tmpFile.getAbsolutePath());
            ArrayList<String> args = new ArrayList<String>();
            args.add("java");
            args.addAll(Arrays.asList(jvmargs));
            args.addAll(Arrays.asList("-cp", "bin:gen/test", "unittest.Main"));
            ProcessBuilder pb = new ProcessBuilder(args.toArray(new String[0]));
            pb.redirectErrorStream(true);
            Process p = pb.start();
            BufferedReader r = new BufferedReader(new InputStreamReader(p.getInputStream()));
            while (true) {
                String s;
                s = r.readLine();
                if (s == null)
                    break;
                output.append(s+"\n");
            }
            return output;
        } catch (IOException e) {
            e.printStackTrace();
            return null;
        }

    }
    
    boolean runJavaAndTestResult(String javaCode, boolean expectFail) {
        StringBuffer output = null; 
        try {
            output = runJava(javaCode);
            String s = output.toString().trim();
            String result = null;
            if (s.startsWith("__ABS_TESTRESULT=")) {
                result = s.split("=")[1];
            }
            
            if (result == null)
                throw new NoTestResultFoundException();
            
            return Boolean.valueOf(result);
        } catch (NoTestResultFoundException e) {
            if (expectFail) {
                throw e;
            } else {
                System.out.println(output.toString());
                System.out.println(javaCode);
                Assert.fail(e.getMessage());
                return false;
            }
        } catch (Exception e) {
            System.out.println(output.toString());
           System.out.println(javaCode);
            Assert.fail(e.getMessage());
            return false;
        }
    }
    
    String getJavaCode(String absCode, boolean withStdLib) {
        try {
        Model model = null;
        String code = null;
        try {
            code = absCode;
//            if (withStdLib) 
//                code = "data Unit = Unit; data Bool = True | False; data Int; data String; data Fut<A>; " + code; 
            model = Main.parseString(code, withStdLib);
            if (model.hasErrors()) {
                Assert.fail(model.getErrors().get(0).getMsgString());
            } else {
                SemanticErrorList el = model.typeCheck();
                if (!el.isEmpty()) {
                    Assert.fail(el.get(0).getMsg());
                }
            }
            
        } catch (Exception e) {
            e.printStackTrace();
            Assert.fail(e.getClass().getSimpleName()+":"+e.getMessage());
            return null;
        }
        
        if (model.hasErrors()) {
            Assert.fail(model.getErrors().getFirst().getMsgString());
            return null;
        }
        return getJavaCode(model);
        } catch (NumberFormatException e) {
            Assert.fail(e.getMessage());
            return null;
        }
    }

    private String getJavaCode(Model model) {
        ByteArrayOutputStream out = new ByteArrayOutputStream();
        model.generateJava(new PrintStream(out));
        String res = out.toString();
        //res = res.replace('\n', ' ');
        //res = res.replaceAll("[ ]+", " ");
        res = res.trim();
        return res;
    }
    
    void assertEqual(String absCode, String javaCode, String pkg) {
        try {
            StringBuffer expectedJavaCode = new StringBuffer();
            if (pkg != null) {
                expectedJavaCode.append("package "+pkg+"; ");
            }
            
            expectedJavaCode.append(JavaBackendConstants.LIB_IMPORT_STATEMENT+" ");
            expectedJavaCode.append(javaCode);
            String generatedJavaCode = getJavaCode(absCode, false);
            Assert.assertEquals(expectedJavaCode.toString(), generatedJavaCode);
            
            assertValidJava(generatedJavaCode);
            
        } catch (Exception e) {
            Assert.fail(e.getMessage());
        }
    }

    private static File getTempFile(String testCode) throws IOException {
        File tmpFile = File.createTempFile("abs", "test");
        PrintWriter p = new PrintWriter(new FileOutputStream(tmpFile));
        p.print(testCode);
        p.close();
        tmpFile.deleteOnExit();
        
        return tmpFile;
    }
    
    void assertEvalTrue(String absCode) {
        assertEvalEquals(absCode, true);
    }

    public void assertEvalEquals(String absCode, boolean value) {
        String javaCode = getJavaCode(absCode, true);
        if (DEBUG)
            System.out.println(javaCode);
        boolean res = runJavaAndTestResult(javaCode, false);
        if (value != res)
            System.out.println(javaCode);
        Assert.assertEquals(value,res);
    }

    public void assertEvalFails(String absCode) {
        String javaCode = getJavaCode(absCode, true);
        try {
            runJavaAndTestResult(javaCode, true);
            System.out.println(javaCode);
            Assert.fail("Expected that Java run failed, but did not.");
        } catch (NoTestResultFoundException e) {
            // OK
        } 
        
    }
    
}
