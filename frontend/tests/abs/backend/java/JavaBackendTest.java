/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.backend.java;

import java.io.*;
import java.util.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import static org.junit.Assert.*;

import abs.ABSTest;
import abs.backend.java.codegeneration.JavaCode;
import abs.backend.java.codegeneration.JavaCodeGenerationException;
import abs.backend.java.lib.runtime.ABSException;
import abs.backend.java.lib.runtime.ABSRuntime;
import abs.backend.java.observing.COGView;
import abs.backend.java.observing.ObjectView;
import abs.backend.java.observing.SystemObserver;
import abs.backend.java.scheduling.RandomSchedulingStrategy;
import abs.frontend.analyser.SemanticConditionList;
import abs.frontend.ast.Model;

public class JavaBackendTest extends ABSTest {

    private static final boolean DEBUG = false;

    @SuppressWarnings("serial")
    final protected List<String> jvmArgs = new ArrayList<String>() {{ add("-Dabs.terminateOnException=true"); }};
    /**
     * Additional arguments when running the compiled ABS program.
     */
    final protected List<String> absArgs = new ArrayList<String>();

    public final long seed;
    public final static long seed_UNUSED = -1;

    public JavaBackendTest() { seed = seed_UNUSED; }

    public JavaBackendTest(long randomSeed) {
        jvmArgs.add("-Dabs.totalscheduler="+RandomSchedulingStrategy.class.getName());
        jvmArgs.add("-Dabs.randomseed="+randomSeed);
        assert randomSeed != seed_UNUSED : "not a valid seed value";
        seed = randomSeed;
    }

    // factory method for creating the ABSRuntime
    protected ABSRuntime makeAbsRuntime() {
        return new ABSRuntime();
    }
    
    void assertValidStdLib(String absCode) throws Exception {
        assertValidJava(getJavaCode("module JavaUnitTest; " + absCode, Config.WITH_STD_LIB, Config.WITHOUT_MODULE_NAME));
    }

    void assertValid(String absCode) throws Exception {
        assertValidJava(getJavaCode("module JavaUnitTest; " + absCode, Config.WITHOUT_MODULE_NAME));
    }

    public static void assertValidJava(JavaCode javaCode) throws Exception {
        try {
            javaCode.compile("-classpath", "bin", "-d", javaCode.getSrcDir().getAbsolutePath()+"/gen/test");
        } catch (Exception e) {
            System.out.println(javaCode);
            throw e;
        } finally {
            javaCode.deleteCode();
        }
    }
    
    /**
     * compiles and executes the given code
     * ABS assertions can be used to check the result 
     */
    protected void assertValidJavaExecution(String absFile, boolean useStdLib) throws Exception {
        FileReader fileReader = new FileReader(absFile);
        BufferedReader bufferedReader = new BufferedReader(fileReader);
        List<String> lines = new ArrayList<String>();
        String line = null;
        while ((line = bufferedReader.readLine()) != null) {
            lines.add(line);
        }
        bufferedReader.close();
        assertValidJavaExecution(useStdLib, lines.toArray(new String[lines.size()]));
    }

    void assertValidJavaExecution(boolean withStdLib, String ... codeLines) throws Exception {
        StringBuilder absCode = new StringBuilder();
        for (String line : codeLines) {
            absCode.append(line);
            absCode.append("\n");
        }
        JavaCode javaCode = getJavaCode(absCode.toString(), withStdLib ? Config.WITH_STD_LIB : null);
        try {
            String genDir = javaCode.getSrcDir().getAbsolutePath()+"/gen/test";
            javaCode.compile("-classpath", "bin", "-d", genDir);
            final ABSRuntime r = makeAbsRuntime();
            r.enableDebugging(true);
            final boolean[] finished = new boolean[] {false};
            final List<ABSException> exceptions = Collections.synchronizedList(new ArrayList<ABSException>());
            r.addSystemObserver(new SystemObserver() {
                
                @Override
                public void systemStarted() {
                }
                
                @Override
                public void systemFinished() {
                    synchronized (finished) {
                        finished[0] = true;
                        finished.notifyAll();
                    }
                }
                
                @Override
                public void systemError(ABSException e) {
                    exceptions.add(e);
                }
                
                @Override
                public void newCOGCreated(COGView cog, ObjectView initialObject) {
                }
            });
            r.start(new File(genDir), "Test.Main");
            
            while (!finished[0]) {
                synchronized (finished) {
                    finished.wait(100);
                }
            }
            r.shutdown();
            for (ABSException e : exceptions) {
                throw e;
            }
        } catch (Exception e) {
            System.out.println(javaCode);
            throw e;
        } finally {
            javaCode.deleteCode();
        }
    }

    @SuppressWarnings("serial")
    static class NoTestResultFoundException extends RuntimeException {
        NoTestResultFoundException() {
            super("No test result was found!");
        }
    }

    final static Pattern p = Pattern.compile(".*__ABS_TESTRESULT=([^\n]*)\n.*", Pattern.MULTILINE | Pattern.DOTALL);

    protected StringBuffer runJava(JavaCode javaCode, String... jvmargs) throws Exception {
        StringBuffer output = new StringBuffer();
        javaCode.compile("-classpath", "bin", "-d", javaCode.getSrcDir().getAbsolutePath()+"/gen/test");

        ArrayList<String> args = new ArrayList<String>();
        args.add("java");
        args.addAll(Arrays.asList(jvmargs));
        args.addAll(Arrays.asList("-cp", "bin" + File.pathSeparator + "lib/apfloat.jar" + File.pathSeparator + javaCode.getSrcDir().getAbsolutePath()+"/gen/test", javaCode.getFirstMainClass()));
        args.addAll(absArgs);
        ProcessBuilder pb = new ProcessBuilder(args.toArray(new String[0]));
        pb.redirectErrorStream(true);
        Process p = pb.start();
        BufferedReader r = new BufferedReader(new InputStreamReader(p.getInputStream()));
        while (true) {
            String s;
            s = r.readLine();
            if (s == null)
                break;
            output.append(s + "\n");
        }
        r.close();
        javaCode.deleteCode();
        return output;
    }

    public boolean runJavaAndTestResult(JavaCode javaCode, boolean expectFail) throws Exception {
        StringBuffer output = null;
        try {
            output = runJava(javaCode, jvmArgs.toArray(new String[0]));
            String s = output.toString() + "\n";
            String result = null;
            Matcher m = p.matcher(s);
            if (m.matches()) {
                result = m.group(1);
            }

            if (result == null)
                throw new NoTestResultFoundException();

            return Boolean.valueOf(result);
        } catch (NoTestResultFoundException e) {
            if (expectFail) {
                throw e;
            } else {
            	assert output != null; // we're sure that runJava returned.
                System.err.println(output.toString());
                //System.out.println(javaCode);
                return false;
            }
        } catch (Exception e) {
            if (output != null)
                System.err.println(output.toString());
            else
                System.err.println("NO OUTPUT");
            //System.err.println(javaCode);
            throw e;
        }
    }

    protected JavaCode getJavaCode(String absCode, Config... config) throws Exception {
        Model model = null;
        String code = null;
        code = absCode;
        // if (withStdLib)
        // code =
        // "data Unit = Unit; data Bool = True | False; data Int; data String; data Fut<A>; "
        // + code;
        final int len = config.length;
        Config[] c2 = new Config[len+2];
        for (int i =0; i<len; i++) {
            c2[i] = config[i];
        }
        c2[len] = Config.TYPE_CHECK;
        // c2[len+1] = Config.WITH_LOC_INF; // XXX: Trips up CI.
        model = assertParse(code, c2);
        if (model.hasErrors()) {
            fail(model.getErrors().getFirstError().getHelpMessage());
        } else {
            SemanticConditionList el = model.typeCheck();
            if (el.containsErrors()) {
                fail(el.getFirstError().getMsg());
            }
        }

        if (model.hasErrors()) {
            fail(model.getErrors().getFirstError().getHelpMessage());
            return null;
        }
        return getJavaCode(model);
    }

    static JavaCode getJavaCode(Model model) throws IOException, JavaCodeGenerationException {
        JavaCode code = new JavaCode();
        model.generateJavaCode(code);
        return code;
    }

    public void assertEvalTrue(String absCode) throws Exception {
        assertEvalEquals(absCode, true);
    }

    public void assertEvalEquals(String absCode, boolean value) throws Exception {
        JavaCode javaCode = getJavaCode(absCode, Config.WITH_STD_LIB);
        if (DEBUG)
            System.err.println(javaCode);
        boolean res = runJavaAndTestResult(javaCode, false);
        if (value != res) {
            //System.out.println(javaCode);
        }
        assertEquals(value, res);
    }

    public void assertEvalFails(String absCode) throws Exception {
        JavaCode javaCode = getJavaCode(absCode, Config.WITH_STD_LIB);
        try {
            runJavaAndTestResult(javaCode, true);
            System.err.println(javaCode);
            fail("Expected that Java run failed, but did not.");
        } catch (NoTestResultFoundException e) {
            // OK
        }
    }

}
