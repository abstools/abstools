/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved.
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.abs_models.backend.java;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.fail;

import java.io.BufferedReader;
import java.io.File;
import java.io.IOException;
import java.io.InputStreamReader;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.abs_models.ABSTest;
import org.abs_models.backend.java.codegeneration.JavaCode;
import org.abs_models.backend.java.codegeneration.JavaCodeGenerationException;
import org.abs_models.backend.java.scheduling.RandomSchedulingStrategy;
import org.abs_models.frontend.analyser.SemanticConditionList;
import org.abs_models.frontend.ast.Model;

public class JavaBackendTest extends ABSTest {

    private static final boolean DEBUG = false;
    private static final String LIB_CLASSPATH = "build/classes/java/main";

    @SuppressWarnings("serial")
    final protected List<String> jvmArgs = new ArrayList<String>() {{ add("-Dabs.terminateOnException=true"); }};
    /**
     * Additional arguments when running the compiled ABS program.
     */
    final protected List<String> absArgs = new ArrayList<>();

    public final long seed;
    public final static long seed_UNUSED = -1;

    public JavaBackendTest() { seed = seed_UNUSED; }

    public JavaBackendTest(long randomSeed) {
        jvmArgs.add("-Dabs.totalscheduler="+ RandomSchedulingStrategy.class.getName());
        jvmArgs.add("-Dabs.randomseed="+randomSeed);
        assert randomSeed != seed_UNUSED : "not a valid seed value";
        seed = randomSeed;
    }

    void assertValid(String absCode) throws Exception {
        assertValidJava(getJavaCode("module JavaUnitTest; " + absCode, Config.WITHOUT_MODULE_NAME));
    }

    public static void assertValidJava(JavaCode javaCode) throws IOException {
        try {
            javaCode.compile(javaCode.getSrcDir(), null, "-classpath", LIB_CLASSPATH);
        } catch (JavaCodeGenerationException e) {
            System.out.println(javaCode);
            fail();
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
        javaCode.compile(javaCode.getSrcDir(), null, "-classpath", LIB_CLASSPATH);

        ArrayList<String> args = new ArrayList<>();
        args.add("java");
        args.addAll(Arrays.asList(jvmargs));
        // The current directory when tests are run is abstools/frontend/
        Path frontend_jar = Paths.get("dist/absfrontend.jar").toAbsolutePath();
        args.addAll(Arrays.asList("-cp",
                                  frontend_jar.toString()
                                  + File.pathSeparator
                                  + javaCode.getSrcDir().getAbsolutePath(),
                                  javaCode.getFirstMainClass()));
        args.addAll(absArgs);
        ProcessBuilder pb = new ProcessBuilder(args.toArray(new String[0]));
        pb.directory(javaCode.getSrcDir());
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
        model.generateJavaCode(code, true);
        return code;
    }

    public void assertEvalTrue(String absCode) throws Exception {
        assertEvalEquals(absCode, true);
    }

    public void assertEvalEquals(String absCode, boolean value) throws Exception {
        JavaCode javaCode = getJavaCode(absCode);
        if (DEBUG)
            System.err.println(javaCode);
        boolean res = runJavaAndTestResult(javaCode, false);
        if (value != res) {
            //System.out.println(javaCode);
        }
        assertEquals(value, res);
    }

    public void assertEvalFails(String absCode) throws Exception {
        JavaCode javaCode = getJavaCode(absCode);
        try {
            runJavaAndTestResult(javaCode, true);
            System.err.println(javaCode);
            fail("Expected that Java run failed, but did not.");
        } catch (NoTestResultFoundException e) {
            // OK
        }
    }

}
