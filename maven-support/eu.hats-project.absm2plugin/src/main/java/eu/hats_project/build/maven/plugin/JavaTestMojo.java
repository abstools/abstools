package eu.hats_project.build.maven.plugin;

import java.io.BufferedReader;
import java.io.File;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.apache.maven.plugin.MojoFailureException;

/**
 * 
 * A Maven 2 plugin to execute ABS test code in Java
 * 
 * @goal javatest
 * @requiresDependencyResolution
 * @phase test
 * 
 */
public class JavaTestMojo extends AbstractTestMojo {

    /**
     * The ABS Java Backend target folder.
     * 
     * @parameter expression="${abs.javaBackend.targetFolder}"
     *            default-value="${project.build.directory}/abs/gen/test/java"
     * @required
     */
    private File absJavaBackendTestTargetFolder;

    /**
     * @parameter expression="${abs.javaBackend.verbose}" default-value=false
     */
    private boolean verbose;

    private final String testMainClass = "AbsUnit.TestRunner.Main";

    @Override
    protected void doExecute() throws Exception {

        /*
         * Generate test runner
         */
        super.doExecute();
        
        // generate Java code
        JavaGenerator generator = new JavaGenerator();
        List<String> args = new ArrayList<String>();
        if (absTestRunnerFile.exists()) {
            args.add(absTestRunnerFile.getAbsolutePath());
        }
        args.addAll(getABSArguments());
        generator.generateJava(absfrontEnd, absTestSrcFolder, args, absJavaBackendTestTargetFolder,
                verbose, false, true, productName);

        // run java
        StringBuilder result = runJava();

        if (result.length() > 0) {
            getLog().error("Java Test fails.");
            getLog().error(result);
            throw new MojoFailureException("One or more Java tests have failed, see log information for details.");
        }

        // only in debug
        getLog().debug(result);

    }

    private StringBuilder runJava(String... jvmargs) {
        StringBuilder output = new StringBuilder();

        try {
            String classpath = absfrontEnd.getAbsolutePath() + ":" + absJavaBackendTestTargetFolder.getAbsolutePath();

            ArrayList<String> args = new ArrayList<String>();
            args.add("java");
            args.addAll(Arrays.asList(jvmargs));
            args.addAll(Arrays.asList("-cp", classpath, testMainClass));
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
            return output;
        } catch (IOException e) {
            e.printStackTrace();
            return null;
        }

    }

}
