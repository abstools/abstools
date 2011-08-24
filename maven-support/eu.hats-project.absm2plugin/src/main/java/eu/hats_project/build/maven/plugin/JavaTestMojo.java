package eu.hats_project.build.maven.plugin;

import java.io.BufferedReader;
import java.io.File;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.apache.maven.plugin.MojoFailureException;

import abs.backend.tests.ABSTestRunnerGenerator;

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
    
    /**
     * @parameter expression="${classpaths}"
     */
    private String[] classPaths;
    
    /**
     * @parameter expression="${dependencies}"
     */
    private String[] dependencies;
    
    /**
     * @parameter expression="${abs.terminateOnException}" default-value=false
     */
    private Boolean terminateOnException;

    private String[] jvm = new String[0];
    
    /**
     * Default output appender
     */
    private Appendable appender = new StringBuilder(); 
    
    @Override
    protected void makeTest() throws Exception {

        // generate Java code
        JavaGenerator generator = new JavaGenerator();
        List<String> args = new ArrayList<String>();
        if (absTestRunnerFile.exists()) {
            args.add(absTestRunnerFile.getAbsolutePath());
        }
        args.addAll(getABSArguments());
        generator.generateJava(
                absfrontEnd, mTVL, absTestSrcFolder, args, absJavaBackendTestTargetFolder,
                checkProductSelection, verbose, false, true, loctype, productName, getLog());

        // run java
        if (terminateOnException) {
            jvm = Arrays.copyOf(jvm,jvm.length+1);
            jvm[jvm.length-1] = "-Dabs.terminateOnException=true";
        }
        
        runJava(jvm);
        
        if (appender instanceof StringBuilder) {
            final String result = appender.toString();
            if (result.length() > 0) {
                getLog().error("Java Test fails.");
                getLog().error(result);
                throw new MojoFailureException("One or more Java tests have failed, see log information for details.");
            }
            // only in debug
            getLog().debug(result);
        }

    }
    
    protected void setJVMOptions(String[] opts) {
        this.jvm = opts;
    }
    
    protected void setAppendable(Appendable appendable) {
        this.appender = appendable;
    }

    private Appendable runJava(String... jvmargs) throws MojoFailureException {

        try {
            String classpath = absfrontEnd.getAbsolutePath() + ":" + absJavaBackendTestTargetFolder.getAbsolutePath();
            
            if (classPaths != null) {
                for (String cp : classPaths) {
                    classpath += ":" + cp;
                }
            }
            
            if (dependencies != null) {
                for (String d : dependencies) {
                    String[] ds = d.split(":");
                    if (ds.length != 3) {
                        throw new MojoFailureException("Cannot resolve dependency "+d);
                    }
                    
                    try {
                        classpath += ":" + getClasspath(ds[0], ds[1], ds[2]);
                    } catch (Exception e) {
                        throw new MojoFailureException("Cannot resolve dependency "+d,e);
                    }
                }
            }

            List<String> args = new ArrayList<String>();
            args.add("java");
            args.addAll(Arrays.asList(jvmargs));
            
            String main = ((mainBlock == null) ? ABSTestRunnerGenerator.RUNNER_MAIN : mainBlock) + ".Main";
            args.addAll(Arrays.asList("-cp", classpath, main));
            
            String[] argArray = args.toArray(new String[args.size()]);
            new DebugArgOutput().debug("Executing generated Java code", argArray, getLog());
            
            ProcessBuilder pb = new ProcessBuilder(args.toArray(new String[0]));
            pb.redirectErrorStream(true);
            Process p = pb.start();
            BufferedReader r = new BufferedReader(new InputStreamReader(p.getInputStream()));
            while (true) {
                String s;
                s = r.readLine();
                if (s == null)
                    break;
                appender.append(s + "\n");
            }
            return appender;
        } catch (IOException e) {
            e.printStackTrace();
            return null;
        }

    }

}
