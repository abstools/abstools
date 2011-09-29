package eu.hats_project.build.maven.plugin;

import java.io.File;
import java.util.ArrayList;
import java.util.List;

import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.MojoFailureException;
import org.apache.maven.plugin.logging.Log;

import abs.backend.tests.ABSTestRunnerCompiler;

public class TestRunnerGenerator extends MTVLParser {

    List<String> generateTestRunner(
            File absfrontEnd, 
            List<String> absfiles, 
            File absTestRunnerFile,
            Log log) throws Exception {

        if (!absTestRunnerFile.exists() && !absTestRunnerFile.getParentFile().mkdirs()
                && !absTestRunnerFile.createNewFile()) {
            throw new MojoFailureException("Cannot write to file: " + absTestRunnerFile);
        }

        List<String> args = new ArrayList<String>();
        System.setProperty("java.class.path", absfrontEnd.getAbsolutePath());
        args.add("-o");
        args.add(absTestRunnerFile.getAbsolutePath());
        
        absfiles = super.parseMTVL(absfrontEnd, absfiles, null, true, false, log);
        args.addAll(absfiles);
        
        String[] argArray = args.toArray(new String[args.size()]);
        new DebugArgOutput().debug("Generating Test Runner", argArray, log);

        try {
            ABSTestRunnerCompiler.main(args.toArray(new String[0]));
        } catch (Exception e) {
            throw new MojoExecutionException("Could not generate ABSUnit test runner", e);
        }
        
        return args;

    }

}
