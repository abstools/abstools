package eu.hats_project.build.maven.plugin;

import java.io.File;
import java.util.ArrayList;
import java.util.List;

import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.MojoFailureException;

import abs.backend.tests.ABSTestRunnerCompiler;
import abs.backend.tests.ABSTestRunnerGenerator;

/**
 * An abstract class for test goals
 * @author pwong 
 */
abstract class AbstractTestMojo extends AbstractABSMojo {

    /**
     * Check if test runner needs to be generated before simulation
     * @parameter expression="${abs.generateRunner}"
     *            default-value=false
     */
    private boolean generateRunner;
    
    /**
     * The generated ABSUnit test runner file
     * 
     * @parameter expression="${abs.runnerOutput}"
     *            default-value="${project.build.directory}/abs/gen/abs/runner.abs"
     */
    protected File absTestRunnerFile;
    
    /**
     * Main block to execute
     * 
     * @parameter
     */
    protected String mainBlock;
    
    protected final void doExecute() throws Exception {
        if (absTestSrcFolder == null) {
            getLog().warn("Test folder cannot be found. Skip tests");
            return;
        } else if (!absTestSrcFolder.exists()) {
            getLog().warn(String.format("There is no test code at folder %s", absTestSrcFolder));
            return;
        }
        
        if (generateRunner && mainBlock != null) {
            throw new MojoExecutionException("Cannot generated test runner must reside in module "
                    + ABSTestRunnerGenerator.RUNNER_MAIN);
        } else if (! generateRunner && mainBlock == null) {
            throw new MojoExecutionException("A main block has not been specified");
        }
        
        if (generateRunner) {
            if (! absTestRunnerFile.exists() &&
                ! absTestRunnerFile.getParentFile().mkdirs() &&
                ! absTestRunnerFile.createNewFile()) {
                throw new MojoFailureException("Cannot write to file: "+absTestRunnerFile);
            }
            
            List<String> args = new ArrayList<String>();
            System.setProperty("java.class.path", absfrontEnd.getAbsolutePath());
            args.add("-o");
            args.add(absTestRunnerFile.getAbsolutePath());
            args.addAll(getABSArguments());
            
            try {
                ABSTestRunnerCompiler.main(args.toArray(new String[0]));
            } catch (Exception e) {
                throw new MojoExecutionException("Could not generate ABSUnit test runner", e);
            }
        }
        
        makeTest();
    }
    
    protected abstract void makeTest() throws Exception;
    
    @Override
    protected List<String> getABSArguments() throws Exception {
        List<String> args = new ArrayList<String>();
        args.addAll(getFileNames(getAbsFiles(absTestSrcFolder)));
        args.addAll(super.getABSArguments());
        return args;
    }

}
