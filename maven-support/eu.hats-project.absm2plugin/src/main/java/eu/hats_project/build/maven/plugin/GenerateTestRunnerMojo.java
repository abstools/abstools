package eu.hats_project.build.maven.plugin;

import java.io.File;

/**
 * 
 * A Maven 2 plugin to generate ABS test runner source file
 * 
 * @goal genrunner
 * @requiresDependencyResolution
 * @phase generate-test-sources
 * 
 */
public class GenerateTestRunnerMojo extends AbstractABSMojo {

    /**
     * The generated ABSUnit test runner file
     * 
     * @parameter expression="${abs.runnerOutput}"
     *            default-value="${project.build.directory}/abs/gen/abs/runner.abs"
     */
    protected File absTestRunnerFile;

    protected void doExecute() throws Exception {

        TestRunnerGenerator generator = new TestRunnerGenerator();
        generator.generateTestRunner(absfrontEnd, getABSArguments(), absTestRunnerFile, getLog());

    }
}
