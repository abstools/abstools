/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */

package eu.hats_project.build.maven.plugin;

import java.io.BufferedReader;
import java.io.File;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.List;
import java.util.regex.Pattern;

import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.MojoFailureException;
import org.codehaus.plexus.util.FileUtils;

/**
 * A Maven 2 plugin to simulate ABS test code in Maude
 * 
 * @goal maudetest
 * @requiresDependencyResolution
 * @phase test
 */
public class MaudeTestMojo extends AbstractABSMojo {

    /**
     * @parameter expression="${abs.maude.interpreter}"
     * @required           
     */
    private File maudeInterpreter;
    
    /**
     * The ABS Maude Backend output file.
     * 
     * @parameter expression="${abs.maudeBackend.testOutput}"
     *            default-value="${project.build.directory}/abs/gen/maude/test.maude"
     * @required
     */
    private File absMaudeBackendTestOutputFile;

    /**
     * The maude executable
     * 
     * @parameter expression="${maude.executable}"
     *            default-value="maude"
     * @required
     */
    private String maude;
    
    /**
     * Delta names to applied during testing
     * 
     * @parameter
     * 
     */
    private String[] deltaNames;
    
    /**
     * Product selection
     * 
     * @parameter
     * 
     */
    private String productName;
    
    /**
     * @parameter expression="${abs.maudetest.verbose}" default-value=false
     */
    private boolean verbose;
    
    private static final Pattern termination = 
    	Pattern.compile("^.*\\[State\\]:.*$",Pattern.MULTILINE);
	
    @Override
    protected void doExecute() throws Exception {
        
    	if (productName != null && deltaNames != null && deltaNames.length > 0) {
    		throw new MojoExecutionException("Cannot perform product selection" +
    				"and apply deltas on rewrite at the same time");
    	}
    	
    	if (absTestSrcFolder == null) {
    		getLog().warn("Test folder cannot be found. Skip tests");
    		return;
    	} else if (! absTestSrcFolder.exists()) {
			getLog().warn(
					String.format("There is no test code at folder %s",
							absTestSrcFolder));
			return;
    	}
    	
    	//generate test.maude
        MaudeGenerator generator = new MaudeGenerator();
        generator.generateMaude(
                absfrontEnd, 
                absTestSrcFolder, 
                getABSArguments(), 
                absMaudeBackendTestOutputFile, 
                verbose,
                productName);

        //run maude
        final String maudeOutput = runMaude();
        
        if (! maudeTerminatesSuccessfully(maudeOutput)) {
        	getLog().error("Maude Test fails.");
        	getLog().error(maudeOutput);
			throw new MojoFailureException(
					"One or more maude tests have failed, see log information for details.");
        } else {
            // only in debug
            getLog().debug(maudeOutput);
        }
        
    }

    /**
     * XXX Copied from {@link abs.backend.maude.MaudeTests}
     * @throws Exception
     */
    private String runMaude() throws Exception {
        String generatedMaudeCode = FileUtils.fileRead(absMaudeBackendTestOutputFile);
        return getMaudeOutput(generatedMaudeCode);
    }

    /**
     * XXX Copied from {@link abs.backend.maude.MaudeTests}
     * @throws Exception
     */
    protected String getMaudeOutput(String maudeCode) throws IOException, InterruptedException {
        StringBuffer result = new StringBuffer();
        ProcessBuilder pb = new ProcessBuilder();

        String[] cmd = { maude, "-no-banner", "-no-ansi-color", "-no-wrap", "-batch", maudeInterpreter.getAbsolutePath() };
        pb.command(cmd);

        //pb.redirectErrorStream(true);
        Process p = pb.start();

        BufferedReader in = new BufferedReader(new InputStreamReader(p.getInputStream()));
        PrintWriter out = new PrintWriter(p.getOutputStream());
        while (in.ready()) {
            result.append(in.readLine());
        }
        out.println(maudeCode);
        out.flush();
        while (in.ready()) {
            result.append(in.readLine());
        }
        
        if (deltaNames == null || deltaNames.length == 0) {
        	out.println("rew start .");
        } else {
        	// apply deltas
        	String names = "";
        	for (String name : deltaNames) {
        		names = names + "\""+ name + "\" ";
        	}
        	out.println("rew start ("+names+") .");
        }
        
        out.flush();
        while (in.ready()) {
            result.append(in.readLine() + "\n");
        }
        out.println("quit");
        out.flush();
        p.waitFor();
        while (in.ready()) {
            result.append(in.readLine() + "\n");
        }
        return result.toString();
    }

    @Override
    protected List<String> getABSArguments() throws Exception {
        List<String> args = new ArrayList<String>();
        args.addAll(getFileNames(getAbsFiles(absTestSrcFolder)));
        args.addAll(super.getABSArguments());
        return args;
    }
    
	/**
	 * Check that the input string representing the Maude output does not match
	 * against '[State]' as this indicates the simulation did not terminate
	 * successfully.
	 * 
	 * @param maudeOutput
	 * @return
	 */
    boolean maudeTerminatesSuccessfully(String maudeOutput) {
    	return ! termination.matcher(maudeOutput).find();
    }
    
}
