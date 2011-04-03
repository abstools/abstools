/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */

package eu.hats_project.build.maven.plugin;

import java.io.File;
import java.util.ArrayList;
import java.util.List;

import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.MojoFailureException;

import abs.backend.maude.MaudeCompiler;

/**
 * A Maven 2 plugin for the ABS To Maude compiler
 * 
 * @goal genmaude
 * @requiresDependencyResolution
 * @phase compile
 */
public class MaudePlugin extends AbstractABSMojo {

    /**
     * The ABS Maude Backend output file.
     * 
     * @parameter expression="${abs.maudeBackend.output}"
     *            default-value="${project.build.directory}/abs/gen/maude/output.maude"
     * @required
     */
    private File absMaudeBackendOutputFile;
    
    /**
     * @parameter expression="${abs.maudeBackend.verbose}" default-value=false
     */
    private boolean verbose;


	@Override
	public void execute() throws MojoExecutionException, MojoFailureException {
		
        File absfrontend = getABSFrontEnd();
        if (absfrontend == null) {
            throw new MojoExecutionException("Cannot locate ABS frontend.");
        }
        
        if (!absMaudeBackendOutputFile.exists()) {
            if (!absMaudeBackendOutputFile.getParentFile().mkdirs()) {
                throw new MojoExecutionException("Could not create folder for output file " + absMaudeBackendOutputFile);
            }
        }

        if (!absSrcFolder.exists()) {
            throw new MojoExecutionException("Source folder does not exist");
        }

        List<String> args = new ArrayList<String>();
        System.setProperty("java.class.path",absfrontend.getAbsolutePath());
        args.add("-o");
        args.add(absMaudeBackendOutputFile.getAbsolutePath());
        
        if (verbose) {
            args.add("-v");
        }
        
        args.addAll(getFileNames(getAbsFiles(absSrcFolder)));

        try {
			MaudeCompiler.main(args.toArray(new String[0]));
		} catch (Exception e) {
			throw new MojoExecutionException("Could not generate Maude script",e);
		}


	}

}
