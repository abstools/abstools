/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */

package eu.hats_project.build.maven.plugin;

import java.io.File;
import java.util.ArrayList;
import java.util.List;

import org.apache.maven.plugin.MojoExecutionException;

import abs.backend.java.JavaBackend;

/**
 * A Maven 2 plugin for the ABS To Java compiler
 * 
 * @goal genjava
 * @requiresDependencyResolution
 * @phase compile
 */
public class JavaMojo extends AbstractABSMojo {

    /**
     * The ABS Java Backend target folder.
     * 
     * @parameter expression="${abs.javaBackend.targetFolder}"
     *            default-value="${project.build.directory}/abs/gen/java"
     * @required
     */
    private File absJavaBackendTargetFolder;
    
    /**
     * @parameter expression="${abs.javaBackend.verbose}" default-value=false
     */
    private boolean verbose;

    /**
     * @parameter expression="${abs.javaBackend.sourceOnly}" default-value=false
     */
    private boolean sourceOnly;
    
    public void execute() throws MojoExecutionException {
        
        File absfrontend = getABSFrontEnd();
        if (absfrontend == null) {
            throw new MojoExecutionException("Cannot locate ABS frontend.");
        }
        
        if (!absJavaBackendTargetFolder.exists()) {
            if (!absJavaBackendTargetFolder.mkdirs()) {
                throw new MojoExecutionException("Could not create target folder " + absJavaBackendTargetFolder);
            }
        }

        if (!absSrcFolder.exists()) {
            throw new MojoExecutionException("Source folder does not exist");
        }

        List<String> args = new ArrayList<String>();
        System.setProperty("java.class.path",absfrontend.getAbsolutePath());
        args.add("-d");
        args.add(absJavaBackendTargetFolder.getAbsolutePath());
        
        if (sourceOnly) {
            args.add("-sourceonly");
        }
        
        if (verbose) {
            args.add("-v");
        }
        
        args.addAll(getFileNames(getAbsFiles(absSrcFolder)));

        JavaBackend.main(args.toArray(new String[0]));
    }
    
}
