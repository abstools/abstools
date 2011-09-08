/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */

package eu.hats_project.build.maven.plugin;

import java.io.File;
import java.util.ArrayList;
import java.util.List;

import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.logging.Log;

import abs.backend.scala.ScalaBackend;

public class ScalaGenerator extends MTVLParser {
    
    /**
     * 
     * @param absfrontEnd The frontend jar
     * @param absSrcFolder 
     * @param absArguments
     * @param targetFolder
     * @param verbose
     * @param sourceOnly
     * @param stdlib
     * @param loctype 
     * @param productName
     * @param log 
     * @param checkProductSelection 
     * @param mTVL 
     * @throws MojoExecutionException
     */
    void generateScala(File absfrontEnd, 
            File mTVL,
            File absSrcFolder, 
            List<String> absArguments, 
            File targetFolder, 
            boolean checkProductSelection,
            boolean verbose, 
            boolean stdlib, 
            boolean loctype, 
            String productName, 
            Log log) throws MojoExecutionException {
        
        if (!targetFolder.exists()) {
            if (!targetFolder.mkdirs()) {
                throw new MojoExecutionException("Could not create target folder " + targetFolder);
            }
        }

        if (!absSrcFolder.exists()) {
            throw new MojoExecutionException("Source folder does not exist");
        }

        List<String> args = new ArrayList<String>();
        System.setProperty("java.class.path",absfrontEnd.getAbsolutePath());
        args.add("-d");
        args.add(targetFolder.getAbsolutePath());
        
        if (productName != null) {
            
            if (checkProductSelection) {
                try {
                    super.parseMTVL(mTVL, 
                            absSrcFolder, 
                            absArguments, 
                            productName, 
                            verbose, 
                            false, 
                            true, 
                            log);
                } catch (Exception e) {
                    throw new MojoExecutionException("Could not parse mTVL model", e);
                }   
            }
            
            args.add("-product="+productName);
        }
        
        if (! stdlib) {
            args.add("-nostdlib");
        }
        
        if (loctype) {
            args.add("-loctypes");
        }
        
        args.add("-sourceonly");
        
        if (verbose) {
            args.add("-v");
        }
        
        args.addAll(absArguments);
        
        String[] argArray = args.toArray(new String[args.size()]);
        new DebugArgOutput().debug("Generating Scala Code", argArray, log);
        
        try {
			new ScalaBackend().compile(argArray);
		} catch (Exception e) {
			throw new MojoExecutionException("Error during code generation", e);
		}
    }

}
