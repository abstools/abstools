package eu.hats_project.build.maven.plugin;

import java.io.File;
import java.util.ArrayList;
import java.util.List;

import org.apache.maven.plugin.MojoExecutionException;

import abs.backend.maude.MaudeCompiler;

/**
 * 
 * @author pwong
 *
 */
public class MaudeGenerator {

    /**
     * 
     * @param absfrontEnd
     * @param absSrcFolder
     * @param absArguments
     * @param absMaudeBackendOutputFile
     * @param verbose
     * @param stdlib 
     * @param productName
     * @param mainBlock
     *          sets the main block to execute
     * @param timed 
     *          generate code for timed interpreter
     *          
     * @throws MojoExecutionException
     */
    void generateMaude(
    		File absfrontEnd, 
    		File absSrcFolder, 
    		List<String> absArguments, 
    		File absMaudeBackendOutputFile, 
    		boolean verbose, 
    		boolean stdlib, 
    		String productName,
    		String mainBlock, 
    		boolean timed) throws MojoExecutionException {
    	
        if (!absMaudeBackendOutputFile.getParentFile().exists()) {
            if (!absMaudeBackendOutputFile.getParentFile().mkdirs()) {
                throw new MojoExecutionException("Could not create folder for output file " + absMaudeBackendOutputFile);
            }
        }

        if (!absSrcFolder.exists()) {
            throw new MojoExecutionException("Source folder does not exist");
        }

        List<String> args = new ArrayList<String>();
        System.setProperty("java.class.path", absfrontEnd.getAbsolutePath());
        args.add("-o");
        args.add(absMaudeBackendOutputFile.getAbsolutePath());

        if (productName != null) {
            args.add("-product="+productName);
        }
        
        if (verbose) {
            args.add("-v");
        }
        
        if (! stdlib) {
            args.add("-nostdlib");
        }
        
        if (timed) {
            args.add("-timed");
        }
        
        if (mainBlock != null) {
            args.add("-main="+mainBlock);
        }
        
        args.addAll(absArguments);

        try {
            MaudeCompiler.main(args.toArray(new String[0]));
        } catch (Exception e) {
            throw new MojoExecutionException("Could not generate Maude script", e);
        }
 
    }
    
}
