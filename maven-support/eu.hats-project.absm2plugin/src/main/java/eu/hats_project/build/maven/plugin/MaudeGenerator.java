package eu.hats_project.build.maven.plugin;

import java.io.File;
import java.util.ArrayList;
import java.util.List;

import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.logging.Log;

import abs.backend.maude.MaudeCompiler;

/**
 * 
 * @author pwong
 *
 */
public class MaudeGenerator extends MTVLParser {

    /**
     * 
     * @param absfrontEnd
     * @param absSrcFolder
     * @param absArguments
     * @param absMaudeBackendOutputFile
     * @param verbose
     * @param stdlib 
     * @param loctype 
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
                File mTVL,
                File absSrcFolder, 
    		List<String> absArguments, 
    		File absMaudeBackendOutputFile, 
    		boolean checkProductSelection,
    		boolean verbose, 
    		boolean stdlib, 
    		boolean loctype, 
    		String productName,
    		String mainBlock, 
    		boolean timed,
    		Log log) throws MojoExecutionException {
    	
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

        super.parseMTVL(mTVL, absSrcFolder, absArguments, productName, verbose, checkProductSelection, log);
        if (productName != null) {
            args.add("-product="+productName);
        }
        
        if (verbose) {
            args.add("-v");
        }
        
        if (! stdlib) {
            args.add("-nostdlib");
        }
        
        if (loctype) {
            args.add("-loctypes");
        }
        
        if (timed) {
            args.add("-timed");
        }
        
        if (mainBlock != null) {
            args.add("-main="+mainBlock);
        }
        
        args.addAll(absArguments);
        
        String[] argArray = args.toArray(new String[args.size()]);
        new DebugArgOutput().debug("Generating Maude Code", argArray, log);

        try {
            MaudeCompiler.main(argArray);
        } catch (Exception e) {
            throw new MojoExecutionException("Could not generate Maude script", e);
        }
 
    }
    
}
