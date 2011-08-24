package eu.hats_project.build.maven.plugin;

import java.io.File;
import java.util.ArrayList;
import java.util.List;

import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.logging.Log;

import abs.backend.java.JavaBackend;

/**
 * 
 * @author pwong
 *
 */
public class JavaGenerator extends MTVLParser {
    
    /**
     * 
     * @param absfrontEnd The frontend jar
     * @param absSrcFolder 
     * @param absArguments
     * @param absJavaBackendTargetFolder
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
    void generateJava(File absfrontEnd, 
            File mTVL,
            File absSrcFolder, 
            List<String> absArguments, 
            File absJavaBackendTargetFolder, 
            boolean checkProductSelection,
            boolean verbose, 
            boolean sourceOnly,
            boolean stdlib, 
            boolean loctype, 
            String productName, 
            Log log) throws MojoExecutionException {
        
        if (!absJavaBackendTargetFolder.exists()) {
            if (!absJavaBackendTargetFolder.mkdirs()) {
                throw new MojoExecutionException("Could not create target folder " + absJavaBackendTargetFolder);
            }
        }

        if (!absSrcFolder.exists()) {
            throw new MojoExecutionException("Source folder does not exist");
        }

        List<String> args = new ArrayList<String>();
        System.setProperty("java.class.path",absfrontEnd.getAbsolutePath());
        args.add("-d");
        args.add(absJavaBackendTargetFolder.getAbsolutePath());
        
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
        
        if (sourceOnly) {
            args.add("-sourceonly");
        }
        
        if (verbose) {
            args.add("-v");
        }
        
        args.addAll(absArguments);
        
        String[] argArray = args.toArray(new String[args.size()]);
        new DebugArgOutput().debug("Generating Java Code", argArray, log);
        
        JavaBackend.main(argArray);
    }
}
