package eu.hats_project.build.maven.plugin;

import java.io.File;
import java.util.ArrayList;
import java.util.List;

import org.apache.maven.plugin.MojoExecutionException;

import abs.backend.java.JavaBackend;

/**
 * 
 * @author pwong
 *
 */
public class JavaGenerator {
    
    /**
     * 
     * @param absfrontEnd The frontend jar
     * @param absSrcFolder 
     * @param absArguments
     * @param absJavaBackendTargetFolder
     * @param verbose
     * @param sourceOnly
     * @param stdlib
     * @param productName
     * @throws MojoExecutionException
     */
    void generateJava(File absfrontEnd, 
            File absSrcFolder, 
            List<String> absArguments, 
            File absJavaBackendTargetFolder, 
            boolean verbose, 
            boolean sourceOnly,
            boolean stdlib, 
            String productName) throws MojoExecutionException {
        
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
            args.add("-product="+productName);
        }
        
        if (! stdlib) {
            args.add("-nostdlib");
        }
        
        if (sourceOnly) {
            args.add("-sourceonly");
        }
        
        if (verbose) {
            args.add("-v");
        }
        
        args.addAll(absArguments);
        
        JavaBackend.main(args.toArray(new String[args.size()]));
    }
}
