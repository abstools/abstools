package eu.hats_project.build.maven.plugin;

import java.io.File;
import java.util.ArrayList;
import java.util.List;

import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.logging.Log;

import abs.frontend.parser.Main;

/**
 * ABS Type Checker
 * @author pwong
 *
 */
public class TypeChecker extends MTVLParser {

    void typeCheck(File absfrontEnd, 
            File mTVL,
            File absSrcFolder, 
            List<String> absArguments, 
            boolean checkProductSelection,
            boolean verbose,
            boolean stdlib, 
            boolean loctype, 
            String productName, 
            Log log) throws MojoExecutionException {
        
        if (!absSrcFolder.exists()) {
            throw new MojoExecutionException("Source folder does not exist");
        }
        
        List<String> args = new ArrayList<String>();
        System.setProperty("java.class.path",absfrontEnd.getAbsolutePath());

        absArguments = 
            super.parseMTVL(mTVL, absArguments, productName, verbose, checkProductSelection, log);
        
        if (productName != null) {
            args.add("-product="+productName);
        }
        
        if (! stdlib) {
            args.add("-nostdlib");
        }
        
        if (loctype) {
            args.add("-loctypes");
        }
        
        if (verbose) {
            args.add("-v");
        }
        
        args.addAll(absArguments);
        
        String[] argArray = args.toArray(new String[args.size()]);
        new DebugArgOutput().debug("Type checking ABS modules", argArray, log);
        
        Main.main(argArray);
        
    }
    
}
