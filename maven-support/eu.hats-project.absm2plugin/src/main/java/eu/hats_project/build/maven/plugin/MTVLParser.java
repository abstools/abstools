package eu.hats_project.build.maven.plugin;

import java.io.File;
import java.util.ArrayList;
import java.util.List;

import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.logging.Log;

import abs.frontend.parser.Main;

/**
 * @author pwong
 */
abstract class MTVLParser {
    
    protected List<String> parseMTVL(File absFrontend,
            List<String> absArguments, 
            String productName,
            boolean verbose,
            boolean checkProductSelection,
            Log log) throws MojoExecutionException {
        
        try {
            if (productName != null && checkProductSelection) {
                parseMTVL(absFrontend, absArguments, productName, verbose, false, true, false, log);
            } else {
                parseMTVL(absFrontend, absArguments, null, verbose, false, false, true, log);
            }
        } catch (Exception e) {
            throw new MojoExecutionException("Could not parse mTVL model", e);
        }
        
        return absArguments;
    }
    
    private void parseMTVL(
    		File absFrontend,
            List<String> absArguments, 
            String productName,
            boolean verbose,
            boolean solve,
            boolean satifiability,
            boolean solutions,
            Log log) throws Exception {
        
        if (productName == null && satifiability) {
            throw new MojoExecutionException("Cannot check satifiability " +
            		"without specifying a product name");
        }
        
        List<String> args = new ArrayList<String>();
        String prop = System.getProperty("java.class.path");
        if (prop == null)
            System.setProperty("java.class.path",absFrontend.getAbsolutePath());
        else 
            System.setProperty("java.class.path",prop+":"+absFrontend.getAbsolutePath());

        if (verbose) {
            args.add("-v");
        }
        
        if (solve) {
            args.add("-solve");
        }
        
        if (satifiability && productName != null) {
            args.add("-check="+productName);
        }
        
        if (solutions) {
            args.add("-nsol");
            args.add("-noattr"); // not sure what happens if attributes have infinite domain
        }
        
        args.addAll(absArguments);
        String[] argArray = args.toArray(new String[args.size()]);
        new DebugArgOutput().debug("Parsing MTVL File", argArray, log);
        
        Main.main(argArray);
        
    }

}
