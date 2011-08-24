package eu.hats_project.build.maven.plugin;

import java.io.File;
import java.util.ArrayList;
import java.util.List;

import mtvl.parser.Main;

import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.logging.Log;

/**
 * @author pwong
 */
abstract class MTVLParser {

    protected void parseMTVL(
            File mTVL,
            File absSrcFolder, 
            List<String> absArguments, 
            String productName,
            boolean verbose,
            boolean solve,
            boolean satifiability,
            Log log) throws Exception {
        
        if (productName == null && satifiability) {
            throw new MojoExecutionException("Cannot check satifiability " +
            		"without specifying a product name");
        }
        
        if (!absSrcFolder.exists()) {
            throw new MojoExecutionException("Source folder does not exist");
        }
        
        List<String> args = new ArrayList<String>();
        String prop = System.getProperty("java.class.path");
        if (prop == null)
            System.setProperty("java.class.path",mTVL.getAbsolutePath());
        else 
            System.setProperty("java.class.path",prop+":"+mTVL.getAbsolutePath());

        if (verbose) {
            args.add("-v");
        }
        
        if (solve) {
            args.add("-s");
        }
        
        if (satifiability) {
            args.add("-c");
        }
        
        if (productName != null) {
            args.add(productName);
        }
        
        args.addAll(absArguments);
        String[] argArray = args.toArray(new String[args.size()]);
        new DebugArgOutput().debug("Parsing MTVL File", argArray, log);
        
        Main.main(argArray);
        
    }

}
