package eu.hats_project.build.maven.plugin;

import java.util.ArrayList;
import java.util.List;

import abs.frontend.parser.Main;

/**
 * A Maven 2 plugin for type checking ABS
 * 
 * @goal typecheck
 * @phase compile
 * @requiresDependencyResolution compile
 */
public class TypeCheckingMojo extends AbstractABSMojo {

    protected void doExecute() throws Exception {

        List<String> args = new ArrayList<String>();
        System.setProperty("java.class.path",absfrontEnd.getAbsolutePath());
        
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
        
        args.addAll(getABSArguments());
        
        String[] argArray = args.toArray(new String[args.size()]);
        getLog().debug("Type checking ABS modules -->");
        for (String a : argArray) { 
            getLog().debug(a);
        }
        
        Main.main(argArray);
        
    }

}
