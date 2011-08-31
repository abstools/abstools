package eu.hats_project.build.maven.plugin;


/**
 * A Maven 2 plugin for type checking ABS
 * 
 * @goal typecheck
 * @phase compile
 * @requiresDependencyResolution compile
 */
public class TypeCheckingMojo extends AbstractABSMojo {

    protected void doExecute() throws Exception {

        TypeChecker checker = new TypeChecker();
        checker.typeCheck(
                mTVL, 
                absfrontEnd, 
                absSrcFolder, 
                getABSArguments(), 
                checkProductSelection, 
                verbose, 
                stdlib, 
                loctype, 
                productName, 
                getLog());
        
    }

}
