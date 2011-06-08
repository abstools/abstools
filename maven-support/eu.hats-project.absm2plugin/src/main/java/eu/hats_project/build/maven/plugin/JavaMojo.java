/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */

package eu.hats_project.build.maven.plugin;

import java.io.File;

/**
 * A Maven 2 plugin for the ABS To Java compiler
 * 
 * @goal genjava
 * @phase compile
 * @requiresDependencyResolution compile
 */
public class JavaMojo extends AbstractABSMojo {

    /**
     * The ABS Java Backend target folder.
     * 
     * @parameter expression="${abs.javaBackend.targetFolder}"
     *            default-value="${project.build.directory}/abs/gen/java"
     * @required
     */
    private File absJavaBackendTargetFolder;
    
    /**
     * @parameter expression="${abs.javaBackend.sourceOnly}" default-value=false
     */
    private boolean sourceOnly;
       
    protected void doExecute() throws Exception {
        
       JavaGenerator generator = new JavaGenerator();
       generator.generateJava(
              absfrontEnd, 
              absSrcFolder, 
              getABSArguments(), 
              absJavaBackendTargetFolder, 
              verbose, 
              sourceOnly, 
              stdlib,
              loctype,
              productName);
      
    }
    
}
