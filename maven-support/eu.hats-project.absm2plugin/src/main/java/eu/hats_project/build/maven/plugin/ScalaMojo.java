/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */

package eu.hats_project.build.maven.plugin;

import java.io.File;

/**
 * A Maven 2 plugin for the ABS To Scala compiler
 * 
 * @goal genscala
 * @phase generate-sources
 */
public class ScalaMojo extends AbstractABSMojo {
	/**
     * The ABS Scala Backend output directory.
     * 
     * @parameter expression="${abs.scalaBackend.outputDirectory}"
     *            default-value="${project.build.directory}/generated-sources/abs"
     * @required
     */
    private File outputDirectory;
    
	@Override
	protected void doExecute() throws Exception {
        ScalaGenerator generator = new ScalaGenerator();
        generator.generateScala(
                absfrontEnd, 
                mTVL,
                absSrcFolder, 
                getABSArguments(), 
                outputDirectory,
                checkProductSelection,
                verbose,
                stdlib,
                loctype,
                productName,
                getLog());
        
        project.addCompileSourceRoot(outputDirectory.getAbsolutePath());
	}

}
