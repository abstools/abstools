package eu.hats_project.build.maven.plugin;
import java.io.File;

import org.apache.maven.plugin.AbstractMojo;
import org.apache.maven.plugin.MojoExecutionException;

import abs.backend.java.*;

/**
 * Says "Hi" to the user.
 * @goal 
 */
public class ABSMojo extends AbstractMojo
{
	/**
     * The ABS source folder.
     *
     * @parameter expression="${abs.srcFolder}" default-value="src/main/abs"
     */
    private File absSrcFolder;
    
	/**
     * The ABS Java Backend target folder.
     *
     * @parameter expression="${abs.javaBackend.targetFolder}" default-value="target/main/java"
     */
    private File absJavaBackendTargetFolder;
    
    /**
     * @parameter expression="${abs.javaBackend.sourceOnly}" default-value=false
     */
    private boolean sourceOnly;
	
    public void execute() throws MojoExecutionException
    {
    	
    }
}

