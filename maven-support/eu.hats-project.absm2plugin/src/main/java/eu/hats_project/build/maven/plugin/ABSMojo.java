package eu.hats_project.build.maven.plugin;
import java.io.File;
import java.io.FileFilter;
import java.util.ArrayList;
import java.util.List;

import org.apache.maven.plugin.AbstractMojo;
import org.apache.maven.plugin.MojoExecutionException;

import abs.backend.java.*;

/**
 * A Maven 2 plugin for the ABS compiler
 * @goal genjava
 * @phase compile
 */
public class ABSMojo extends AbstractMojo
{
	/**
     * The ABS source folder.
     *
     * @parameter expression="${abs.srcFolder}" default-value="${project.basedir}/src/main/abs"
     * @required
     */
    private File absSrcFolder;
    
	/**
     * The ABS Java Backend target folder.
     *
     * @parameter expression="${abs.javaBackend.targetFolder}" default-value="${project.build.directory}/abs/gen/java"
     * @required
     */
    private File absJavaBackendTargetFolder;
    
    /**
     * @parameter expression="${abs.javaBackend.sourceOnly}" default-value=false
     */
    private boolean sourceOnly;

    public void execute() throws MojoExecutionException
    {
    	if (!absJavaBackendTargetFolder.exists()) {
    		if (!absJavaBackendTargetFolder.mkdirs()) {
    			throw new MojoExecutionException("Could not create target folder "+absJavaBackendTargetFolder);
    		}
    	}
    	
    	if (!absSrcFolder.exists()) {
			return;
    	}
    	
    	ArrayList<String> args = new ArrayList<String>();
    	args.add("-d");
    	args.add(absJavaBackendTargetFolder.getAbsolutePath());
    	if (sourceOnly) {
    		args.add("-sourceonly");
    	}
    	args.addAll(getFileNames(getAbsFiles(absSrcFolder)));
    	
    	JavaBackend.main(args.toArray(new String[0]));
    }
    
    
    private List<String> getFileNames(List<File> files) {
    	ArrayList<String> res = new ArrayList<String>(files.size());
    	for (File f : files) {
    		res.add(f.getAbsolutePath());
    	}
    	
    	return res;
    }
    
    private ArrayList<File> getAbsFiles(File dir) 
    {
    	ArrayList<File> absFiles = new ArrayList<File>();
    	for (File f : dir.listFiles()) {
    		if (!f.isHidden() && f.canRead()) {
    			if (f.isDirectory()) {
    				absFiles.addAll(getAbsFiles(f));
    			} else {
    				if (f.getName().endsWith(".abs")) {
    					absFiles.add(f);
    				}
    			}
    		}
    	}
    	return absFiles;
    }

}

