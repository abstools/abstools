package eu.hats_project.build.maven.plugin;

import java.io.File;
import java.util.ArrayList;
import java.util.List;

import org.apache.maven.plugin.MojoExecutionException;

import abs.backend.maude.MaudeCompiler;

public class MaudeGenerator {

    void generateMaude(File absfrontEnd, File absSrcFolder, List<String> absArguments, File absMaudeBackendOutputFile, boolean verbose) throws MojoExecutionException {
        if (!absMaudeBackendOutputFile.getParentFile().exists()) {
            if (!absMaudeBackendOutputFile.getParentFile().mkdirs()) {
                throw new MojoExecutionException("Could not create folder for output file " + absMaudeBackendOutputFile);
            }
        }

        if (!absSrcFolder.exists()) {
            throw new MojoExecutionException("Source folder does not exist");
        }

        List<String> args = new ArrayList<String>();
        System.setProperty("java.class.path", absfrontEnd.getAbsolutePath());
        args.add("-o");
        args.add(absMaudeBackendOutputFile.getAbsolutePath());

        if (verbose) {
            args.add("-v");
        }

        args.addAll(absArguments);

        try {
            MaudeCompiler.main(args.toArray(new String[0]));
        } catch (Exception e) {
            throw new MojoExecutionException("Could not generate Maude script", e);
        }
 
    }
    
}
