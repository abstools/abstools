package org.abs_models.backend.scala;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardOpenOption;


public class DefaultScalaWriterSupplier implements JavaWriterSupplier {

	private final PathResolver pathResolver;
	  private final String packageName;
	  private final String extension;
	  private final Path outputDirectory;

	  public DefaultScalaWriterSupplier(PathResolver pathResolver, String packageName,
                                        Path outputDirectory) {
	    this(pathResolver, packageName, ".scala", outputDirectory);
	  }

	  public DefaultScalaWriterSupplier(PathResolver pathResolver, String packageName, String extension,
                                        Path outputDirectory) {
	    this.pathResolver = pathResolver;
	    this.packageName = packageName;
	    this.extension = extension;
	    this.outputDirectory = outputDirectory;
	  }

	  @Override
	  public ScalaWriter apply(String typeName) {
	    try {
	      Path fqdnOutputDirectory =
	          this.pathResolver.resolveOutputDirectory(packageName, outputDirectory);
	      System.out.println(fqdnOutputDirectory.resolve(typeName + this.extension));
	      return new ScalaWriter(
	          Files.newBufferedWriter(fqdnOutputDirectory.resolve(typeName + this.extension),
	              StandardOpenOption.CREATE, StandardOpenOption.WRITE),false);
	    } catch (IOException e) {
	      throw new IllegalArgumentException("Cannot create Java writer for " + typeName, e);
	    }
	  }

}
