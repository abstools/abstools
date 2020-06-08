package org.abs_models.backend.scala;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardOpenOption;

/**
 * An implementation of {@link JavaWriterSupplier} using the
 * package spec and the current output directory to generate
 * Java files.
 */
public class DefaultJavaWriterSupplier implements JavaWriterSupplier {

  private final PathResolver pathResolver;
  private final String packageName;
  private final String extension;
  private final Path outputDirectory;

  public DefaultJavaWriterSupplier(PathResolver pathResolver, String packageName,
                                   Path outputDirectory) {
    this(pathResolver, packageName, ".java", outputDirectory);
  }

  public DefaultJavaWriterSupplier(PathResolver pathResolver, String packageName, String extension,
                                   Path outputDirectory) {
    this.pathResolver = pathResolver;
    this.packageName = packageName;
    this.extension = extension;
    this.outputDirectory = outputDirectory;
  }

  @Override
  public JavaWriter apply(String typeName) {
    try {
      Path fqdnOutputDirectory =
          this.pathResolver.resolveOutputDirectory(packageName, outputDirectory);
      return new JavaWriter(
          Files.newBufferedWriter(fqdnOutputDirectory.resolve(typeName + this.extension),
              StandardOpenOption.CREATE, StandardOpenOption.WRITE),false);
    } catch (IOException e) {
      throw new IllegalArgumentException("Cannot create Java writer for " + typeName, e);
    }
  }

}
