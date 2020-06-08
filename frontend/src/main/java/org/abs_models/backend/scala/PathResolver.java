package org.abs_models.backend.scala;

import java.nio.file.Path;
import java.util.function.BiFunction;

/**
 * A path resolve abstraction using a name and a directory to
 * another path.
 */
@FunctionalInterface
public interface PathResolver extends BiFunction<String, Path, Path> {

  /**
   * Default basic implementation.
   */
  PathResolver DEFAULT_PATH_RESOLVER = (packageName, outputDirectory) -> {
    if (packageName == null || packageName.isEmpty()) {
      return outputDirectory;
    }
    String[] parts = packageName.split("\\.");
    for (String packagePart : parts) {
      outputDirectory = outputDirectory.resolve(packagePart);
    }
    return outputDirectory;
  };

  Path resolveOutputDirectory(String pakkage, Path outputDirectory);

  @Override
  default Path apply(String pakkage, Path outputDirectory) {
    return resolveOutputDirectory(pakkage, outputDirectory);
  }

}
