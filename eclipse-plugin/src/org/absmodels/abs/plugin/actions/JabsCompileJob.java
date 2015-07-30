package org.absmodels.abs.plugin.actions;

import java.io.IOException;
import java.nio.file.FileVisitResult;
import java.nio.file.FileVisitor;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.nio.file.SimpleFileVisitor;
import java.nio.file.attribute.BasicFileAttributes;
import java.util.Collections;
import java.util.HashSet;
import java.util.Set;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.jface.action.IAction;

/**
 * An PDE {@link Job} to compile ABS code to Java source using
 * {@link <a href="https://github.com/CrispOSS/jabsc">jabsc</a>}
 * 
 * @author nobeh
 */
public class JabsCompileJob extends AbstractJavaJob {

  /**
   * An NIO {@link FileVisitor} to collect all ABS sources in
   * the project.
   */
  static class SourceCollector extends SimpleFileVisitor<java.nio.file.Path> {
    private final Set<java.nio.file.Path> sources = new HashSet<java.nio.file.Path>();

    @Override
    public FileVisitResult preVisitDirectory(java.nio.file.Path dir, BasicFileAttributes attrs)
        throws IOException {
      return super.preVisitDirectory(dir, attrs);
    }

    @Override
    public FileVisitResult visitFile(java.nio.file.Path file, BasicFileAttributes attrs)
        throws IOException {
      super.visitFile(file, attrs);
      if (file.getFileName().endsWith(".abs")) {
        sources.add(file);
      }
      return FileVisitResult.CONTINUE;
    }
  }

  /**
   * Ctor.
   * 
   * @param name
   * @param action
   * @param project
   * @param file
   */
  public JabsCompileJob(String name, IAction action, IProject project, IFile file) {
    super(name, action, project, file);
  }

  @Override
  protected IStatus run(IProgressMonitor monitor) {
    try {
      setJobOptions();
      setUpConsole();
      execute(monitor, javaPath, project);
      project.refreshLocal(IProject.DEPTH_INFINITE, monitor);
      return showInfoMessage(":D:D:D");
    } catch (Exception e) {
      IStatus status = showErrorMessage("", e);
      return status;
    }
  }

  protected void execute(IProgressMonitor monitor, Path destinationPath, IProject project)
      throws IOException {
    Set<java.nio.file.Path> sources = list(this.file);
    System.out.println(sources);
    System.out.println(destinationPath);
  }

  private Set<java.nio.file.Path> list(IFile file) throws IOException {
    java.nio.file.Path p = Paths.get(file.getLocationURI());
    if (Files.isRegularFile(p) && file.getName().endsWith(".abs")) {
      return Collections.singleton(p);
    }
    System.out.println(p);
    if (!Files.isDirectory(p)) {
      p = p.getParent();
    }
    SourceCollector v = new SourceCollector();
    Files.walkFileTree(p, v);
    return v.sources;
  }

}
