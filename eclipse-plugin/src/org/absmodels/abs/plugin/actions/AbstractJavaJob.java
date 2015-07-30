package org.absmodels.abs.plugin.actions;

import static org.absmodels.abs.plugin.util.Constants.ACTION_DEBUG_ID;
import static org.absmodels.abs.plugin.util.Constants.ACTION_START_SDE;
import static org.absmodels.abs.plugin.util.Constants.ALWAYS_COMPILE;
import static org.absmodels.abs.plugin.util.Constants.DO_DEBUG;
import static org.absmodels.abs.plugin.util.Constants.JAVA_SOURCE_PATH;
import static org.absmodels.abs.plugin.util.Constants.NATURE_ID;
import static org.absmodels.abs.plugin.util.Constants.NO_WARNINGS;
import static org.absmodels.abs.plugin.util.Constants.PLUGIN_ID;
import static org.absmodels.abs.plugin.util.Constants.SOURCE_ONLY;
import static org.absmodels.abs.plugin.util.UtilityFunctions.getAbsNature;
import static org.absmodels.abs.plugin.util.UtilityFunctions.standardExceptionHandling;

import org.absmodels.abs.plugin.console.MsgConsole;
import org.absmodels.abs.plugin.exceptions.AbsJobException;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.Assert;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.jface.action.IAction;

/**
 * A base abstract class for Java compilation tasks.
 * 
 * @see JavaJob
 * @see JabsCompileJob
 * 
 * @author unattributed
 * @author nobeh
 */
public abstract class AbstractJavaJob extends Job {

  // debug this class -> enables printlns
  protected static boolean debugMode = DO_DEBUG;

  protected final String name;
  protected final IAction action;
  protected final IProject project;
  protected final IFile file;
  protected MsgConsole javaConsole;

  // Job state and configuration
  protected Path javaPath;
  protected boolean compileCode;
  protected boolean debugCode;
  protected boolean sourceOnly;
  protected boolean noWarnings;
  protected Process debugProcess = null;
  protected boolean isCanceled = false;
  protected boolean startSDE;

  // used by run config
  protected String debuggerArgsOther;
  protected String debuggerArgsSystemObserver;
  protected String debuggerArgsTotalScheduler;
  protected String debuggerCompileFirst;
  protected String debuggerArgsRandomSeed;
  protected boolean terminateOnException = true;
  protected boolean useInternalDebugger;
  protected boolean debuggerIsInDebugMode;

  /**
   * Ctor.
   * 
   * @param name
   * @param action
   * @param project
   * @param file
   */
  public AbstractJavaJob(String name, IAction action, IProject project, IFile file) {
    super(name);
    this.name = name;
    this.action = action;
    this.project = project;
    this.file = file;
  }

  /**
   * Returns path of generated Java files. Throws an
   * IllegalArumentException, if path can not be found for the
   * given project.
   * 
   * @param project
   * @return
   * @throws CoreException
   * @throws AbsJobException
   */
  protected Path getPathToGeneratedJavaFiles(IProject project) throws AbsJobException {
    Path path;
    String tempPath;
    try {
      tempPath = getAbsNature(project).getProjectPreferenceStore().getString(JAVA_SOURCE_PATH);
      Assert.isLegal(tempPath != null);
    } catch (NullPointerException e) {
      standardExceptionHandling(e);
      throw new AbsJobException("No ABS project selected.");
    }
    path = new Path(tempPath);
    if (!path.isAbsolute()) {
      path = new Path(project.getLocation().append(path).toOSString());
    }
    return path;
  }

  protected void setUpConsole() {
    javaConsole = getAbsNature(project).getJavaConsole();
    javaConsole.clear();
  }

  /**
   * Init property options
   * 
   * @see org.absmodels.abs.plugin.properties.JavaPropertyPage
   * @throws AbsJobException, if project is not an ABS project
   */
  protected void loadPropertyOptions() throws AbsJobException {
    javaPath = getPathToGeneratedJavaFiles(project);
    sourceOnly = getAbsNature(project).getProjectPreferenceStore().getBoolean(SOURCE_ONLY);
    noWarnings = getAbsNature(project).getProjectPreferenceStore().getBoolean(NO_WARNINGS);
    compileCode = getAbsNature(project).getProjectPreferenceStore().getBoolean(ALWAYS_COMPILE);
    if (debuggerCompileFirst != null && debuggerCompileFirst.length() > 0) {
      compileCode = Boolean.valueOf(debuggerCompileFirst);
    }
  }

  /**
   * The action id decides which options are used. Options:
   * 
   * <pre>
   * only compile | start debugger | start debugger with SDEdit
   * </pre>
   */
  protected void setJobOptions() {
    if (action.getId().equals(ACTION_DEBUG_ID)) {
      debugCode = true;
      startSDE = false;
    } else if (action.getId().equals(ACTION_START_SDE)) {
      startSDE = true;
      debugCode = true;
    } else {
      debugCode = false;
      compileCode = true;
      startSDE = false;
    }
  }

  protected IStatus showInfoMessage(String errorMessage) {
    return new Status(Status.INFO, PLUGIN_ID, errorMessage);
  }

  protected IStatus showErrorMessage(String errorMessage) {
    if (debugMode)
      System.err.println(errorMessage);
    return new Status(Status.ERROR, PLUGIN_ID, errorMessage);
  }

  protected IStatus showErrorMessage(String errorMessage, Exception e) {
    // Errors are automatically logged by Eclipse
    return new Status(Status.ERROR, PLUGIN_ID, errorMessage + "\n" + e.getLocalizedMessage(), e);
  }

  protected IStatus createStatus() {
    if (isCanceled) {
      if (debugMode) {
        if (javaConsole != null) {
          System.out.println("Cancelled");
        }
      }
      return new Status(Status.CANCEL, PLUGIN_ID, "Job canceled");
    } else {
      if (debugMode) {
        System.out.println("End");
      }
      return new Status(Status.OK, PLUGIN_ID, "done");
    }
  }

  protected IStatus validateProject() {
    try {
      // must be an ABS project
      if (project == null || !project.exists() || !project.isOpen()) {
        return showErrorMessage("The project does not exist/is not open.");
      } else if (!project.hasNature(NATURE_ID)) {
        return showErrorMessage(
            "Chosen project '" + project.getName() + "' is not an ABS project.");
      }
      return showErrorMessage("Incompatible project: " + project.getName());
    } catch (Exception e) {
      return showErrorMessage("Project does not exist or project is not open", e);
    }
  }

}
