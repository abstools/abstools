package eu.hatsproject.absplugin.actions;

import java.util.List;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.action.IAction;

import abs.common.WrongProgramArgumentException;
import abs.frontend.ast.Model;
import eu.hatsproject.absplugin.actions.runconfig.RunConfigEnums.DebuggerObserver;
import eu.hatsproject.absplugin.actions.runconfig.RunConfigEnums.DebuggerScheduler;
import eu.hatsproject.absplugin.exceptions.AbsJobException;
import eu.hatsproject.absplugin.util.Constants;

public class ABSUnitTestJavaExecutionJob extends ABSUnitTestExecutionJob {

	private final IAction action;
	private final IFile file;
	private final String otherArgs;
	private final String randomSeeds;
	private final boolean compile;
	private final List<String> observerStrings;
	private final List<String> classPathList;
	private final String schedulerString;
	private final boolean useExternalDebugger;
	private final boolean debugMode;

	public ABSUnitTestJavaExecutionJob(IProject project,
			String productName,
			IAction action,
			IFile file,
			String otherArgs,
			String randomSeeds,
			boolean compile,
			List<String> observerStrings,
			List<String> classPathList,
			String schedulerString,
			boolean useExternalDebugger,
			boolean debugMode) {
		super(project, productName);
		this.action = action;
		this.file = file;
		this.otherArgs = otherArgs;
		this.randomSeeds = randomSeeds;
		this.compile = compile;
		this.observerStrings = observerStrings;
		this.classPathList = classPathList;
		this.schedulerString = schedulerString;
		this.useExternalDebugger = useExternalDebugger;
		this.debugMode = debugMode;
	}

	protected void executeTest(IProgressMonitor monitor) throws CoreException {
		JavaJob job = new JavaJob(JavaJob.RUN_JOB, action, project, file, true);
		if (productName != null) {
			try {
				Model model = JavaJob.getModelFromProject(project);
				job.setProduct(model.findProduct(productName));
			} catch (WrongProgramArgumentException e) {
				throw new CoreException(new Status(IStatus.ERROR, Constants.PLUGIN_ID, "Launch failed", e));
			} catch (AbsJobException e) {
				throw new CoreException(new Status(IStatus.ERROR, Constants.PLUGIN_ID, "Launch failed", e));
			}
		}
		modifyDebuggerArguments(job);
		job.runJob(monitor);
	}
	
	private void modifyDebuggerArguments(JavaJob job) {
		job.setDebuggerArgsOther(otherArgs);
		job.setArgsDebuggerRandomSeed(randomSeeds);
		job.setDebuggerCompileFirst(compile);
		
		String observerArgs = "";
		for (String observerClassName : observerStrings) {
			if(!observerClassName.isEmpty()){
				if (observerArgs.equals("")){
					observerArgs = DebuggerObserver.getEmptyCommand()+observerClassName;
				} else {
					observerArgs += ","+observerClassName;
				}
			}
		}
		job.setDebuggerArgsSystemObserver(observerArgs);
		job.setExtraClassPaths(classPathList);
		job.setDebuggerArgsTotalScheduler(DebuggerScheduler.valueOf(schedulerString).getCommand());
		job.setInternalDebugger(!useExternalDebugger);
		job.setDebuggerDebugMode(debugMode);
	}

}
