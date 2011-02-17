package eu.hatsproject.absplugin.actions;

import static eu.hatsproject.absplugin.util.Constants.PLUGIN_ID;

import java.io.IOException;
import java.io.PrintStream;

import org.eclipse.core.resources.WorkspaceJob;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.IJobChangeEvent;
import org.eclipse.core.runtime.jobs.IJobChangeListener;

import eu.hatsproject.absplugin.console.ConsoleManager;
import eu.hatsproject.absplugin.console.ConsoleManager.MessageType;
import eu.hatsproject.absplugin.console.MsgConsole;
import eu.hatsproject.absplugin.util.UtilityFunctions;

final class SDEditWatchdog extends WorkspaceJob {
	
	private final JavaJob javaJob;
	private MsgConsole sdeditconsole; 
	private Process sdeditProcess; 
	private Process debugProcess;

	SDEditWatchdog(JavaJob javaJob, String name, MsgConsole sdeditconsole, Process sdeditProcess, Process debugProcess) {
		super(name);
		this.javaJob = javaJob;
		this.addJobChangeListener(new SDEditWatchdogListener());
		this.sdeditconsole = sdeditconsole;
		this.debugProcess = debugProcess;
		this.sdeditProcess = sdeditProcess;
	}

	@Override
	public IStatus runInWorkspace(IProgressMonitor monitor)
			throws CoreException {
		try {
			ConsoleManager.addConsole(sdeditconsole);
			PrintStream out = sdeditconsole.getPrintStream(MessageType.MESSAGE_INFO);
			PrintStream err = sdeditconsole.getPrintStream(MessageType.MESSAGE_ERROR);
			UtilityFunctions.getProcessOutput(javaJob.getSdeditProcess(), out, err);
			int exitValue = sdeditProcess.waitFor();
			if(exitValue == 0){
				return new Status(Status.OK, PLUGIN_ID, "SDEdit finished successfully.");
			} else {
				return new Status(Status.ERROR, PLUGIN_ID, "SDEdit finished with errors!!");
			}
		} catch (InterruptedException e) {
			e.printStackTrace();
			return new Status(Status.ERROR, PLUGIN_ID,"Fatal Error!", e);
		} catch (IOException e) {
			e.printStackTrace();
			return new Status(Status.ERROR, PLUGIN_ID,"Fatal Error!", e);
		}
		
	}
	
	public final class SDEditWatchdogListener implements IJobChangeListener {

		@Override
		public void sleeping(IJobChangeEvent event) {
		}

		@Override
		public void scheduled(IJobChangeEvent event) {
		}

		@Override
		public void running(IJobChangeEvent event) {
		}

		@Override
		public void done(IJobChangeEvent event) {
			IStatus jobstatus = event.getResult();
			switch(jobstatus.getSeverity()){
			case Status.OK:
				
				break;
			case Status.ERROR:
				Throwable exception = jobstatus.getException();
				if(exception!= null){
					exception.printStackTrace(sdeditconsole.getPrintStream(MessageType.MESSAGE_ERROR));
				}
				if (debugProcess != null) {
				   debugProcess.destroy();
				}
				break;
			default:
				// How do we end up here???
				assert false;
				break;
			}
		}

		@Override
		public void awake(IJobChangeEvent event) {
		}

		@Override
		public void aboutToRun(IJobChangeEvent event) {
		}
	}
}