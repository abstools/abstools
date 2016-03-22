/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.absmodels.abs.plugin.actions;

import static org.absmodels.abs.plugin.util.Constants.PLUGIN_ID;

import java.io.IOException;
import java.io.PrintStream;

import org.absmodels.abs.plugin.actions.JavaJob.SDEditProcess;
import org.absmodels.abs.plugin.console.ConsoleManager;
import org.absmodels.abs.plugin.console.MsgConsole;
import org.absmodels.abs.plugin.console.ConsoleManager.MessageType;
import org.absmodels.abs.plugin.util.UtilityFunctions;
import org.eclipse.core.resources.WorkspaceJob;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.IJobChangeEvent;
import org.eclipse.core.runtime.jobs.IJobChangeListener;


final class SDEditWatchdog extends WorkspaceJob {
	
   private final SDEditProcess process;

	SDEditWatchdog(SDEditProcess process) {
		super("SDEdit running...");
		this.process = process;
		this.addJobChangeListener(new SDEditWatchdogListener());
	}

	@Override
	public IStatus runInWorkspace(IProgressMonitor monitor)
			throws CoreException {
		try {
			ConsoleManager.addConsole(process.getConsole());
			PrintStream out = process.getConsole().getPrintStream(MessageType.MESSAGE_INFO);
			PrintStream err = process.getConsole().getPrintStream(MessageType.MESSAGE_ERROR);
			UtilityFunctions.getProcessOutput(process.getProcess(), out, err);
			int exitValue = process.getProcess().waitFor();
			if(exitValue == 0){
				return new Status(Status.OK, PLUGIN_ID, "SDEdit finished successfully.");
			} else {
				return new Status(Status.ERROR, PLUGIN_ID, "SDEdit finished with errors!!");
			}
		} catch (InterruptedException e) {
			return handleException(e);
		} catch (IOException e) {
		   return handleException(e);
		}
		
	}

   private IStatus handleException(Exception e) {
      e.printStackTrace();
      return new Status(Status.ERROR, PLUGIN_ID,"Fatal Error!", e);
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
					exception.printStackTrace(process.getConsole().getPrintStream(MessageType.MESSAGE_ERROR));
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