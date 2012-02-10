/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package eu.hatsproject.absplugin.navigator.actionProvider;

import static eu.hatsproject.absplugin.navigator.NavigatorUtils.getProject;
import static eu.hatsproject.absplugin.util.Constants.PLUGIN_ID;
import static eu.hatsproject.absplugin.util.UtilityFunctions.showErrorMessage;

import java.io.IOException;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.jface.action.Action;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.TreeSelection;
import org.eclipse.swt.widgets.Shell;

import eu.hatsproject.absplugin.actions.MavenJob;
import eu.hatsproject.absplugin.exceptions.AbsJobException;
import eu.hatsproject.absplugin.exceptions.NoABSNatureException;

/**
 * An {@link Action} for update package dependencies by invoking
 * maven goals
 * 
 * @author pwong
 *
 */
public class MavenAction extends RefreshDependenciesAction {

	public MavenAction(Shell shell, ISelection iSelection) {
		super(shell,iSelection,"Update (Maven)");
	}
	
	@Override
	public void run(){
		if (selection != null && selection instanceof TreeSelection){
			final IProject project = getProject((TreeSelection)selection);
			new Job("Maven") {
				
				protected IStatus run(IProgressMonitor monitor) {
					final MavenJob mavenJob = new MavenJob(project);
					mavenJob.setUser(true);
					try {
						mavenJob.runMavenUpdates();
					} catch (NoABSNatureException e) {
						showErrorMessage(e.getMessage());
					} catch (AbsJobException e) {
						showErrorMessage(e.getMessage());
					} catch (IOException e) {
						showErrorMessage(e.getMessage());
					}
					MavenAction.super.run();
					return new Status(IStatus.OK, PLUGIN_ID, "done");
				}
			}.schedule();
		}
	}

}
