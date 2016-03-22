/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.absmodels.abs.plugin.actions;

import static org.absmodels.abs.plugin.util.Constants.*;
import static org.absmodels.abs.plugin.util.UtilityFunctions.*;

import java.io.*;
import java.util.ArrayList;

import org.absmodels.abs.plugin.builder.AbsNature;
import org.absmodels.abs.plugin.exceptions.*;
import org.absmodels.abs.plugin.internal.NoModelException;
import org.eclipse.core.resources.*;
import org.eclipse.core.runtime.*;
import org.eclipse.core.runtime.jobs.Job;
import org.osgi.framework.Bundle;

import abs.backend.maude.MaudeCompiler;
import abs.common.WrongProgramArgumentException;
import abs.frontend.ast.Model;
import abs.frontend.delta.DeltaModellingException;

public class MaudeJob extends Job{
	private Process process;
	private IProject project;
	private File destFile;
	private boolean exec;
	private boolean realTime;
	private boolean abort;
	
	private boolean partialExec;
	private int steps;
	private String product;
	private String mainBlock; /* may be null or empty */
	
	/**
	 * MaudeJobs created with this constructor will generate a .maude file out of a given project and may execute
	 * the generated file afterwards.
	 * @param project The project to be executed
	 * @param realTime Indicates, if the realTime scheduler should be used. Note, that this functionality has 
	 * never been tested or used
	 * @param exec indicates, if the generated .maude file should be executed
	 */
	public MaudeJob(IProject project, boolean realTime, boolean exec){
		super("Maude Job");
		this.project     = project;
		this.realTime    = realTime;
		this.exec        = exec;
		this.partialExec = false;
		setUser(true);
	}
	
	/**
	 * MaudeJobs created with this constructor will generate a .maude file out of a given project and execute 
	 * Maude with a command resulting in a partial execution with only a given number of steps.
	 * @param project The project to be executed
	 * @param realTime Indicates, if the realTime scheduler should be used. Note, that this functionality has 
	 * never been tested or used
	 * @param exec (probably superfluous parameter here) indicates, if the generated .maude file should be executed
	 * @param steps Number of steps to be executed
	 */
	public MaudeJob(IProject project, boolean realTime, boolean exec, int steps){
		super("Maude Job");
		this.project     = project;
		this.realTime    = realTime;
		this.exec        = exec;
		this.partialExec = true;
		this.steps       = steps;
		setUser(true);
	}
	
	@Override
	public IStatus run(IProgressMonitor monitor){
		//Set title and totalWork of the process according to clicked button
		if(exec){
			monitor.beginTask("Executing Maude", 11);
		} else{
			monitor.beginTask("Compiling Maude", 6);
		}
		return runJob(monitor);
	}
	
	public IStatus runJob(IProgressMonitor monitor) {
		abort = false;
		boolean failed = false;
		StringBuffer output = new StringBuffer();
		AbsNature nature = getAbsNature(project);	
		if(nature == null){
			return new Status(IStatus.INFO, PLUGIN_ID, "Could not compile current selection. Project is not an ABS project.");
		}
		
		//Compile Maude Code
		monitor.subTask("Compiling ABS model to Maude");
		try{
			if(!abort) compileMaude(monitor,nature);
		} catch(CoreException e1) {
			return new Status(IStatus.ERROR, PLUGIN_ID, "Fatal error while compilig", e1);
		} catch (IOException e2) {
			return new Status(IStatus.ERROR, PLUGIN_ID, "Fatal error while compilig", e2);
		} catch (ParseException e4) {
			return new Status(IStatus.INFO, PLUGIN_ID, MAUDE_ERROR, "Could not compile current selection. Code has parse errors.", e4);
		} catch (TypeCheckerException e5) {
			return new Status(IStatus.INFO, PLUGIN_ID, MAUDE_ERROR, "Could not compile current selection. Code has type errors.", e5);
		} catch (WrongProgramArgumentException e) {
			return new Status(IStatus.ERROR, PLUGIN_ID, MAUDE_ERROR, "Could not compile current selection.", e);
		} catch (DeltaModellingException e) {
			return new Status(IStatus.ERROR, PLUGIN_ID, MAUDE_ERROR, "Could not compile current selection.", e);
		} catch (NoModelException e) {
			return new Status(IStatus.INFO, PLUGIN_ID, "No ABS model in project");
		} 
		monitor.worked(5);
		
		//Execute generated Maude code
		monitor.subTask("Executing generated Maude code");
		if(exec){
			try{
				if(!abort) failed = !executeMaude(output);
			} catch (IOException e1) {
				return new Status(IStatus.INFO, PLUGIN_ID, MAUDE_ERROR_MAUDE_PATH, "Encountered IOException while executing Maude (probably misconfigured location of maude executable)", e1);
			} catch (InterruptedException e2) {
				return new Status(IStatus.ERROR, PLUGIN_ID, "Fatal error while executing Maude", e2);
			} finally{
				if(!monitor.isCanceled()){
					monitor.worked(5);
					monitor.done();
				}
			}
		}
		
		//If an error was encountered during Maude execution, the info code of the status is set to ERROR_MAUDE, otherwise MAUDE_INFO.
		if(!abort){
			if(failed){
				return new Status(IStatus.OK, PLUGIN_ID , MAUDE_ERROR_MAUDE, output.toString(), null);
			} else{
				if(exec){
					return new Status(IStatus.OK, PLUGIN_ID, MAUDE_INFO, output.toString(), null);
				} else{
					return new Status(IStatus.OK, PLUGIN_ID, MAUDE_OK, output.toString(), null);
				}
			}
		} else{
			monitor.setCanceled(true);
			return new Status(IStatus.INFO, PLUGIN_ID, MAUDE_USER_ABORT, null, null);
		}
	}
		
	@Override
	protected void canceling() {
		abort = true;
		if(process != null){
			process.destroy();
		}
		super.cancel();
	}
	
	/**
	 * Compiles an ABS project into a .maude file. If an ABS file is currently opened in the editor, the project containing this file
	 * will be compiled, otherwise the project currently selected in the project explorer will be compiled.
	 * @throws ParseException Is thrown, if the project which is compiled has parse errors
	 * @throws TypeCheckerException Is thrown, if the project which is compiled has type errors
	 * @throws DeltaModellingException 
	 * @throws WrongProgramArgumentException 
	 * @throws NoModelException 
	 */
	private void compileMaude(IProgressMonitor monitor, AbsNature nature) throws CoreException, IOException, ParseException, TypeCheckerException, WrongProgramArgumentException, DeltaModellingException, NoModelException {
		PrintStream ps = null;
		FileInputStream fis = null;
		try{
			String path = nature.getProjectPreferenceStore().getString(MAUDE_PATH);
			IFolder folder = project.getFolder(path);
			prepareFolder(monitor, folder);
			String fileName = project.getName()+".maude";
			final IFile wspaceFile = folder.getFile(fileName);
			/* generateMaude only knows how to fill PrintStreams */
			final File tmpFile = File.createTempFile(fileName,null);
			ps = new PrintStream(tmpFile);
			
			//Get model, check for errors and throw respective exception
			Model model = nature.getCompleteModel();
			if (model == null)
				throw new NoModelException();
			if(model.hasParserErrors()){
				throw new ParseException(model.getParserErrors());
			}
			if (getProduct() != null) {
				// work on a copy:
                model = model.parseTreeCopy();
				
				model.flattenForProduct(getProduct());
				model.flushCache(); // #335
			}
			if(model.hasTypeErrors()){
				throw new TypeCheckerException(model.typeCheck());
			}

			String mb = getMainBlock();
			if (mb != null && mb.isEmpty())
				mb = null;
            // KLUDGE: use default values for clock limit, resource cost for now
			if(realTime){
                model.generateMaude(ps, MaudeCompiler.SIMULATOR.EQ_TIMED, mb, 100, 0);
			} else{
                model.generateMaude(ps, MaudeCompiler.SIMULATOR.RL, mb, 100, 0);
			}
			ps.close();
			fis = new FileInputStream(tmpFile);
			if (wspaceFile.exists())
				wspaceFile.setContents(fis, true, false, monitor);
			else
				wspaceFile.create(fis, true, monitor);
			fis.close();
			tmpFile.delete();
			destFile = new File(project.getLocation().append(path).toFile(),fileName);
		} finally{
			if (ps != null){
				ps.flush();
				ps.close();
			}
			if (fis != null) {
				fis.close();
			}
		}
	}
	
	/**
	 * Eclipsism for recursively creating folder hierarchy.
	 * @author stolz
	 */
	static void prepareFolder(IProgressMonitor monitor, IFolder folder) throws CoreException{
	  IContainer parent = folder.getParent();
	  if (parent instanceof IFolder)
	    prepareFolder(monitor,(IFolder)parent);
	  if (!folder.exists())
	    folder.create(true,true,monitor);
	}

	/**
	 * Runs configured Maude executable with the earlier generated .maude file in a separate process. Appends output of the process
	 * to output String. Return value depends on which stream of the process was copied. If the error stream contains characters, this
	 * stream will be copied, otherwise the output stream.
	 * @return true if successful (output stream contained data), false else (error stream contained data)
	 */
	private boolean executeMaude(StringBuffer output) throws IOException, InterruptedException{
		PrintWriter pw = null;
		
		try{
		ArrayList<String> args = new ArrayList<String>();
			
			args.add(getMaudePath());
			args.add("-no-banner");
			args.add(getFilePath());
			
			if(!abort) process = Runtime.getRuntime().exec(args.toArray(new String[args.size()]));
			
			//Write Maude command and execute it
			OutputStream os = process.getOutputStream();
			pw = new PrintWriter(os);
			if(partialExec){
				pw.write(getMaudeCommand(steps));	
			} else{
				pw.write(MAUDE_COMMAND);
			}
		} finally{
			if(pw != null){
				pw.close();
			}
		}
		
		return getProcessOutput(process, output);
	}
	
	private String getFilePath(){
		if(Platform.getOS().equals(Platform.OS_WIN32)){
			return convertToCygDrivePath(destFile);
		} else {
			return destFile.getAbsolutePath();
		}
	}
	
	private String getMaudePath() {
		return getDefaultPreferenceStore().getString(MAUDE_EXEC_PATH);
	}

	private String convertToCygDrivePath(File destFile) {
		return "/cygdrive/" + convertToUnixPath(destFile);
	}
	
	private String convertToUnixPath(File destFile) {
		return destFile.getAbsolutePath().replaceFirst(":", "").replace('\\', '/');
	}

	public String getProduct() {
		return product;
	}

	public void setProduct(String product) {
		this.product = product;
	}

	public void setMainBlock(String mainBlock) {
		this.mainBlock = mainBlock;
	}

	public String getMainBlock() {
		return mainBlock;
	}
}
