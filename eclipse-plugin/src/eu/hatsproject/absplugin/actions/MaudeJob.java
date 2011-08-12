/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package eu.hatsproject.absplugin.actions;

import static eu.hatsproject.absplugin.util.Constants.ABSFRONTEND_PLUGIN_ID;
import static eu.hatsproject.absplugin.util.Constants.BACKEND_MAUDE_INTERPRETER;
import static eu.hatsproject.absplugin.util.Constants.MAUDE_COMMAND;
import static eu.hatsproject.absplugin.util.Constants.MAUDE_ERROR;
import static eu.hatsproject.absplugin.util.Constants.MAUDE_ERROR_MAUDE;
import static eu.hatsproject.absplugin.util.Constants.MAUDE_ERROR_MAUDE_PATH;
import static eu.hatsproject.absplugin.util.Constants.MAUDE_EXEC_PATH;
import static eu.hatsproject.absplugin.util.Constants.MAUDE_INFO;
import static eu.hatsproject.absplugin.util.Constants.MAUDE_PATH;
import static eu.hatsproject.absplugin.util.Constants.MAUDE_USER_ABORT;
import static eu.hatsproject.absplugin.util.Constants.*;
import static eu.hatsproject.absplugin.util.UtilityFunctions.copyFile;
import static eu.hatsproject.absplugin.util.UtilityFunctions.getAbsNature;
import static eu.hatsproject.absplugin.util.UtilityFunctions.getDefaultPreferenceStore;
import static eu.hatsproject.absplugin.util.UtilityFunctions.getMaudeCommand;
import static eu.hatsproject.absplugin.util.UtilityFunctions.getProcessOutput;

import java.io.File;
import java.io.IOException;
import java.io.OutputStream;
import java.io.PrintStream;
import java.io.PrintWriter;
import java.util.ArrayList;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.FileLocator;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;

import abs.common.WrongProgramArgumentException;
import abs.frontend.ast.Model;
import abs.frontend.delta.exceptions.ASTNodeNotFoundException;
import eu.hatsproject.absplugin.builder.AbsNature;
import eu.hatsproject.absplugin.exceptions.NoABSNatureException;
import eu.hatsproject.absplugin.exceptions.ParseException;
import eu.hatsproject.absplugin.exceptions.TypeCheckerException;

public class MaudeJob extends Job{
	private Process process;
	private IProject project;
	private File destFolder;
	private File destFile;
	private boolean exec;
	private boolean realTime;
	private boolean abort;
	
	private boolean partialExec;
	private int steps;
	private String product;
	
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
		abort = false;
		boolean failed = false;
		StringBuffer output = new StringBuffer();
		
		//Set title and totalWork of the process according to clicked button
		if(exec){
			monitor.beginTask("Executing Maude", 11);
		} else{
			monitor.beginTask("Compiling Maude", 6);
		}
		
		//Compile Maude Code
		monitor.subTask("Compiling ABS model to Maude");
		try{
			if(!abort) compileMaude();
		} catch(CoreException e1) {
			return new Status(IStatus.INFO, PLUGIN_ID, MAUDE_ERROR, "Fatal error while compilig", e1);
		} catch (IOException e2) {
			return new Status(IStatus.INFO, PLUGIN_ID, MAUDE_ERROR, "Fatal error while compilig", e2);
		} catch (NoABSNatureException e3) {
			return new Status(IStatus.INFO, PLUGIN_ID, MAUDE_ERROR, "Could not compile current selection. Project is not an ABS project", e3);
		} catch (ParseException e4) {
			return new Status(IStatus.INFO, PLUGIN_ID, MAUDE_ERROR, "Could not compile current selection. Code has parse errors", e4);
		} catch (TypeCheckerException e5) {
			return new Status(IStatus.INFO, PLUGIN_ID, MAUDE_ERROR, "Could not compile current selection. Code has type errors", e5);
		} catch (WrongProgramArgumentException e) {
			return new Status(IStatus.ERROR, PLUGIN_ID, MAUDE_ERROR, "Could not compile current selection.", e);
		} catch (ASTNodeNotFoundException e) {
			return new Status(IStatus.ERROR, PLUGIN_ID, MAUDE_ERROR, "Could not compile current selection.", e);
		} 
		monitor.worked(5);
		
		//Copy standard ABS-interpreter into gen folder
		monitor.subTask("Copying Maude Interpreter");
		try{	
			if(!abort) copyMaudeInterpreter();
		} catch(CoreException e1){
			e1.printStackTrace();
			return new Status(IStatus.INFO, PLUGIN_ID, MAUDE_ERROR, "Fatal error while copying Maude Interpreter", e1);
		} catch (IOException e2) {
			e2.printStackTrace();
			return new Status(IStatus.INFO, PLUGIN_ID, MAUDE_ERROR, "Fatal error while copying Maude Interpreter", e2);
		}
		monitor.worked(1);
		
		//Execute generated Maude code
		monitor.subTask("Executing generated Maude code");
		if(exec){
			try{
				if(!abort) failed = !executeMaude(output);
			} catch (IOException e1) {
				return new Status(IStatus.INFO, PLUGIN_ID, MAUDE_ERROR_MAUDE_PATH, "Encountered IOException while executing Maude (probably misconfigured location of maude executable)", e1);
			} catch (InterruptedException e2) {
				return new Status(IStatus.INFO, PLUGIN_ID, MAUDE_ERROR, "Fatal error while executing Maude", e2);
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
	 * @returns true, if successful, false else
	 * @throws NoABSNatureException Is thrown, if a the project which is compiled is not an ABS project
	 * @throws ParseException Is thrown, if the project which is compiled has parse errors
	 * @throws TypeCheckerException Is thrown, if the project which is compiled has type errors
	 * @throws ASTNodeNotFoundException 
	 * @throws WrongProgramArgumentException 
	 */
	private void compileMaude() throws CoreException, IOException, NoABSNatureException, ParseException, TypeCheckerException, WrongProgramArgumentException, ASTNodeNotFoundException {
		PrintStream ps = null;
		//Check if project is an ABSProject
		try{
			AbsNature nature = getAbsNature(project);
		
			if(nature == null){
				throw new NoABSNatureException();
			}
			
			String path = nature.getProjectPreferenceStore().getString(MAUDE_PATH);
			destFolder  = project.getLocation().append(path).toFile();
			destFile    = new File(destFolder, project.getName() + ".maude");
			
			if(!destFile.exists()){
				destFolder.mkdirs();
				destFile.createNewFile();
			}
			
			ps = new PrintStream(destFile);
			
			//Get model, check for errors and throw respective exception
			Model model = nature.getCompleteModel();
			if(model.hasParserErrors()){
				throw new ParseException(model.getParserErrors());
			}
			if (getProduct() != null) {
				model = model.copy(); // FIXME: fullCopy() necessary?
				model.flattenForProduct(getProduct());
			}
			if(model.hasTypeErrors()){
				throw new TypeCheckerException(model.typeCheck());
			}
			
			// FIXME: provide a dialog to choose the main block
			if(realTime){
                            model.generateMaude(ps, "ABS-SIMULATOR-EQ-TIMED", null);
			} else{
                            model.generateMaude(ps, "ABS-SIMULATOR-RL", null);
			}
		} finally{
			if (ps != null){
				ps.flush();
				ps.close();
			}
		}
	}
	
	/**
	 * Copies the default abs-interpreter.maude, which is necessary for all Maude executions, from the plugin to the Maude folder
	 * of this project.
	 */
	private void copyMaudeInterpreter() throws CoreException, IOException{
		File bundle = FileLocator.getBundleFile(Platform.getBundle(ABSFRONTEND_PLUGIN_ID));

		File src = new File(bundle, BACKEND_MAUDE_INTERPRETER);
		if (!src.exists()) {
			src = new File(bundle, "src/"+BACKEND_MAUDE_INTERPRETER);
		}
		copyFile(src, new File(destFolder, "abs-interpreter.maude"));
		
		project.refreshLocal(IProject.DEPTH_INFINITE, new NullProgressMonitor());
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
}