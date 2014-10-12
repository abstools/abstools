package org.absmodels.abs.plugin.actions;

import static org.absmodels.abs.plugin.util.Constants.ABSUNIT_TEST_ERROR;
import static org.absmodels.abs.plugin.util.Constants.ABSUNIT_TEST_OK;
import static org.absmodels.abs.plugin.util.Constants.ABSUNIT_TEST_USER_ABORT;
import static org.absmodels.abs.plugin.util.Constants.PLUGIN_ID;
import static org.absmodels.abs.plugin.util.UtilityFunctions.getAbsNature;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.PrintStream;

import org.absmodels.abs.plugin.builder.AbsNature;
import org.absmodels.abs.plugin.exceptions.ParseException;
import org.absmodels.abs.plugin.exceptions.TypeCheckerException;
import org.absmodels.abs.plugin.util.Constants;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IncrementalProjectBuilder;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;

import abs.backend.tests.ABSTestRunnerGenerator;
import abs.backend.tests.ASTBasedABSTestRunnerGenerator;
import abs.common.WrongProgramArgumentException;
import abs.frontend.ast.Model;
import abs.frontend.delta.DeltaModellingException;

public abstract class ABSUnitTestExecutionJob extends Job {

	private Process process;
	private boolean abort;
	private File destFile;
	
	protected final IProject project;
	protected final String productName;
	
	protected ABSUnitTestExecutionJob(IProject project, String productName){
		super("Executing ABSUnit tests");
		this.project     = project;
		this.productName = productName;
		setUser(true);
	}
	
	protected IStatus run(IProgressMonitor monitor) {
		AbsNature nature = getAbsNature(project);	
		if(nature == null){
			return new Status(IStatus.INFO, PLUGIN_ID, "Could not compile current selection. Project is not an ABS project.");
		}
		
		monitor.beginTask("ABSUnit Test Execution", 112);
		
		//Parsing and Type Checking ABS model
		monitor.subTask("Generating Test Runner");
		
		try{
			if(!abort) generateTestRunner(monitor,nature);
		} catch(CoreException e1) {
			return new Status(IStatus.ERROR, PLUGIN_ID, "Fatal error while generating test runner", e1);
		} catch (IOException e2) {
			return new Status(IStatus.ERROR, PLUGIN_ID, "Fatal error while generating test runner", e2);
		} catch (ParseException e4) {
			return new Status(IStatus.INFO, PLUGIN_ID, ABSUNIT_TEST_ERROR, "Could not generating test runnern. Code has parse errors.", e4);
		} catch (TypeCheckerException e5) {
			return new Status(IStatus.INFO, PLUGIN_ID, ABSUNIT_TEST_ERROR, "Could not generating test runner. Code has type errors.", e5);
		} 
		monitor.worked(12);
		
		
        try {
            // make sure the generated testrunner file is parsed:
            project.build(IncrementalProjectBuilder.INCREMENTAL_BUILD, null);
        } catch (CoreException e) {
            return new Status(IStatus.ERROR, PLUGIN_ID, "Fatal error while compiling tests", e);
        }
        
		monitor.subTask("Executing ABSUnit tests");
		IStatus status = null;
		try {
			status = executeTest(monitor);
		} catch (CoreException e) {
			return new Status(IStatus.ERROR, PLUGIN_ID, "Fatal error while executing tests", e);
		}
		
		if(abort){
			monitor.setCanceled(true);
			return new Status(IStatus.INFO, PLUGIN_ID, ABSUNIT_TEST_USER_ABORT, null, null);
		}
		
		return new Status(IStatus.OK, PLUGIN_ID, ABSUNIT_TEST_OK, status.getMessage(), null);
	}
	
	protected abstract IStatus executeTest(IProgressMonitor monitor) throws CoreException;

	@Override
	protected void canceling() {
		abort = true;
		if(process != null){
			process.destroy();
		}
		super.cancel();
	}
	
	/**
	 * Generate an ABS file from an ABS project that can be used to execute unit tests
	 * @throws ParseException Is thrown, if the project which is compiled has parse errors
	 * @throws TypeCheckerException Is thrown, if the project which is compiled has type errors
	 * @throws DeltaModellingException 
	 * @throws WrongProgramArgumentException 
	 */
	private void generateTestRunner(IProgressMonitor monitor, AbsNature nature) 
	throws 	CoreException, 
			IOException, 
			ParseException, 
			TypeCheckerException {
		
		PrintStream ps = null;
		FileInputStream fis = null;
		try{
			
			String path = Constants.DEFAULT_MAVEN_TARGET_ABS_PATH; //find a way to change
			IFolder folder = project.getFolder(path);
			MaudeJob.prepareFolder(monitor, folder);
			String fileName = "runner.abs";
			final IFile wspaceFile = folder.getFile(fileName);
			/* generateMaude only knows how to fill PrintStreams */
			final File tmpFile = File.createTempFile(fileName,null);
			ps = new PrintStream(tmpFile);
			
			//Get model, check for errors and throw respective exception
			Model model = nature.getCompleteModel();
			if(model.hasParserErrors()){
				throw new ParseException(model.getParserErrors());
			}
			if(model.hasTypeErrors()){
				throw new TypeCheckerException(model.typeCheck());
			}

			String mb = abs.backend.tests.ABSTestRunnerGenerator.RUNNER_MAIN;
			ABSTestRunnerGenerator gen = new ASTBasedABSTestRunnerGenerator(model);
			gen.generateTestRunner(ps);
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

}
