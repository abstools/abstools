/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package eu.hatsproject.absplugin.actions;

import static eu.hatsproject.absplugin.util.Constants.ABSFRONTEND_PLUGIN_ID;
import static eu.hatsproject.absplugin.util.Constants.ACTION_DEBUG_ID;
import static eu.hatsproject.absplugin.util.Constants.ACTION_START_SDE;
import static eu.hatsproject.absplugin.util.Constants.ALWAYS_COMPILE;
import static eu.hatsproject.absplugin.util.Constants.DEBUGGER_ARGS_OTHER_DEFAULT;
import static eu.hatsproject.absplugin.util.Constants.DO_DEBUG;
import static eu.hatsproject.absplugin.util.Constants.JAVA_SOURCE_PATH;
import static eu.hatsproject.absplugin.util.Constants.NATURE_ID;
import static eu.hatsproject.absplugin.util.Constants.NO_WARNINGS;
import static eu.hatsproject.absplugin.util.Constants.PLUGIN_ID;
import static eu.hatsproject.absplugin.util.Constants.SDEDIT_PLUGIN_ID;
import static eu.hatsproject.absplugin.util.Constants.SOURCE_ONLY;
import static eu.hatsproject.absplugin.util.UtilityFunctions.getAbsNature;
import static eu.hatsproject.absplugin.util.UtilityFunctions.standardExceptionHandling;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FilenameFilter;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.io.PrintWriter;
import java.net.InetSocketAddress;
import java.net.Socket;
import java.util.ArrayList;
import java.util.LinkedList;
import java.util.List;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.Assert;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.FileLocator;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.jdt.core.compiler.CompilationProgress;
import org.eclipse.jdt.core.compiler.batch.BatchCompiler;
import org.eclipse.jface.action.IAction;
import org.eclipse.swt.widgets.Display;
import org.eclipse.ui.PartInitException;
import org.osgi.framework.Bundle;

import abs.backend.java.JavaBackend;
import abs.backend.java.codegeneration.JavaCode;
import abs.backend.java.codegeneration.JavaCodeGenerationException;
import abs.backend.tests.ABSTestRunnerGenerator;
import abs.common.WrongProgramArgumentException;
import abs.frontend.ast.CompilationUnit;
import abs.frontend.ast.Model;
import abs.frontend.ast.ModuleDecl;
import abs.frontend.ast.Product;
import abs.frontend.delta.exceptions.ASTNodeNotFoundException;
import eu.hatsproject.absplugin.actions.runconfig.RunConfigEnums.DebuggerObserver;
import eu.hatsproject.absplugin.actions.runconfig.RunConfigEnums.DebuggerScheduler;
import eu.hatsproject.absplugin.builder.AbsNature;
import eu.hatsproject.absplugin.console.ConsoleManager;
import eu.hatsproject.absplugin.console.MsgConsole;
import eu.hatsproject.absplugin.debug.model.Debugger;
import eu.hatsproject.absplugin.debug.model.Debugger.InvalidRandomSeedException;
import eu.hatsproject.absplugin.exceptions.AbsJobException;
import eu.hatsproject.absplugin.util.UtilityFunctions;

public class JavaJob extends Job {
	// debug this class -> enables printlns
	private static boolean debugMode = DO_DEBUG;

	private MsgConsole javaConsole;
	private IProject project;
	private IProgressMonitor refreshProgressMonitor;
	private IFile currentFile;
	private Product product = null;

	// job
	private IAction action;
	private Path javaPath;
	private boolean compileCode;
	private boolean debugCode;
	private boolean sourceOnly;
	private boolean noWarnings;
	private boolean startSDE;
	
	// canceling job
	private Process debugProcess = null;
	private boolean isCanceled = false;

	// used by run config
	private String debuggerArgsOther;
	private String debuggerArgsSystemObserver;
	private String debuggerArgsTotalScheduler;
	private String debuggerCompileFirst;
	private String debuggerArgsRandomSeed;
	private boolean useInternalDebugger;
	private boolean debuggerIsInDebugMode;

   private SDEditProcess sdeditProcess;

   private List<String> extraClassPaths;

   private final boolean absUnit;

   public static final String COMPILE_JOB = "ABS Java Code Generation";
   public static final String RUN_JOB = "ABS Java Execution";
	
   public JavaJob(String name, IAction action, IProject project, IFile file) {
	   this(name,action,project,file,false);
   }
   
	/**
	 * creates a new job for an action. 
	 * 
	 * @param name - job name
	 * @param action - compile, debug or SDedit action
	 * @param project - ABS file
	 * @param file - current opened file
	 */
	public JavaJob(String name, IAction action, IProject project, IFile file, boolean absUnit) {
		super(name);
		this.action = action;
		this.project = project;
		this.currentFile = file;
		this.setUser(true);
		useInternalDebugger = true;
		debuggerIsInDebugMode = true;
		this.absUnit = absUnit;
	}
	
	@Override
	protected IStatus run(IProgressMonitor monitor) {
		monitor.beginTask("ABS Java Backend", 100);
		return runJob(monitor);
	}
	
	public IStatus runJob(IProgressMonitor monitor) {
		try{
			//must be an ABS project
			if (project == null || !project.exists()){
				return showErrorMessage("The project does not exist.");
			} else if (!project.hasNature(NATURE_ID)){
				return showErrorMessage("Chosen project '"+project.getName()+"' is not an ABS project.");
			}			
		} catch (CoreException e) {
			return showErrorMessage("Project does not exist or project is not open", e);
		}
	
		monitor.worked(2);
		
		try {
			loadPropertyOptions();
			monitor.worked(14);
		} catch (AbsJobException e) {
			return showErrorMessage(e.getLocalizedMessage());
		}
		
		return executeJob(monitor); 
	}

	private void setUpConsole() {
		javaConsole = getAbsNature(project).getJavaConsole();
		javaConsole.clear();
	}
	
	/**
	 * Init property options
	 * 
	 * @see eu.hatsproject.absplugin.properties.JavaPropertyPage
	 * 
	 * @throws AbsJobException, if project is not an ABS project
	 */
	private void loadPropertyOptions() throws AbsJobException {
		javaPath    = getPathToGeneratedJavaFiles(project);
		sourceOnly  = getAbsNature(project).getProjectPreferenceStore().getBoolean(SOURCE_ONLY);
		noWarnings  = getAbsNature(project).getProjectPreferenceStore().getBoolean(NO_WARNINGS);
		compileCode = getAbsNature(project).getProjectPreferenceStore().getBoolean(ALWAYS_COMPILE);
		if (debuggerCompileFirst != null && debuggerCompileFirst.length() > 0){
			compileCode = Boolean.valueOf(debuggerCompileFirst);
		}
	}

	private IStatus executeJob(IProgressMonitor monitor){
		
		setJobOptions();
		setUpConsole();
		monitor.worked(14);

		try {
			String absFrontendLocation = "";
			if (!isCanceled)
				absFrontendLocation = getAbsFrontendLocation();
			monitor.worked(40);
			
			if (compileCode && !isCanceled) {
				executeCompiler(absFrontendLocation);
			}
			monitor.worked(12);

			if (debugCode && !isCanceled) {
				executeJavaMain(absFrontendLocation);

			}
			monitor.worked(12);

			// refresh project tree
			project.refreshLocal(IProject.DEPTH_INFINITE, refreshProgressMonitor);

			return createStatus();

		} catch (AbsJobException e) {
			return showErrorMessage(e.getLocalizedMessage());
		} catch (JavaCodeGenerationException e) {
		    return showErrorMessage(e.getLocalizedMessage());
		} catch (Exception e) {
			//do not kill the plug-in, if something goes wrong
			return showErrorMessage("Fatal error!", e);
		}
	}

	/**
	 * The action id decides which options are used.
	 * Options:
	 * only compile | start debugger | start debugger with SDEdit  
	 */
	private void setJobOptions() {
		if(action.getId().equals(ACTION_DEBUG_ID)){
			debugCode = true;
			startSDE = false;
		} else if (action.getId().equals(ACTION_START_SDE)){
			startSDE = true;
			debugCode = true;
		}else {
			debugCode = false;
			compileCode = true;
			startSDE = false;
		}
	}

	private IStatus createStatus() {
		if (isCanceled) {
			if (debugMode)
				System.out.println("canceled");
			return new Status(Status.CANCEL, PLUGIN_ID, "Job canceled");
		} else {
			if (debugMode)
				System.out.println("end");
			return new Status(Status.OK, PLUGIN_ID, "done");
		}
	}
	
	private void executeCompiler(String absFrontendLocation) throws AbsJobException,
			CoreException, IOException, JavaCodeGenerationException {
		// generate .java files
		generateJavaCode(javaPath, project);
	
		// generate class files
		if (!sourceOnly && !isCanceled) {
			generateJavaClassFiles(absFrontendLocation, javaPath.toFile(),
					noWarnings);
		}
	}

	/**
	 * Generates .java files (no .class files).
	 * If 'product' is set, will flatten accordingly.
	 * 
	 * @param path - where to add the modules / java-files
	 * @param project - the ABS project
	 * @throws IOException, if unable to create java files 
	 * @throws AbsJobException, if unable to generate java files 
	 * @throws JavaCodeGenerationException, if unable to generate java files  
	 */
	private void generateJavaCode(Path path, IProject project) throws AbsJobException, IOException, JavaCodeGenerationException {
		Model model = getModelFromProject(project);
		if (debugMode) System.out.println("Creating java source files");
	    JavaCode javaCode = new JavaCode(path.toFile());
	    if (product != null) {
	    	/* [stolz] Flattening for a product will mangle the model according to [ramus], so we get our own copy,
	    	 * since the nature will hold on to the original model.
	    	 * FIXME: Model.fullCopy will trigger an exception, so we have to see what we do about that.
	    	 * For safety, we also pass the product by name.
	    	 */
	    	String productN = product.getModuleDecl().getName()+"."+product.getName();
	    	model = model.copy();
			try {
				model.flattenForProduct(productN);
			} catch (WrongProgramArgumentException e) {
				throw new AbsJobException(e);
			} catch (ASTNodeNotFoundException e) {
				throw new AbsJobException(e);
			}
	    }
	    model.generateJavaCode(javaCode);
	    
	    int countUnits=model.getNumCompilationUnit();
	    if(countUnits==0) throw new AbsJobException("No compilation unit found");
	}

	/**
	 * generates .class files (needs .java files)
	 * 
	 * @param absFrontendLocation -where to find the absfrontend
	 * @param path - directory with java files
	 * @param noWarnings - do not show any compile warnings
	 * @throws AbsJobException 
	 */
	private void generateJavaClassFiles(String absFrontendLocation, File path, boolean noWarnings) throws AbsJobException {
		if (debugMode) System.out.println("Creating class files");
		
		//args
		String noWarn;
		if(noWarnings){
			noWarn = "-nowarn";
		}else{
			noWarn = "";
		}
		if(!path.isDirectory() || !path.isAbsolute()){
			if (debugMode) System.err.println("Not a absolute path of a directory: "+path.getAbsolutePath());
			throw new AbsJobException("Path is not an absolute path of a directory");
		}
		String args = "-1.5 "+noWarn+" -classpath "+"\""+absFrontendLocation+"\""+" "+"\""+path.getAbsolutePath()+"\"";
		if (debugMode) System.out.println("arguments: "+args);
	
		//console
		try {
			ConsoleManager.displayConsoleView();
		} catch (PartInitException e) {
			standardExceptionHandling(e);
			throw new AbsJobException("Not able to show console");
		}
	
		//compile with jdt-BatchCompiler
		CompilationProgress progress = null;
		OutputStream os = javaConsole.getOutputStream(ConsoleManager.MessageType.MESSAGE_ERROR);
		boolean compilationSuccessful = BatchCompiler.compile(
				args,
				new PrintWriter(os),
				new PrintWriter(os),
				progress);
		if (!compilationSuccessful) {
		    throw new AbsJobException("Sorry, there seems to be a bug in the java backend. The generated java " +
		    		"files could not be compiled correctly.");
		}
	}

	private void executeJavaMain(String absFrontendLocation) throws AbsJobException,
			IOException, CoreException {
		checkPath(javaPath);
		if (startSDE && !isCanceled) {
		   startSDEdit();
		}
		if (!isCanceled)
			findAndExecuteMain(absFrontendLocation);
	}

   private void startSDEdit() throws IOException {
      if (sdeditProcess == null)
         sdeditProcess = new SDEditProcess();
      sdeditProcess.start();
   }

	private void checkPath(Path javaPath) throws AbsJobException {
		File javaDir = javaPath.toFile();
		File[] genDirs = javaDir.listFiles();
		if(genDirs == null || genDirs.length == 0){
			throw new AbsJobException("No generated Java files found. Please compile ABS files to Java before debugging.");
		}
	}

	/**
	 * Tries to find the main block in the chosen ABS file or in the 
	 * ABS project, if file is null.
	 * 
	 * Finally: executes Modulename.Main
	 * 
	 * @param absFrontendLocation - where to find the absfrontend
	 * @throws AbsJobException - if no main block found
	 * @throws IOException - If an I/O error occurs 
	 */
	private void findAndExecuteMain(String absFrontendLocation) throws AbsJobException, IOException {
		String info = null;
		ModuleDecl module;
		if (absUnit) {
		    module = getModuleByName(ABSTestRunnerGenerator.RUNNER_MAIN);
		} else {
			module = searchForMainBlockInCurrentFile();

			//Search for the main file if previous searching was not successful
			if(module == null){
			    List<ModuleDecl> modules = getAllModulesWithMainInCurrentProject();
				
				if(modules.size() == 0){
					throw new AbsJobException("No Module with main block found.");
				} else {
				    module = modules.get(0);
				}
				if(currentFile != null){
					info = "No main file found for \""+currentFile.getName()+"\".\nBut there is a main file in the project:";
				}
			}
			
		}
		String moduleName = JavaBackend.getFullJavaNameForMainBlock(module);
		try {
			debugAbsFiles(absFrontendLocation, javaPath, startSDE, moduleName, info);
		} catch (InvalidRandomSeedException e) {
			throw new AbsJobException(e);
		}
	}

   
    /**
     * finds a module with a given name in the current
     * @param moduleName
     * @return
     * @throws AbsJobException
     */
	private ModuleDecl getModuleByName(String moduleName) throws AbsJobException {
	    AbsNature nature = UtilityFunctions.getAbsNature(project);
	    if(nature == null){
	        throw new AbsJobException("Could not start the debugger, because selected file (" + currentFile.getName() +  ") is not in an ABS project!");
	    }
	    synchronized (nature.modelLock) {
	        Model model = nature.getCompleteModel();
	        ModuleDecl result = model.lookupModule(moduleName);
	        if (result == null) {
	            throw new AbsJobException("Could not find a module with name " + moduleName + ".");
	        }
            return result ;
	    }
	}

   private List<ModuleDecl> getAllModulesWithMainInCurrentProject() throws AbsJobException {
       AbsNature nature = UtilityFunctions.getAbsNature(project);
       if(nature == null){
           throw new AbsJobException("Could not start the debugger, because selected file (" + currentFile.getName() +  ") is not in an ABS project!");
       }
       synchronized (nature.modelLock) {
           Model model = nature.getCompleteModel();
           return getAllModulesWithMain(model);
       }
   }
   
   /**
    * searches a model for modules with a main block
    * @param model
    * @return a list of all modules which have a main block
    */
   private List<ModuleDecl> getAllModulesWithMain(Model model) {
       List<ModuleDecl> result = new LinkedList<ModuleDecl>();
       for (CompilationUnit cu : model.getCompilationUnits()) {
           result.addAll(findModulesWithMain(cu));
       }
       return result;
   }
	

   /**
    * 
    * @return a module with a main block from the current file or null if no such module was found
    * @throws AbsJobException
    */
    private ModuleDecl searchForMainBlockInCurrentFile() throws AbsJobException{
        if (currentFile == null) {
            return null;
        }
		AbsNature nature = UtilityFunctions.getAbsNature(project);
		if(nature == null){
			throw new AbsJobException("Could not start the debugger, because selected file (" + currentFile.getName() +  ") is not in an ABS project!");
		}
		synchronized (nature.modelLock) {
			CompilationUnit unit = nature.getCompilationUnit(currentFile);
			List<ModuleDecl> modules = findModulesWithMain(unit);
			if (modules.size() == 0) {
			    return null;
			} else {
			    return modules.get(0);
			}
		}
	}

    
    
    
    /**
     * searches a compilation unit for modules with a main block
     * @param unit
     * @return a list of all modules which have a main block
     */
    private List<ModuleDecl> findModulesWithMain(CompilationUnit unit) {
        List<ModuleDecl> result = new LinkedList<ModuleDecl>();
        if(unit != null){
        	int countModules = unit.getNumModuleDecl();
        	int i = 0;
        	while(i<countModules){
        		ModuleDecl module = unit.getModuleDecl(i);
        		if (module.hasBlock()) {
        		    result.add(module);
        		}
        		//go to next module
        		i++;
        	}
        }
        return result;
    }
	
	


	private void debugAbsFiles(String absFrontendLocation, final Path javaPath,
			boolean useBothObserver, final String moduleName, String info)
			throws IOException, InvalidRandomSeedException {
		setDebuggerArgumentsIfNull(useBothObserver);

		if(useInternalDebugger){
			executeABSSystem(javaPath, moduleName);	
		} else {
	      executeABSSystemInExternalProcess(absFrontendLocation, javaPath, moduleName, info);
		}
	}

   private void executeABSSystemInExternalProcess(String absFrontendLocation, final Path javaPath,
         final String moduleName, String info) throws IOException {
      final ArrayList<String> args = new ArrayList<String>();

      args.add("java");
      args.add("-cp");
      args.add(absFrontendLocation+File.pathSeparator+javaPath+getExtraClassPathString());
      
      if(debuggerIsInDebugMode){
         addIfNotNullOrEmpty(args,"-Dabs.debug=true");
      }
      addIfNotNullOrEmpty(args,debuggerArgsOther);
      addIfNotNullOrEmpty(args,debuggerArgsSystemObserver);
      
      addIfNotNullOrEmpty(args,debuggerArgsTotalScheduler);
      addIfNotNullOrEmpty(args,debuggerArgsRandomSeed);

      args.add(moduleName);

      if (debugMode) System.out.println("run java code with: "+args);
      debugProcess = Runtime.getRuntime().exec(args.toArray(new String[args.size()]));
      printProcessOutput(moduleName, info, debugProcess);
   }
	
	private String getExtraClassPathString() {
	   StringBuilder sb = new StringBuilder();
	   for (String p : extraClassPaths) {
	      sb.append(File.pathSeparator+p);
	   }
      return sb.toString();
   }

   /**
	 * The following arguments may be null, if the user selected one of the tool bar icons.
	 * Otherwise these arguments should be set, before using JavaJob.schedule(). 
	 */
	private void setDebuggerArgumentsIfNull(boolean startSDE) {
		if (debuggerArgsSystemObserver == null){
			if(useInternalDebugger){
				debuggerArgsSystemObserver = DebuggerObserver.ECLIPSE.getCommand();
			} else {
				debuggerArgsSystemObserver = DebuggerObserver.GRAPHICAL.getCommand();
			}
			
			if(startSDE){
				debuggerArgsSystemObserver += ","+DebuggerObserver.UML.getClassName();
			}
		}
	
		if (debuggerArgsTotalScheduler == null) {
			if(useInternalDebugger){
				debuggerArgsTotalScheduler = DebuggerScheduler.ECLIPSE.getCommand();
			} else {
				debuggerArgsTotalScheduler = DebuggerScheduler.INTERACTIVE.getCommand();
			}
		}
		
		if(debuggerArgsOther == null){
			debuggerArgsOther = DEBUGGER_ARGS_OTHER_DEFAULT;
		}
	}

	private void addIfNotNullOrEmpty(ArrayList<String> args,
			String s) {
		if (s != null && !s.isEmpty())
			args.add(s);
	}

	private void executeABSSystem(final Path javaPath,
			final String moduleName) throws InvalidRandomSeedException {
		Debugger.startABSRuntime(project.getName(), moduleName, javaPath,
				debuggerArgsSystemObserver, debuggerArgsTotalScheduler,
				debuggerIsInDebugMode, debuggerArgsRandomSeed);
	}

	/**
	 * prints the output on users console using a new thread
	 * 
	 * @param moduleName
	 * @param info, String nam
	 * @param p
	 * @throws IOException
	 */
	private void printProcessOutput(final String moduleName, final String info, final Process p) throws IOException {

		final Display display = Display.getDefault();

		// run in a different thread to prevent blocking of the SWT UI thread
		Runnable r = new Runnable() {
			@Override
			public void run() {
				try {
					if(info != null) javaConsole.println(info, ConsoleManager.MessageType.MESSAGE_INFO);
					javaConsole.println("run "+moduleName+"..", ConsoleManager.MessageType.MESSAGE_INFO);

					//Output
					BufferedReader stdInput = new BufferedReader(new InputStreamReader(p.getInputStream()));
					BufferedReader stdError = new BufferedReader(new InputStreamReader(p.getErrorStream()));

					// read the output from the command
					String s=null;
					if (debugMode) System.out.println("Here is the standard output of the command:");
					while ((s = stdInput.readLine()) != null) {
						asyncShowString(display, javaConsole, s, ConsoleManager.MessageType.MESSAGE_INFO);
				
						if (debugMode) System.out.println(s);
					}

					// read any errors from the attempted command
					if (debugMode) System.out.println("Here is the standard error of the command (if any):");
					while ((s = stdError.readLine()) != null) {
						asyncShowString(display, javaConsole, s, ConsoleManager.MessageType.MESSAGE_ERROR);
						if (debugMode) System.err.println(s);
					}

					stdInput.close();
					stdError.close();

					asyncShowString(display, javaConsole, "..finished", ConsoleManager.MessageType.MESSAGE_INFO);
				} catch (IOException e) {
					standardExceptionHandling(e);
				}
			}

			private void asyncShowString(final Display display,final MsgConsole console, final String s, final ConsoleManager.MessageType mtype) {
				display.asyncExec(new Runnable() {
					@Override
					public void run() {
						console.println(s, mtype);
					}});
			}};
	
		new Thread(r).start();	
	}

	public static Model getModelFromProject(IProject project) throws AbsJobException {
		AbsNature nature = UtilityFunctions.getAbsNature(project);
		if(nature == null){
			throw new AbsJobException("The file is not in an ABS project!");
		}
		Model model = nature.getCompleteModel();
		//if (model==null && curFile!=null ) model = abs.frontend.parser.Main.parse(new File(curFile.getLocation().toString()), withStdLib);
		if (model == null){
			throw new AbsJobException("No ABS model found");
		}
		if (model.hasTypeErrors() || model.hasParserErrors()){
			// just to be sure
			throw new AbsJobException("An ABS file in the project has type or parser errors");
		}
		return model;
	}
	
	
	private String getAbsFrontendLocation() throws IOException,FileNotFoundException {
		String absFrontendLocation;
		File absFrontend = FileLocator.getBundleFile(Platform.getBundle(ABSFRONTEND_PLUGIN_ID));
		File binFrontend = new File(absFrontend, "bin");
		if(binFrontend.exists()){
			absFrontendLocation = binFrontend.getAbsolutePath();
		} else {
			absFrontendLocation = absFrontend.getAbsolutePath();
		}
		
		return absFrontendLocation;
	}
	

	/**
	 * Returns path of generated Java files. 
	 * Throws an IllegalArumentException, if path can not be found for the given project.
	 * 
	 * @param project
	 * @return
	 * @throws CoreException
	 * @throws AbsJobException
	 */
	private Path getPathToGeneratedJavaFiles(IProject project) throws AbsJobException {
		Path path;
		String tempPath;
		try{
			tempPath = getAbsNature(project).getProjectPreferenceStore().getString(JAVA_SOURCE_PATH);
			Assert.isLegal(tempPath != null);
		} catch (NullPointerException e) {
			standardExceptionHandling(e);
			throw new AbsJobException("No ABS project selected.");
		} 
	
		path =  new Path(tempPath);
		if (!path.isAbsolute()){
			path = new Path(project.getLocation().append(path).toOSString());
		} 

		return path;
	}
	
	class SDEditProcess {
	   
	   private Process process;
	   private final int PORT = 60001;
      private SDEditWatchdog sdeWatchdogJob;
      private MsgConsole console;

      /**
	    * starts a new SDedit process 
	    * 
	    * @param PORT
	    * @throws IOException
	    */
	   private void start() throws IOException{
	      if (sdeditIsRunning())
	         return;

         console = getAbsNature(project).getSDEditConsole();
         console.clear();

         Process p = startProcess();

	      if (sdeWatchdogJob != null) {
	         sdeWatchdogJob.cancel();
	      }

	      sdeWatchdogJob = 
	         new SDEditWatchdog(this);

	      sdeWatchdogJob.schedule();
	   }
	   
	   public Process getProcess() {
	      return process;
	   }
	   
	   public MsgConsole getConsole() {
	      return console;
	   }

	   private Process startProcess() throws IOException {
	      Bundle seditbundle = Platform.getBundle(SDEDIT_PLUGIN_ID);
	      File jarFile = FileLocator.getBundleFile(seditbundle).getAbsoluteFile();

	      ArrayList<String> args = new ArrayList<String>();

	      /* Directly run on the extracted Eclipse plugin:
	       * classes are in bin, and dependencies in lib.
	       * No need to build SDEdit.jar!
	       */
	      args.add("java");
	      args.add("-cp");
	      StringBuffer cps = new StringBuffer();
	      cps.append("bin/");
	      File lib = new File(jarFile,"lib");
	      assert lib.exists();
	      for (String f : lib.list(new FilenameFilter() {

			@Override
			public boolean accept(File dir, String name) {
				return name.endsWith(".jar");
			}})) {
	    	  cps.append(File.pathSeparator + "lib/"+f);
	      }
	      args.add(cps.toString());
	      args.add("net.sf.sdedit.Main");
	      args.add("-s");
	      args.add(String.valueOf(PORT));
	      
         ProcessBuilder pb = new ProcessBuilder(args);
         pb.directory(jarFile); /* Shorter classpaths */
         process = pb.start();
         return process;
	   }

	   private boolean sdeditIsRunning() {
	      try {
	         Socket s = new Socket();
	         s.connect(new InetSocketAddress("localhost", PORT));
	         s.close();
	         if (debugMode)
	            System.out.println("SDEdit server is already running");
	         return true;
	      } catch (IOException e) {
	         return false;
	      }
	   }

      public void cancel() {
         if (process != null)
            process.destroy();
         
         sdeWatchdogJob.cancel();
      }

   }

	@Override
	protected void canceling() {
		isCanceled = true;
		if(debugProcess != null) debugProcess.destroy();
		if(sdeditProcess != null) sdeditProcess.cancel();
		super.canceling();
	}

	private IStatus showErrorMessage(String errorMessage){
		if (debugMode) System.err.println(errorMessage);
		return new Status(Status.ERROR, PLUGIN_ID, errorMessage);
	}

	private IStatus showErrorMessage(String errorMessage, Exception e){
		standardExceptionHandling(e);
		return new Status(Status.ERROR, PLUGIN_ID, errorMessage, e);
	}

	public void setDebuggerArgsOther(String debuggerArgsOther) {
		this.debuggerArgsOther = debuggerArgsOther;
	}

	/**
	 * Set method
	 * @param debuggerArgsSystemObserver - starting with "-Dabs.systemobserver="
	 */
	public void setDebuggerArgsSystemObserver(String debuggerArgsSystemObserver) {
		this.debuggerArgsSystemObserver = debuggerArgsSystemObserver;
	}

	/**
	 * Set method
	 * @param debuggerArgsTotalScheduler - starting with "-Dabs.totalscheduler="
	 */
	public void setDebuggerArgsTotalScheduler(String debuggerArgsTotalScheduler) {
		this.debuggerArgsTotalScheduler = debuggerArgsTotalScheduler;
	}
	
	/**
	 * Set method
	 * @param debuggerRandomSeed - starting with ""-Dabs.randomseed=""
	 */
	public void setArgsDebuggerRandomSeed(String debuggerRandomSeed) {
		this.debuggerArgsRandomSeed = debuggerRandomSeed;
	}
	
	public void setDebuggerCompileFirst(boolean debuggerCompileFirst) {
		this.debuggerCompileFirst = String.valueOf(debuggerCompileFirst);
	}
	
	public void setInternalDebugger(boolean b) {
		this.useInternalDebugger = b;
	}
	
	public void setDebuggerDebugMode(boolean b) {
		this.debuggerIsInDebugMode = b;
	}

	public void setExtraClassPaths(List<String> extraClassPaths) {
		this.extraClassPaths = extraClassPaths;
	}

	public Product getProduct() {
		return product;
	}

	public void setProduct(Product product) {
		this.product = product;
	}
}
