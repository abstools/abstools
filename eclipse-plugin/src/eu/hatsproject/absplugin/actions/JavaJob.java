package eu.hatsproject.absplugin.actions;

import static eu.hatsproject.absplugin.util.Constants.*;
import static eu.hatsproject.absplugin.util.UtilityFunctions.getAbsNature;
import static eu.hatsproject.absplugin.util.UtilityFunctions.standardExceptionHandling;
import static eu.hatsproject.absplugin.util.UtilityFunctions.isABSFile;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.io.PrintWriter;
import java.net.InetSocketAddress;
import java.net.Socket;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.*;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.jdt.core.compiler.CompilationProgress;
import org.eclipse.jdt.core.compiler.batch.BatchCompiler;
import org.eclipse.jface.action.IAction;
import org.eclipse.swt.widgets.Display;
import org.eclipse.ui.PartInitException;
import org.osgi.framework.Bundle;

import abs.backend.java.JavaCode;
import abs.frontend.ast.CompilationUnit;
import abs.frontend.ast.Model;
import abs.frontend.ast.ModuleDecl;
import eu.hatsproject.absplugin.actions.runconfig.RunConfigEnums.DebuggerObserver;
import eu.hatsproject.absplugin.actions.runconfig.RunConfigEnums.DebuggerScheduler;
import eu.hatsproject.absplugin.builder.AbsNature;
import eu.hatsproject.absplugin.console.ConsoleManager;
import eu.hatsproject.absplugin.console.MsgConsole;
import eu.hatsproject.absplugin.debug.model.Debugger;
import eu.hatsproject.absplugin.exceptions.AbsJobException;
import eu.hatsproject.absplugin.util.UtilityFunctions;

public class JavaJob extends Job {
	// debug this class -> enables printlns
	private static boolean debugMode = DO_DEBUG;

	private MsgConsole javaConsole;
	private MsgConsole sdeditconsole;
	private IProject myProject;
	private IProgressMonitor refreshProgressMonitor;
	private IFile curFile;
	private int sdeditPort = 60001;

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
	private Process sdeditProcess = null;
	private boolean isCanceled = false;

	// used by run config
	private String debuggerArgsOther;
	private String debuggerArgsSystemObserver;
	private String debuggerArgsTotalScheduler;
	private String debuggerCompileFirst;
	private String debuggerArgsRandomSeed;
	private boolean useInternalDebugger;
	private boolean debuggerIsInDebugMode;
	
	
	/**
	 * creates a new job for an action. 
	 * 
	 * @param name - job name
	 * @param action - compile, debug or SDedit action
	 * @param project - ABS file
	 * @param file - current opened file
	 */
	public JavaJob(String name, IAction action, IProject project, IFile file) {
		super(name);
		this.action = action;
		this.myProject = project;
		this.curFile = file;
		this.setUser(true);
		useInternalDebugger = true;
		debuggerIsInDebugMode = true;
	}

	@Override
	protected IStatus run(IProgressMonitor monitor) {
		monitor.beginTask("ABS Java Backend", 100);
		
		try{
			//must be an ABS project
			if (myProject == null || !myProject.exists()){
				return showErrorMessage("The project does not exist.");
			} else if (!myProject.hasNature(NATURE_ID)){
				return showErrorMessage("Chosen project '"+myProject.getName()+"' is not an ABS project.");
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
		javaConsole = getAbsNature(myProject).getJavaConsole();
		javaConsole.clear();
		if(startSDE){
			sdeditconsole = getAbsNature(myProject).getSDEditConsole();
			sdeditconsole.clear();
		}
	}
	
	/**
	 * Init property options
	 * 
	 * @see eu.hatsproject.absplugin.properties.JavaPropertyPage
	 * 
	 * @throws AbsJobException, if project is not an ABS project
	 */
	private void loadPropertyOptions() throws AbsJobException {
		javaPath    = getPathToGeneratedJavaFiles(myProject);
		sourceOnly  = getAbsNature(myProject).getProjectPreferenceStore().getBoolean(SOURCE_ONLY);
		noWarnings  = getAbsNature(myProject).getProjectPreferenceStore().getBoolean(NO_WARNINGS);
		compileCode = getAbsNature(myProject).getProjectPreferenceStore().getBoolean(ALWAYS_COMPILE);
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
				executeDebugger(absFrontendLocation);

			}
			monitor.worked(12);

			// refresh project tree
			myProject.refreshLocal(IProject.DEPTH_INFINITE, refreshProgressMonitor);

			return createStatus();

		} catch (AbsJobException e) {
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
			CoreException, IOException {
		// generate .java files
		generateJavaCode(javaPath, myProject);
	
		// generate class files
		if (!sourceOnly && !isCanceled) {
			generateJavaClassFiles(absFrontendLocation, javaPath.toFile(),
					noWarnings);
		}
	}

	/**
	 * generates .java files (no .class files)
	 * 
	 * @param path - where to add the modules / java-files
	 * @param project - the ABS project
	 * @throws IOException, if unable to create java files 
	 * @throws AbsJobException, if unable to generate java files 
	 */
	private void generateJavaCode(Path path, IProject project) throws AbsJobException, IOException {
		Model model = getModelFromProject(project);
	
		if (debugMode) System.out.println("Creating java source files");
	    JavaCode javaCode = new JavaCode(path.toFile());
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
		String args = "-1.6 "+noWarn+" -classpath "+"\""+absFrontendLocation+"\""+" "+"\""+path.getAbsolutePath()+"\"";
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
		BatchCompiler.compile(
				args,
				new PrintWriter(os),
				new PrintWriter(os),
				progress);
	}

	private void executeDebugger(String absFrontendLocation) throws AbsJobException,
			IOException, CoreException {
		checkPath(javaPath);
		if (startSDE && !isCanceled) {
			startSDE(sdeditPort);
		}
		if (!isCanceled)
			findAndExecuteMain(absFrontendLocation, javaPath, myProject, curFile, startSDE);
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
	 * @param javaPath - where to find generated Java files
	 * @param project - the ABS project
	 * @param currentFile - ABS file or Main.java (maybe null)
	 * @param withSDE - start SDEdit
	 * @throws AbsJobException - if no main block found
	 * @throws IOException - If an I/O error occurs 
	 */
	private void findAndExecuteMain(String absFrontendLocation, Path javaPath, IProject project, IFile currentFile, boolean withSDE) throws AbsJobException, IOException {
		File mainFile = null;
		String moduleName = null;
		String info = null;

		//Search for a main file in the model of the current abs file
		if(UtilityFunctions.isABSFile(currentFile)){
		   searchForMainInModel(currentFile, project, moduleName, mainFile);
		} else if(currentFile.getName().equals("Main.java")){
		   if (currentFile.getLocation().isAbsolute()) {
		      mainFile = currentFile.getLocation().toFile();
		   } else {
		      mainFile = project.getLocation().append(currentFile.getLocation()).toFile();
		   }
		}

		//Search for the main file if previous searching was not successful
		if(mainFile == null){
			mainFile = findMainInGeneratedJavaFiles(javaPath);
			if(mainFile == null){
				throw new AbsJobException("No 'Main.java' file found.");
			}
			if(currentFile!=null){
				info = "No main file found for \""+currentFile.getName()+"\".\nBut there is a main file in the project:";
			}
		}

		//Find the module name
		if(moduleName == null){
			moduleName = "Main";
			File dir = new File(mainFile.getAbsolutePath());
			while(!javaPath.toFile().equals(dir)){
				if(dir.isDirectory()){
					moduleName = dir.getName() + "." + moduleName;
				}
				dir = dir.getParentFile();
				if (dir == null){
					if(debugMode) System.err.println("module name for "+mainFile.getAbsolutePath()+" not found");
					throw new AbsJobException("Module name not found");
				}
			}
		}

		debugAbsFiles(absFrontendLocation, javaPath, withSDE, moduleName, info);
	}

	private void searchForMainInModel(IFile currentFile, IProject project, String moduleName, File mainFile) throws AbsJobException{
		AbsNature nature = UtilityFunctions.getAbsNature(project);
		if(nature == null){
			throw new AbsJobException("Could not start the debugger, because selected file (" + currentFile.getName() +  ") is not in an ABS project!");
		}
		synchronized (nature.modelLock) {
			CompilationUnit unit = nature.getCompilationUnit(currentFile);
			if(unit != null){
				int countModules = unit.getNumModuleDecl();
				int i = 0;
				while(i<countModules){
					ModuleDecl module = unit.getModuleDecl(countModules-1);
					moduleName = module.getName(); 
	        
					if(moduleName != null && moduleName.length() > 0){
						if(moduleName.contentEquals("\\.")) moduleName = moduleName.replaceAll("\\.", File.separator);
						IPath temp = javaPath.append(moduleName);
						temp = temp.append("Main.java");
						mainFile = temp.toFile();
						if(mainFile.exists()){
							//file found
							moduleName += ".Main";
							break;
						}
					} 		        				
					//go to next model
					mainFile = null;
					moduleName = null;
					i++;
				}
			}
		}
	}
	
	/**
	 * Searches for a "Main.java" file in the generated files
	 * 
	 * @param javaPath - where to find the generated files
	 * @return file with main block
	 * @throws AbsJobException 
	 */
	private File findMainInGeneratedJavaFiles(Path javaPath) throws AbsJobException{
		File mainFile = null;
		List<File> mainFiles = null;
		File javaDir = javaPath.toFile();
		File[] genDirs = javaDir.listFiles();
		
		if(genDirs != null){
			ArrayList<File> modules = new ArrayList<File>(Arrays.asList(genDirs));
			mainFiles = new ArrayList<File>();
			for (int i = 0; i<modules.size(); i++) {
				File file = modules.get(i);
				if(file.isDirectory()){
					//add all subdirs and files to List
					modules.addAll(Arrays.asList(file.listFiles()));
				}
				if(file.isFile() && file.getName().equals("Main.java")){
					//Main.java found
					mainFiles.add(file);
				}
			}
		}
		
		//get the first one
		if(mainFiles == null || mainFiles.size()==0){
			throw new AbsJobException("No 'Main.java' file found");
		} else {
			mainFile = mainFiles.get(0);
		}
		
		return mainFile;
	}

	private void debugAbsFiles(String absFrontendLocation, final Path javaPath,
			boolean useBothObserver, final String moduleName, String info)
			throws IOException {
		setDebuggerArgumentsIfNull(useBothObserver);
		
		final ArrayList<String> args = new ArrayList<String>();

		args.add("java");
		args.add("-cp");
		args.add(absFrontendLocation+File.pathSeparator+javaPath);
		
		if(debuggerIsInDebugMode){
			addIfNotNullOrEmpty(args,"-Dabs.debug=true");
		}
		addIfNotNullOrEmpty(args,debuggerArgsOther);
		addIfNotNullOrEmpty(args,debuggerArgsSystemObserver);
		
		addIfNotNullOrEmpty(args,debuggerArgsTotalScheduler);
		addIfNotNullOrEmpty(args,debuggerArgsRandomSeed);

		args.add(moduleName);

		if (debugMode) System.out.println("run java code with: "+args);

		if(useInternalDebugger){
			startInternalDebugger(javaPath, moduleName);	
		} else {
			debugProcess = Runtime.getRuntime().exec(args.toArray(new String[args.size()]));
			printProcessOutput(moduleName, info, debugProcess);
		}
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

	private void startInternalDebugger(final Path javaPath,
			final String moduleName) {
		Debugger.startABSRuntime(myProject.getName(), moduleName, javaPath,
				debuggerArgsSystemObserver, debuggerArgsTotalScheduler,
				debuggerIsInDebugMode, debuggerArgsRandomSeed);
	}

	/**
	 * prints the output on users console using a new thread
	 * 
	 * @param moduleName
	 * @param info
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

	private Model getModelFromProject(IProject project) throws AbsJobException {
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
	
	/**
	 * starts a new SDedit process 
	 * 
	 * @param port
	 * @throws IOException
	 */
	private void startSDE(int port) throws IOException{
		if (sdeditIsRunning(port))
			return;
		
		ArrayList<String> args = buildSDEditCommand(port);
		
		Bundle seditbundle = Platform.getBundle(SDEDIT_PLUGIN_ID);
		
		File installdir = FileLocator.getBundleFile(seditbundle).getAbsoluteFile();
		ProcessBuilder pb = new ProcessBuilder(args.toArray(new String[0]));
		pb.directory(new File(installdir.toString()));
		sdeditProcess = pb.start();

		Job watchdogJob = 
			new SDEditWatchdog(this,"SDEdit running...", sdeditconsole, sdeditProcess, debugProcess);

		watchdogJob.schedule();
	}

	private ArrayList<String> buildSDEditCommand(int port) {
		ArrayList<String> args = new ArrayList<String>();
		
		args.add("java");
		args.add("-classpath");
		
		StringBuilder sb = buildClasspath();
		args.add(sb.toString());
		
		args.add(SDE_MAIN_CLASS);
		args.add("-s");
		args.add(String.valueOf(port));
		return args;
	}

	private boolean sdeditIsRunning(int port) {
		try {
			Socket s = new Socket();
			s.connect(new InetSocketAddress("localhost", port));
			s.close();
			if (debugMode)
				System.out.println("SDEdit server is already running");
			return true;
		} catch (IOException e) {
			return false;
		}
   }

	private StringBuilder buildClasspath() {
		ArrayList<String> libList = new ArrayList<String>();

		libList.add("lib/commons-cli-1.1.jar");
		libList.add("lib/freehep-export-2.1.1.jar");
		libList.add("lib/freehep-graphics2d-2.1.1.jar");
		libList.add("lib/freehep-graphicsio-2.1.1.jar");
		libList.add("lib/freehep-graphicsio-emf-2.1.1.jar");
		libList.add("lib/freehep-graphicsio-java-2.1.1.jar");
		libList.add("lib/freehep-graphicsio-pdf-2.1.1.jar");
		libList.add("lib/freehep-graphicsio-ps-2.1.1.jar");
		libList.add("lib/freehep-graphicsio-svg-2.1.1.jar");
		libList.add("lib/freehep-graphicsio-swf-2.1.1.jar");
		libList.add("lib/freehep-io-2.0.2.jar");
		libList.add("lib/freehep-swing-2.0.3.jar");
		libList.add("lib/freehep-util-2.0.2.jar");
		libList.add("lib/freehep-xml-2.1.1.jar");
		libList.add("lib/openide-lookup-1.9-patched-1.0.jar");
		
		StringBuilder cp = new StringBuilder();
		cp.append("bin");
		for (String lib : libList) {
			cp.append(File.pathSeparator);
			cp.append(lib);
		}
		
		return cp;
	}

	@Override
	protected void canceling() {
		isCanceled = true;
		if(debugProcess != null) debugProcess.destroy();
		if(sdeditProcess != null) sdeditProcess.destroy();
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

	public MsgConsole getSdeditconsole() {
		return sdeditconsole;
	}

	public Process getSdeditProcess() {
		return sdeditProcess;
	}
}
