/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.absmodels.abs.plugin.builder;


import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.lang.reflect.InvocationTargetException;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Properties;
import java.util.Set;

import org.absmodels.abs.plugin.Activator;
import org.absmodels.abs.plugin.console.ConsoleManager;
import org.absmodels.abs.plugin.console.MsgConsole;
import org.absmodels.abs.plugin.editor.outline.PackageAbsFile;
import org.absmodels.abs.plugin.editor.outline.PackageContainer;
import org.absmodels.abs.plugin.editor.outline.PackageEntry;
import org.absmodels.abs.plugin.editor.reconciling.AbsModelManager;
import org.absmodels.abs.plugin.editor.reconciling.AbsModelManagerImpl;
import org.absmodels.abs.plugin.internal.IncrementalModelBuilder;
import org.absmodels.abs.plugin.internal.NoModelException;
import org.absmodels.abs.plugin.internal.TypecheckInternalException;
import org.eclipse.core.resources.*;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.Path;
import org.eclipse.jface.preference.IPersistentPreferenceStore;
import org.eclipse.ui.actions.WorkspaceModifyOperation;
import org.eclipse.ui.preferences.ScopedPreferenceStore;

import abs.frontend.analyser.SemanticError;
import abs.frontend.analyser.SemanticErrorList;
import abs.frontend.ast.ASTNode;
import abs.frontend.ast.CompilationUnit;
import abs.frontend.ast.Model;
import abs.frontend.parser.Main;
import abs.frontend.parser.ParserError;
import abs.frontend.parser.SyntaxError;
import abs.frontend.typechecker.locationtypes.LocationType;
import abs.frontend.typechecker.locationtypes.infer.LocationTypeInferrerExtension;
import abs.frontend.typechecker.locationtypes.infer.LocationTypeVariable;
import static org.absmodels.abs.plugin.util.Constants.*;
import static org.absmodels.abs.plugin.util.UtilityFunctions.*;

public class AbsNature implements IProjectNature {
	private IProject project;
	private final IncrementalModelBuilder modelbuilder = new IncrementalModelBuilder();
	private AbsModelManager modelManager = new AbsModelManagerImpl(this);
	
	public static final String PACKAGE_DEPENDENCIES = ".dependencies";
	public static final String READONLY_PACKAGE_SUFFIX = "-readonly";
	
	private final PackageContainer packageContainer = new PackageContainer();
	
	/**
	 * the lock for access to the model. Should always be synchronized with when accessing the 
	 * model in the model builder in some way
	 */
	public volatile Object modelLock = new Object(); // TODO: Should probably be final, but then the mocks fail.
	private ScopedPreferenceStore preferencestore;
	
	/**
	 * The default console singleton used by getDefaultJavaConsole()
	 */
	private MsgConsole defaultJavaConsole = null;
	
	/**
	 * The default console singleton used by getDefaultSDEditConsole()
	 */
	private MsgConsole defaultSDEditConsole = null;
	
	/**
	 * The default console singleton used by getMaudeConsole()
	 */
	private MsgConsole defaultMaudeConsole = null;
	
	/**
	 * The default console singleton used by getMaudeConsole()
	 */
	private MsgConsole defaultABSUnitTestExecutionConsole = null;
	
	/**
	 * The default console singleton used by getMavenConsole()
	 */
	private MsgConsole defaultMavenConsole = null;
	
	/**
	 * Gives the default console for Java Output
	 * @return Java Message Console
	 */	
	public MsgConsole getJavaConsole(){
		if (defaultJavaConsole == null){
			defaultJavaConsole = ConsoleManager.newConsole("Java Output (" + getProject().getName() + ")");
		}
		
		return defaultJavaConsole;
	}
	
	/**
	 * Gives the default console for SDEdit Output
	 * @return SDEdit Message Console
	 */	
	public MsgConsole getSDEditConsole(){
		if (defaultSDEditConsole == null){
			defaultSDEditConsole = ConsoleManager.newConsole("SDEdit Output (" + getProject().getName() + ")");
		}
		
		return defaultSDEditConsole;
	}
	
	/**
	 * Gives the default console for Maude Output
	 * @return Maude Message Console
	 */	
	public MsgConsole getMaudeConsole(){
		if (defaultMaudeConsole == null){
			defaultMaudeConsole = ConsoleManager.newConsole("Maude Output (" + getProject().getName() + ")");
		}
		
		return defaultMaudeConsole;
	}
	
	/**
	 * Gives the default console for ABSUnit Test Execution Output
	 * @return ABSUnit Test Execution Message Console
	 */	
	public MsgConsole getABSUnitTestExecutionConsole(){
		if (defaultABSUnitTestExecutionConsole == null){
			defaultABSUnitTestExecutionConsole = ConsoleManager.newConsole("ABSUnit Test Execution Output (" + getProject().getName() + ")");
		}
		
		return defaultABSUnitTestExecutionConsole;
	}
	
	/**
	 * Gives the default console for Maven Output
	 * @return Maven Message Console
	 */	
	public MsgConsole getMavenConsole(){
		if (defaultMavenConsole == null){
			defaultMavenConsole = ConsoleManager.newConsole("Maven Output (" + getProject().getName() + ")");
		}
		
		return defaultMavenConsole;
	}

	/**
	 * adds the current builder for abs files to the project. Is only called once when the
	 * nature is initially assigned to the project.
	 * 
	 * @see org.eclipse.core.resources.IProjectNature#configure()
	 */
	@Override
	public void configure() throws CoreException {
		IProjectDescription desc = project.getDescription();
		ICommand[] commands = desc.getBuildSpec();

		for (int i = 0; i < commands.length; ++i) {
			if (commands[i].getBuilderName().equals(BUILDER_ID)) {
				return;
			}
		}

		ICommand[] newCommands = new ICommand[commands.length + 1];
		System.arraycopy(commands, 0, newCommands, 0, commands.length);
		ICommand command = desc.newCommand();
		command.setBuilderName(BUILDER_ID);
		newCommands[newCommands.length - 1] = command;
		desc.setBuildSpec(newCommands);
		project.setDescription(desc, null);
	}

	/**
	 * removes the builder for abs files from the project. Gets called only when
	 * removing the nature from the project.
	 * 
	 * @see org.eclipse.core.resources.IProjectNature#deconfigure()
	 */
	@Override
	public void deconfigure() throws CoreException {
		IProjectDescription description = getProject().getDescription();
		ICommand[] commands = description.getBuildSpec();
		for (int i = 0; i < commands.length; ++i) {
			if (commands[i].getBuilderName().equals(BUILDER_ID)) {
				ICommand[] newCommands = new ICommand[commands.length - 1];
				System.arraycopy(commands, 0, newCommands, 0, i);
				System.arraycopy(commands, i + 1, newCommands, i,
						commands.length - i - 1);
				description.setBuildSpec(newCommands);
				project.setDescription(description, null);			
				return;
			}
		}
	}

	@Override
	public IProject getProject() {
		return project;
	}

	@Override
	public void setProject(IProject project) {
		this.project = project;
		// initialise package dependencies
		initDependencies(); 
	}
	
	public void createMarkers(Model model) throws CoreException {
        createMarkers(model.getErrors());
        createMarkers(model.getTypeErrors());
    }

    public void createMarkers(SemanticErrorList errors) throws CoreException {
        for (SemanticError e : errors) {
            createMarker(e);
        }
    }
	
	public void createMarker(SemanticError error) throws CoreException {
		createMarker(error.getNode(), error.getMsg(), IMarker.SEVERITY_ERROR, TYPECHECK_MARKER_TYPE);
	}

	/**
	 * take a node of the AST, finds the corresponding compilation unit and adds a marker for the message to the file the
	 * compilation unit was parsed from
	 * @param node some AST node which is part of a compilation unit
	 * @param message the message to be shown in the marker pop up
	 * @param severity {@link IMarker#SEVERITY_ERROR} {@link IMarker#SEVERITY_WARNING} {@link IMarker#SEVERITY_INFO}
	 * @param markerType the id of the marker that should be created
	 * @throws CoreException {@link IMarker#setAttribute(String, boolean)} {@link IMarker#getAttribute(String, int)}
	 */
	private static void createMarker( ASTNode<?> node, String message, int severity, String markerType) throws CoreException {
		if (node == null)
			return; 
		
	   IFile declfile = ResourcesPlugin.getWorkspace().getRoot().getFileForLocation(new Path((node.getCompilationUnit()).getFileName()));
		/* [stolz] I had a situation where I had the ABSFrontend in the workspace, and then closed it:
			org.eclipse.core.internal.resources.ResourceException: Resource '/ABSFrontend/src/abs/lang/abslang.abs' does not exist.
				at org.eclipse.core.internal.resources.Resource.checkExists(Resource.java:320)
				at org.eclipse.core.internal.resources.Resource.checkAccessible(Resource.java:194)
				at org.eclipse.core.internal.resources.Resource.createMarker(Resource.java:711)
				at org.abs-models.abs.plugin.builder.AbsNature.createMarker(AbsNature.java:241)
			FIXME: We shouldn't have picked that one up in the first place, I guess, but better be safe than sorry.
		 */
		if(declfile==null || !declfile.isAccessible())
			return;
		assert declfile.isAccessible();
		IMarker marker = declfile.createMarker(markerType);
		marker.setAttribute(IMarker.MESSAGE, message);
		marker.setAttribute(IMarker.SEVERITY, severity);
		marker.setAttribute(START_LINE, node.getStartLine()-1);
		marker.setAttribute(START_COLUMN, node.getStartColumn()-1);
		marker.setAttribute(END_LINE, node.getEndLine()-1);
		marker.setAttribute(END_COLUMN, node.getEndColumn());
		marker.setAttribute(IMarker.LINE_NUMBER, node.getStartLine());
   }
	
	/**
	 * parses the given resource with the given model builder. The resource is only parsed if it is an abs file
	 * @param resource the abs file
	 * @param withincomplete include incomplete expressions into the AST?
	 * @param monitor 
	 */
	public void parseABSFile(IResource resource, final boolean withincomplete, IProgressMonitor monitor) {
		if (resource.exists() && isABSFile(resource)) {
			final IFile file = (IFile) resource;
			assert file.exists();
			try {
				// Markers modify the workspace:
				new WorkspaceModifyOperation() {

					@Override
					protected void execute(IProgressMonitor monitor) throws CoreException,
					InvocationTargetException, InterruptedException {
						/* Only delete PARSE-markers first: if we've just been launched,
						 * we don't want to erase persistent markers, since type-errors etc.
						 * only come back through an explicit build, which doesn't happen
						 * on launching Eclipse even with auto-build. [stolz] 
						 */
						file.deleteMarkers(PARSE_MARKER_TYPE, true, IResource.DEPTH_ZERO);
						try {
							if (!file.isSynchronized(IResource.DEPTH_ZERO)) {
								file.refreshLocal(IResource.DEPTH_ZERO, monitor);
							}

							Main m = new Main();
							m.setWithStdLib(true);
							m.setAllowIncompleteExpr(withincomplete);

							List<CompilationUnit> units = new ArrayList<CompilationUnit>();
							if (isABSPackage(file)) {
								units.addAll(m.parseABSPackageFile(file.getLocation().toFile()));
							} else {
								CompilationUnit cu = m.parseUnit(file.getLocation().toFile(), null, new InputStreamReader(file.getContents()));
								cu.setName(file.getLocation().toFile().getAbsolutePath());
								units.add(cu);
							}
							modelbuilder.addCompilationUnits(units);

							for (CompilationUnit cu : units) {
								if(cu.hasParserErrors()){
									for(ParserError err : cu.getParserErrors()){
										addMarker(file, err);
									}
								}
							}
						} catch(NoModelException e){
							//ignore
						} catch (CoreException e) {
							throw e;
						}catch (Exception e) {
							throw new InvocationTargetException(e);
						}
					}
				}.run(monitor);
			} catch (InvocationTargetException e) {
				Activator.logException(e);
			} catch (InterruptedException e) {
			}
		} else
			assert false : resource;
	}

	public boolean toIncludeInScope(IResource resource) {
		if (project == null)
			return false;
		IFolder target = project.getFolder("target");
		if (! target.exists()) {
			return true;
		}
		boolean ignore = getProjectPreferenceStore().getBoolean(MAVEN_IGNORE_TARGET_FOLDER);
		return !ignore || !target.getProjectRelativePath().isPrefixOf(resource.getProjectRelativePath());
	}

	public static void addMarker(IResource file, ParserError err) throws CoreException {
      int startline   = err.getLine()-1;
      int startcolumn = err.getColumn()-1;
      int endline;
      int endcolumn;

      endcolumn = -1;
      endline   = startline;

      String message = err.getMessage();
      int severity   = IMarker.SEVERITY_ERROR;
      IMarker marker = file.createMarker(PARSE_MARKER_TYPE);
      marker.setAttribute(IMarker.MESSAGE, message);
      marker.setAttribute(IMarker.SEVERITY, severity);
      marker.setAttribute(START_LINE, startline);
      marker.setAttribute(START_COLUMN, startcolumn);
      marker.setAttribute(END_LINE, endline);
      marker.setAttribute(END_COLUMN, endcolumn);
      marker.setAttribute(IMarker.LINE_NUMBER, startline+1);
   }
	
	/**
	 * takes the properties from the project preference store for location type checking and location type precision. Typeckecks
	 * the current model in the current model builder.
	 * Note that your model must be sufficiently "complete" and not have any semantic errors .
	 * @param monitor 
	 * @throws CoreException {@link IResource#deleteMarkers(String, boolean, int)} does not handle exceptions thrown by
	 * #createMarker(SemanticError) and #createMarker(TypecheckInternalException)
	 */
    void typeCheckModel(IProgressMonitor monitor) throws CoreException{
	   getProject().deleteMarkers(TYPECHECK_MARKER_TYPE, true, IResource.DEPTH_INFINITE);
	   getProject().deleteMarkers(LOCATION_TYPE_INFERENCE_MARKER_TYPE, true, IResource.DEPTH_INFINITE);
	   boolean dolocationtypecheck = getProjectPreferenceStore().getBoolean(LOCATION_TYPECHECK);
	   String defaultlocationtype = getProjectPreferenceStore().getString(DEFAULT_LOCATION_TYPE);
	   String defaultlocationtypeprecision = getProjectPreferenceStore().getString(LOCATION_TYPE_PRECISION);
	   boolean checkProducts = getProjectPreferenceStore().getBoolean(PRODUCT_TYPECHECK);
	   try {
		   addPackagesForTypeChecking();
		   final SemanticErrorList typeerrors = modelbuilder.typeCheckModel(monitor,dolocationtypecheck, defaultlocationtype, defaultlocationtypeprecision, checkProducts);
		   createMarkers(typeerrors);

		   if (dolocationtypecheck) {
			   createLocationTypeInferenceMarker();
		   }
	   } catch (NoModelException e) {
		   //ignore
		   return;
	   } catch (TypecheckInternalException e) {
		   /* Internal error caught. Log, and turn into an error marker */
		   Activator.logException(e);
		   createMarker(e);
		   return;
	   }
   }
	
	/**
	 * Add ABS package dependencies to {@link AbsNature#modelbuilder} for type checking 
	 * @throws TypecheckInternalException 
	 */
	private void addPackagesForTypeChecking() throws TypecheckInternalException {
		try {
			Main m = new Main();
			m.setWithStdLib(true);
			m.setAllowIncompleteExpr(true);
			List<CompilationUnit> units = new ArrayList<CompilationUnit>();
			for (PackageEntry entry : packageContainer.getPackages()) {
				File file = new File(entry.getPath());
				if (isABSPackage(file)) {
				   units.addAll(m.parseABSPackageFile(file));
				}
			}
			modelbuilder.addCompilationUnits(units);
		} catch (IOException e) {
			throw new TypecheckInternalException(e);
		} catch (NoModelException e) {
			//ignore
		}
	}

	/**
	 * retrieves the result of the type inference from the modelbuilders
	 * @return the result of the type inference or <b>null</b> if type inference is off or
	 * an exception occurred while type inference was in progress
	 */
	public Map<LocationTypeVariable, LocationType> getLocationTypeInferrerResult(){
		synchronized (modelLock) {
			LocationTypeInferrerExtension locationTypeInferrerExtension = modelbuilder.getLocationTypeInferrerExtension();
			if (locationTypeInferrerExtension != null) {
				Map<LocationTypeVariable, LocationType> inferResult = locationTypeInferrerExtension.getResults();
				return inferResult;
			} else {
				return null;
			}
		}
	}

	private void createLocationTypeInferenceMarker() throws CoreException {
	   synchronized (modelLock) {
	       LocationTypeInferrerExtension locationTypeInferrerExtension = modelbuilder.getLocationTypeInferrerExtension();
	       if (locationTypeInferrerExtension != null) {
	           Map<LocationTypeVariable, LocationType> inferResult = locationTypeInferrerExtension.getResults();
	           if (inferResult != null) {
	               if (getProjectPreferenceStore().getBoolean(LOCATION_TYPE_OVERLAY)) {
	                   for (Entry<LocationTypeVariable, LocationType> e : inferResult.entrySet()) {
	                       LocationType annoType = e.getKey().getAnnotatedType();
	                       if (annoType == null || annoType.equals(LocationType.INFER)) {
	                           int severity = IMarker.SEVERITY_ERROR;
	                           if (e.getValue().isNear()) {
	                               severity = IMarker.SEVERITY_INFO;
	                           } else if (e.getValue().isSomewhere()) {
	                               severity = IMarker.SEVERITY_WARNING;
	                           }

	                           createMarker(e.getKey().getNode(), "Inferred Location Type: " + e.getValue(), severity
	                        		   , LOCATION_TYPE_INFERENCE_MARKER_TYPE);
	                       }
	                   }
	               } 
	           } else {
	               IMarker marker = getProject().createMarker(TYPECHECK_MARKER_TYPE);
	               marker.setAttribute(IMarker.MESSAGE, "Location Type Inference Failed");
	               marker.setAttribute(IMarker.SEVERITY, IMarker.SEVERITY_ERROR);
	           }
	       }
	   }
   }

	/**
	 * adds a marker for the TypecheckInternalException to the current project corresponding to this narture
	 * @param e the exception from type checking the model
	 * @throws CoreException {@link IResource#createMarker(String)}
	 */
	private void createMarker(TypecheckInternalException e)
			throws CoreException {
		IMarker exceptionMarker = project.createMarker(TYPECHECK_MARKER_TYPE);
		exceptionMarker.setAttribute(IMarker.MESSAGE, "Exception while typechecking: "+e.getMessage());
		exceptionMarker.setAttribute(IMarker.SEVERITY, IMarker.SEVERITY_ERROR);
	}
	
	public void removeCompilationUnit(IResource file){
		CompilationUnit cu = getCompilationUnit(file);
		try {
			modelbuilder.removeCompilationUnit(cu);
		} catch (NoModelException e) {
			//ignore
		}
	}
	
	/**
	 * retrieves the compilationUnit for the given file from the modelbuilder.
	 * 
	 * @param curFile
	 * @return the comilationUnit for the file or <b>null</b> if the model is not (yet) parsed
	 */
	public CompilationUnit getCompilationUnit(IResource curFile){
		assert curFile != null;
		return getCompilationUnit(curFile.getLocation().toFile().getAbsolutePath());
	}
	
	/**
	 * retrieves the compilationUnit for the file with the given absoluteFilePath from the modelbuilder
	 * @param absoluteFilePath
	 * @return
	 */
	public CompilationUnit getCompilationUnit(String absoluteFilePath){
		try {
			return modelbuilder.getCompilationUnit(absoluteFilePath);
		} catch (NoModelException e) {
			return null;
		}
	}
	
	public void cleanModel(){
		modelbuilder.cleanModel();
	}
	
	public Model getCompleteModel(){
		return getCompleteModel(null);
	}

	public Model getCompleteModel(IProgressMonitor mon){
		Model model = modelbuilder.getCompleteModel();
		/* Not compiled yet? */
		if (model == null && project != null) {
			try {
				project.build(IncrementalProjectBuilder.FULL_BUILD, mon);
				// Now it should be ready:
				model = modelbuilder.getCompleteModel();
			} catch (CoreException e) {
				Activator.logException(e);
			}
		}
		return model;
	}
	
	/**
	 * creates and returns the singleton for the project-wide preference store
	 * @return the project preference store
	 */
	public IPersistentPreferenceStore getProjectPreferenceStore(){
		if(preferencestore == null){
			preferencestore = new ScopedPreferenceStore(new ProjectScope(getProject()), PLUGIN_ID);
			initProjectDefaults();
		}
		return preferencestore;
	}

	/**
	 * Set the defaults for the properties in the project property pages
	 */
	private void initProjectDefaults(){
		preferencestore.setDefault(LOCATION_TYPECHECK, true);
		preferencestore.setDefault(PRODUCT_TYPECHECK, true);
		preferencestore.setDefault(LOCATION_TYPE_OVERLAY, true);
		preferencestore.setDefault(DEFAULT_LOCATION_TYPE, DEFAULT_DEFAULT_LOCATION_TYPE);
		preferencestore.setDefault(LOCATION_TYPE_PRECISION, DEFAULT_LOCATION_TYPE_PRECISION);
		
		if (project.getFile(DEFAULT_MAVEN_POM_PATH).exists()) {
			preferencestore.setDefault(MAUDE_PATH, DEFAULT_MAVEN_TARGET_MAUDE_PATH);
			preferencestore.setDefault(JAVA_SOURCE_PATH, DEFAULT_MAVEN_TARGET_JAVA_PATH);
		} else {
			preferencestore.setDefault(MAUDE_PATH, DEFAULT_MAUDE_PATH);
			preferencestore.setDefault(JAVA_SOURCE_PATH, DEFAULT_JAVA_PATH);
		}
		
		preferencestore.setDefault(NO_WARNINGS, true);
		preferencestore.setDefault(SOURCE_ONLY, false);
		preferencestore.setDefault(ALWAYS_COMPILE, true);
		
		/*
		 * Maven 
		 */
		preferencestore.setDefault(MAVEN_EXEC_PATH, DEFAULT_MAVEN_EXEC_PATH);
		preferencestore.setDefault(MAVEN_IGNORE_TARGET_FOLDER, false);
	}
	
	public void initDependencies() {
		try {
			if (project != null) {
				packageContainer.clear();
				File file = new File(project.getFile(PACKAGE_DEPENDENCIES).getLocationURI());
				Set<PackageEntry> entries = new HashSet<PackageEntry>();
				if (file.exists()) {
					Properties prop = new Properties();
					prop.loadFromXML(new FileInputStream(file));
					for (String qualified : prop.stringPropertyNames()) {
						Boolean readonly = Boolean.valueOf(prop.getProperty(qualified));
						File f = new File(qualified);
						if (isABSPackage(f)) {
							entries.add(new PackageEntry(packageContainer, f.getName(), qualified, readonly));
						}
					}
					packageContainer.setPackages(entries);
					packageContainer.setProject(project);
				}
			}
		} catch (IOException e) {
			Activator.logException(e);
		}
	}

	public PackageContainer getPackages() {
		return packageContainer;
	}

	/**
	 * @author stolz
	 */
	public void emptyModel() {
		modelbuilder.addCompilationUnit(null); // Hack to get stdlib
	}

	/**
	 * @deprecated unused
	 */
	public void parseABSFile(PackageAbsFile file, boolean withincomplete,
			Object monitor) {
		Main m = new Main();
		m.setWithStdLib(true);
		m.setAllowIncompleteExpr(withincomplete);

		List<CompilationUnit> units = new ArrayList<CompilationUnit>();
		try {
			final File f = new File(file.getAbsoluteFilePath());
			assert f.exists();
			units.addAll(m.parseABSPackageFile(f));
			modelbuilder.addCompilationUnits(units);
		} catch (IOException e) {
			Activator.logException(e);
		} catch (NoModelException e) {
		}
	}

	public AbsModelManager getModelManager() {
		return modelManager;
	}

    
}
