/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package eu.hatsproject.absplugin.builder;

import static eu.hatsproject.absplugin.util.Constants.*;

import java.io.InputStreamReader;
import java.util.Map;
import java.util.Map.Entry;

import org.eclipse.core.resources.*;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.Path;
import org.eclipse.jface.preference.IPersistentPreferenceStore;
import org.eclipse.ui.preferences.ScopedPreferenceStore;

import abs.frontend.analyser.SemanticError;
import abs.frontend.analyser.SemanticErrorList;
import abs.frontend.ast.ASTNode;
import abs.frontend.ast.CompilationUnit;
import abs.frontend.ast.Model;
import abs.frontend.parser.ParserError;
import abs.frontend.parser.SyntaxError;
import abs.frontend.typechecker.locationtypes.LocationType;
import abs.frontend.typechecker.locationtypes.infer.LocationTypeInferrerExtension;
import abs.frontend.typechecker.locationtypes.infer.LocationTypeVariable;
import beaver.Symbol;
import eu.hatsproject.absplugin.console.ConsoleManager;
import eu.hatsproject.absplugin.console.MsgConsole;
import eu.hatsproject.absplugin.internal.IncrementalModelBuilder;
import eu.hatsproject.absplugin.internal.NoModelException;
import eu.hatsproject.absplugin.internal.TypecheckInternalException;
import static eu.hatsproject.absplugin.util.UtilityFunctions.*;

public class AbsNature implements IProjectNature {
	private IProject project;
	IncrementalModelBuilder modelbuilder = new IncrementalModelBuilder();
	
	/**
	 * the lock for access to the model. Should always be synchronized with when accessing the 
	 * model in the model builder in some way
	 */
	public volatile Object modelLock = new Object();
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
	}
	
	private void createMarker(SemanticError error) throws CoreException {
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
	private void createMarker( ASTNode<?> node, String message, int severity, String markerType) throws CoreException {
		if (node == null)
			return; 
		
		int start       = node.getStart();
		int end         = node.getEnd();
				
	   IFile declfile;
		while(!(node instanceof CompilationUnit)){
			node = node.getParent();
		}
		declfile = ResourcesPlugin.getWorkspace().getRoot().getFileForLocation(new Path(((CompilationUnit)node).getFileName()));
		if(declfile==null)
			return;
		
		IMarker marker = declfile.createMarker(markerType);
		marker.setAttribute(IMarker.MESSAGE, message);
		marker.setAttribute(IMarker.SEVERITY, severity);
		marker.setAttribute(START_LINE, Symbol.getLine(start)-1);
		marker.setAttribute(START_COLUMN, Symbol.getColumn(start)-1);
		marker.setAttribute(END_LINE, Symbol.getLine(end)-1);
		marker.setAttribute(END_COLUMN, Symbol.getColumn(end));
		marker.setAttribute(IMarker.LINE_NUMBER, Symbol.getLine(start));
   }
	
	/**
	 * {@link #parseABSFile(IResource, IncrementalModelBuilder, boolean)}
	 */
	public void parseABSFile(IResource resource) throws CoreException{
		parseABSFile(resource, false);
	}
	
	/**
	 * {@link #parseABSFile(IResource, IncrementalModelBuilder, boolean)}
	 */
	public void parseABSFile(IResource resource, boolean withincomplete) throws CoreException{
		parseABSFile(resource, modelbuilder, withincomplete);
	}
	
	/**
	 * parses the given resource with the given model builder. The resource is only parsed if it is an abs file
	 * @param resource the abs file
	 * @param builder the builder to use
	 * @param withincomplete include incomplete expressions into the AST?
	 * @throws CoreException @{@link IResource#deleteMarkers(String, boolean, int)} 
	 */
	public static void parseABSFile(IResource resource, IncrementalModelBuilder builder, boolean withincomplete) throws CoreException {
		if (isABSFile(resource)) {
			IFile file = (IFile) resource;
			file.deleteMarkers(MARKER_TYPE, true, IResource.DEPTH_ZERO);
			try {
			   if (!file.isSynchronized(IResource.DEPTH_ZERO)) {
			      file.refreshLocal(IResource.DEPTH_ZERO, null);
			   }
				CompilationUnit cu = abs.frontend.parser.Main.parseUnit(file.getLocation().toFile(), null, new InputStreamReader(file.getContents()), true, withincomplete);
				cu.setName(file.getLocation().toFile().getAbsolutePath());
				builder.setCompilationUnit(cu);
				
				if(cu.hasParserErrors()){
					for(ParserError err : cu.getParserErrors()){

						int startline   = err.getLine()-1;
						int startcolumn = err.getColumn()-1;
						int endline;
						int endcolumn;

						if(err instanceof SyntaxError){
							SyntaxError serr = (SyntaxError)err;
							int end   = serr.getToken().getEnd();
							endline   = Symbol.getLine(end)-1;
							endcolumn = Symbol.getColumn(end);
						} else {
							endcolumn = -1;
							endline   = startline;
						}

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
					return;
				}
			} catch(NoModelException e){
				//ignore
			}catch (Exception e) {
				e.printStackTrace();
			}
		}
	}
	
	/**
	 * takes the properties from the project preference store for location type checking and location type precision. Typeckecks
	 * the current model in the current model builder.
	 * @throws CoreException {@link IResource#deleteMarkers(String, boolean, int)} does not handle exceptions thrown by
	 * #createMarker(SemanticError) and #createMarker(TypecheckInternalException)
	 */
	public void typeCheckModel() throws CoreException{
		getProject().deleteMarkers(TYPECHECK_MARKER_TYPE, true, IResource.DEPTH_INFINITE);
		getProject().deleteMarkers(LOCATION_TYPE_INFERENCE_MARKER_TYPE, true, IResource.DEPTH_INFINITE);
		SemanticErrorList typeerrors;
		boolean dolocationtypecheck = getProjectPreferenceStore().getBoolean(LOCATION_TYPECHECK);
		String defaultlocationtype = getProjectPreferenceStore().getString(DEFAULT_LOCATION_TYPE);
		String defaultlocationtypeprecision = getProjectPreferenceStore().getString(LOCATION_TYPE_PRECISION);
		try {
			typeerrors = modelbuilder.typeCheckModel(dolocationtypecheck, defaultlocationtype, defaultlocationtypeprecision);
		} catch (NoModelException e) {
			//ignore
			return;
		} catch (TypecheckInternalException e) {
			createMarker(e);
			return;
		}
		if(typeerrors.size() > 0){
			for(SemanticError error : typeerrors){
				createMarker(error);
			}
		}
		
		if (dolocationtypecheck) {
			createLocationTypeInferenceMarker();
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
	
	public void removeCompilationUnit(IFile file){
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
	public CompilationUnit getCompilationUnit(IFile curFile){
		try {
			return modelbuilder.getCompilationUnit(curFile.getLocation().toFile().getAbsolutePath());
		} catch (NoModelException e) {
			return null;
		}
	}
	
	public void cleanModel(){
		modelbuilder.cleanModel();
	}
	
	public Model getCompleteModel(){
		return modelbuilder.getCompleteModel();
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
	public void initProjectDefaults(){
		preferencestore.setDefault(LOCATION_TYPECHECK, true);
		preferencestore.setDefault(LOCATION_TYPE_OVERLAY, true);
		preferencestore.setDefault(DEFAULT_LOCATION_TYPE, DEFAULT_DEFAULT_LOCATION_TYPE);
		preferencestore.setDefault(LOCATION_TYPE_PRECISION, DEFAULT_LOCATION_TYPE_PRECISION);
		preferencestore.setDefault(MAUDE_PATH, DEFAULT_MAUDE_PATH);
		preferencestore.setDefault(JAVA_SOURCE_PATH, DEFAULT_JAVA_PATH);
		preferencestore.setDefault(NO_WARNINGS, true);
		preferencestore.setDefault(SOURCE_ONLY, false);
		preferencestore.setDefault(ALWAYS_COMPILE, true);
	}
	
	
}
