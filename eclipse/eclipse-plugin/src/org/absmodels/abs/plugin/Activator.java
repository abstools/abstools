/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.absmodels.abs.plugin;

import static org.absmodels.abs.plugin.util.Constants.*;
import static org.absmodels.abs.plugin.util.UtilityFunctions.getDefaultPreferenceStore;

import java.util.ArrayList;

import org.absmodels.abs.plugin.util.Constants;
import org.absmodels.abs.plugin.util.UtilityFunctions;
import org.eclipse.core.resources.*;
import org.eclipse.core.runtime.*;
import org.eclipse.jface.preference.PreferenceConverter;
import org.eclipse.jface.resource.ColorRegistry;
import org.eclipse.jface.resource.JFaceResources;
import org.eclipse.swt.graphics.Device;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.widgets.Display;
import org.eclipse.ui.plugin.AbstractUIPlugin;
import org.osgi.framework.BundleContext;


/**
 * The activator class controls the plug-in life cycle
 */
public class Activator extends AbstractUIPlugin {

	private IResourceChangeListener resourceChangeTracker;
	
	private static class AddRemoveTracker implements IResourceChangeListener{
		private final class RebuildAbsModelJob extends WorkspaceJob {
			private final ArrayList<IResourceDelta> added;

			private RebuildAbsModelJob(ArrayList<IResourceDelta> added) {
				super(REBUILD_ABS_MODEL_JOB_NAME);
				this.added = added;
			}

			/*
			 * TODO: Review--This code looks strange:
			 * The changeset can refer to different projects, but apparently only a build on the last
			 * delta's project is triggered? Also note that there's probably a break missing in the loop.
			 */
			@Override
			public IStatus runInWorkspace(IProgressMonitor monitor)
					throws CoreException {
				IProject project = null;
				boolean absFileWasAdded = false;
				for(IResourceDelta r : added){
					project = r.getResource().getProject();
					if(project.isAccessible() && project.hasNature(NATURE_ID)){
						if (UtilityFunctions.isABSFile(r.getResource()))
							absFileWasAdded = true;
					}
				}
				if (absFileWasAdded)
					project.build(IncrementalProjectBuilder.FULL_BUILD, monitor);
				
				return Status.OK_STATUS;
			}
		}

		@Override
		public void resourceChanged(IResourceChangeEvent event) {
			IResourceDelta delta = event.getDelta();
			final ArrayList<IResourceDelta> removed = new ArrayList<IResourceDelta>();
			final ArrayList<IResourceDelta> added = new ArrayList<IResourceDelta>();
			try {
				delta.accept(new IResourceDeltaVisitor() {
					
					@Override
					public boolean visit(IResourceDelta delta) throws CoreException {
						switch(delta.getKind()){
						case IResourceDelta.ADDED:
							added.add(delta);
							break;
						case IResourceDelta.REMOVED:
							removed.add(delta);
							break;
						case IResourceDelta.CHANGED:
							break;
						}
						return true;
					}
				});
			} catch (CoreException e1) {
				logException(e1);
			}
			if(removed.size()>0){
				new RebuildAbsModelJob(removed).schedule();
			}
			
			if(added.size()>0){
				new RebuildAbsModelJob(added).schedule();
			}
		}
	}
	
	// The shared instance
	private static Activator plugin;
	
	/**
	 * The constructor
	 */
	public Activator() {
		super();
		plugin = this;
	}
	
	/*
	 * (non-Javadoc)
	 * @see org.eclipse.ui.plugin.AbstractUIPlugin#start(org.osgi.framework.BundleContext)
	 */
	@Override
	public void start(BundleContext context) throws Exception {
		super.start(context);
		//Print SWT Stack traces in case of an SWT Error (Ticket #199)
		Device.DEBUG = true;
		Display.DEBUG = true;
		initializePreferenceStore();
		initializeColors();
		IWorkspace workspace = ResourcesPlugin.getWorkspace();
		resourceChangeTracker = new AddRemoveTracker();
		workspace.addResourceChangeListener(resourceChangeTracker,IResourceChangeEvent.POST_CHANGE);
	}
	
	private void setDefaultValue(String name, RGB value){
		PreferenceConverter.setDefault(getDefaultPreferenceStore(), name, value);
	}
	
	private void setDefaultValue(String name, boolean value){
		getDefaultPreferenceStore().setDefault(name, value);
	}
	
	private void setDefaultValue(String name, String value){
		getDefaultPreferenceStore().setDefault(name, value);
	}
	
	public void initializePreferenceStore(){
		//Initialize default values of preferenceStore
		
		//Colors for syntax highlighting
		setDefaultValue(SYNTAXCOLOR_COLOR + SYNTAXCOLOR_KEYWORD,     SYNTAXCOLOR_RGB_KEYWORD);
		setDefaultValue(SYNTAXCOLOR_COLOR + SYNTAXCOLOR_STRING,      SYNTAXCOLOR_RGB_STRING);
		setDefaultValue(SYNTAXCOLOR_COLOR + SYNTAXCOLOR_COMMENT,     SYNTAXCOLOR_RGB_COMMENT);
		setDefaultValue(SYNTAXCOLOR_COLOR + SYNTAXCOLOR_FUNCTION,    SYNTAXCOLOR_RGB_FUNCTION);
		setDefaultValue(SYNTAXCOLOR_COLOR + SYNTAXCOLOR_DATATYPE,    SYNTAXCOLOR_RGB_DATATYPE);
		setDefaultValue(SYNTAXCOLOR_COLOR + SYNTAXCOLOR_VAR,         SYNTAXCOLOR_RGB_VAR        );
		setDefaultValue(SYNTAXCOLOR_COLOR + SYNTAXCOLOR_PARAM,       SYNTAXCOLOR_RGB_PARAM      );
		setDefaultValue(SYNTAXCOLOR_COLOR + SYNTAXCOLOR_FIELD,       SYNTAXCOLOR_RGB_FIELD      );
		setDefaultValue(SYNTAXCOLOR_COLOR + SYNTAXCOLOR_INTERFACE,   SYNTAXCOLOR_RGB_INTERFACE  );
		setDefaultValue(SYNTAXCOLOR_COLOR + SYNTAXCOLOR_CONSTRUCTOR, SYNTAXCOLOR_RGB_CONSTRUCTOR);

		//Style for syntax highlighting
		setDefaultValue(SYNTAXCOLOR_BOLD + SYNTAXCOLOR_KEYWORD,    true);
		setDefaultValue(SYNTAXCOLOR_BOLD + SYNTAXCOLOR_STRING,     false);
		setDefaultValue(SYNTAXCOLOR_BOLD + SYNTAXCOLOR_COMMENT,    false);
		setDefaultValue(SYNTAXCOLOR_BOLD + SYNTAXCOLOR_FUNCTION,   false);
		setDefaultValue(SYNTAXCOLOR_BOLD + SYNTAXCOLOR_DATATYPE,   false);
		setDefaultValue(SYNTAXCOLOR_BOLD + SYNTAXCOLOR_VAR,        false);
		setDefaultValue(SYNTAXCOLOR_BOLD + SYNTAXCOLOR_PARAM,      false);
		setDefaultValue(SYNTAXCOLOR_BOLD + SYNTAXCOLOR_FIELD,      false);
		setDefaultValue(SYNTAXCOLOR_BOLD + SYNTAXCOLOR_INTERFACE,  false);
		setDefaultValue(SYNTAXCOLOR_BOLD + SYNTAXCOLOR_CONSTRUCTOR,false);
		
		setDefaultValue(SYNTAXCOLOR_ITALIC + SYNTAXCOLOR_KEYWORD,    false);
		setDefaultValue(SYNTAXCOLOR_ITALIC + SYNTAXCOLOR_STRING,     false);
		setDefaultValue(SYNTAXCOLOR_ITALIC + SYNTAXCOLOR_COMMENT,    false);
		setDefaultValue(SYNTAXCOLOR_ITALIC + SYNTAXCOLOR_FUNCTION,   true);
		setDefaultValue(SYNTAXCOLOR_ITALIC + SYNTAXCOLOR_DATATYPE,   false);
		setDefaultValue(SYNTAXCOLOR_ITALIC + SYNTAXCOLOR_VAR,        false);
		setDefaultValue(SYNTAXCOLOR_ITALIC + SYNTAXCOLOR_PARAM,      false);
		setDefaultValue(SYNTAXCOLOR_ITALIC + SYNTAXCOLOR_FIELD,      false);
		setDefaultValue(SYNTAXCOLOR_ITALIC + SYNTAXCOLOR_INTERFACE,  false);
		setDefaultValue(SYNTAXCOLOR_ITALIC + SYNTAXCOLOR_CONSTRUCTOR,true);
		
		setDefaultValue(SYNTAXCOLOR_UNDERLINE + SYNTAXCOLOR_KEYWORD,    false);
		setDefaultValue(SYNTAXCOLOR_UNDERLINE + SYNTAXCOLOR_STRING,     false);
		setDefaultValue(SYNTAXCOLOR_UNDERLINE + SYNTAXCOLOR_COMMENT,    false);
		setDefaultValue(SYNTAXCOLOR_UNDERLINE + SYNTAXCOLOR_FUNCTION,   false);
		setDefaultValue(SYNTAXCOLOR_UNDERLINE + SYNTAXCOLOR_DATATYPE,   false);
		setDefaultValue(SYNTAXCOLOR_UNDERLINE + SYNTAXCOLOR_VAR,        false);
		setDefaultValue(SYNTAXCOLOR_UNDERLINE + SYNTAXCOLOR_PARAM,      false);
		setDefaultValue(SYNTAXCOLOR_UNDERLINE + SYNTAXCOLOR_FIELD,      false);
		setDefaultValue(SYNTAXCOLOR_UNDERLINE + SYNTAXCOLOR_INTERFACE,  false);
		setDefaultValue(SYNTAXCOLOR_UNDERLINE + SYNTAXCOLOR_CONSTRUCTOR,false);
		
		setDefaultValue(SYNTAXCOLOR_STRIKETHROUGH + SYNTAXCOLOR_KEYWORD,    false);
		setDefaultValue(SYNTAXCOLOR_STRIKETHROUGH + SYNTAXCOLOR_STRING,     false);
		setDefaultValue(SYNTAXCOLOR_STRIKETHROUGH + SYNTAXCOLOR_COMMENT,    false);
		setDefaultValue(SYNTAXCOLOR_STRIKETHROUGH + SYNTAXCOLOR_FUNCTION,   false);
		setDefaultValue(SYNTAXCOLOR_STRIKETHROUGH + SYNTAXCOLOR_DATATYPE,   false);
		setDefaultValue(SYNTAXCOLOR_STRIKETHROUGH + SYNTAXCOLOR_VAR,        false);
		setDefaultValue(SYNTAXCOLOR_STRIKETHROUGH + SYNTAXCOLOR_PARAM,      false);
		setDefaultValue(SYNTAXCOLOR_STRIKETHROUGH + SYNTAXCOLOR_FIELD,      false);
		setDefaultValue(SYNTAXCOLOR_STRIKETHROUGH + SYNTAXCOLOR_INTERFACE,  false);
		setDefaultValue(SYNTAXCOLOR_STRIKETHROUGH + SYNTAXCOLOR_CONSTRUCTOR,false);
		
		//Default Maude directory is dependent of operating system
		if(Platform.getOS().equals(Platform.OS_WIN32)){
			setDefaultValue(MAUDE_EXEC_PATH, DEFAULT_MAUDE_EXEC_PATH_WIN32);
		} else if(Platform.getOS().equals(Platform.OS_LINUX)){
			setDefaultValue(MAUDE_EXEC_PATH, DEFAULT_MAUDE_EXEC_PATH_LINUX);
		} else if(Platform.getOS().equals(Platform.OS_MACOSX)){
			setDefaultValue(MAUDE_EXEC_PATH, DEFAULT_MAUDE_EXEC_PATH_MACOSX);
		} else{
			setDefaultValue(MAUDE_EXEC_PATH, DEFAULT_MAUDE_EXEC_PATH);
		}
		
//		setDefaultValue(MAUDE_PARTIAL_EXECUTION, false);
//		setDefaultValue(MAUDE_STEP_NUMBER, 0);
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.ui.plugin.AbstractUIPlugin#stop(org.osgi.framework.BundleContext)
	 */
	@Override
	public void stop(BundleContext context) throws Exception {
		ResourcesPlugin.getWorkspace().removeResourceChangeListener(resourceChangeTracker);
		super.stop(context);
	}

	//-------------------- INIT ------------------------------------------------------------------------------
	
	public static void initializeColors(){
		ColorRegistry colorRegistry = JFaceResources.getColorRegistry();
		colorRegistry.put(STYLER_COLOR_BLACK, STYLER_COLOR_BLACK_RGB);
		colorRegistry.put(STYLER_COLOR_GREY, STYLER_COLOR_GREY_RGB);
		colorRegistry.put(STYLER_COLOR_TYPES, STYLER_COLOR_TYPES_RGB);
	}

	/**
	 * Returns the shared instance
	 *
	 * @return the shared instance
	 */
	public static Activator getDefault() {
		return plugin;
	}

	public static void logException(Exception e) {
		getDefault().getLog().log(new Status(IStatus.ERROR, Constants.PLUGIN_ID, e.getMessage(), e));
	}
}
