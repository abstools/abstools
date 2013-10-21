/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.absmodels.abs.plugin;
import java.io.IOException;
import java.util.Map;

import org.absmodels.abs.plugin.builder.AbsNature;
import org.absmodels.abs.plugin.util.Constants;
import org.absmodels.abs.plugin.util.UtilityFunctions;
import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.preference.IPersistentPreferenceStore;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.swt.widgets.Display;
import org.eclipse.ui.handlers.HandlerUtil;

import abs.frontend.typechecker.locationtypes.LocationType;
import abs.frontend.typechecker.locationtypes.infer.InferMain;
import abs.frontend.typechecker.locationtypes.infer.LocationTypeVariable;


public class ProjectAddLocationHandler extends AbstractHandler  {

	@Override
	public Object execute(ExecutionEvent event) throws ExecutionException {
		IStructuredSelection selection = (IStructuredSelection) HandlerUtil
			.getActiveMenuSelection(event);
		Object firstElement = selection.getFirstElement();
		if(firstElement instanceof IProject){
			IProject project = (IProject)firstElement;
			AbsNature nature = UtilityFunctions.getAbsNature(project);
			if(nature==null)
				return null;
			IPersistentPreferenceStore projectStore = nature.getProjectPreferenceStore();
			boolean locationTypecheckingEnabled = projectStore.getBoolean(Constants.LOCATION_TYPECHECK);
			if(!locationTypecheckingEnabled){
				MessageDialog.openInformation(Display.getDefault().getActiveShell(), "Locationtypechecking", "Location type checking is disabled. Please enable for this function to work!");
				return null;
			}
			UtilityFunctions.saveEditors(project, false);
			Map<LocationTypeVariable, LocationType> locationTypeInferrerResult = nature.getLocationTypeInferrerResult();
			if(locationTypeInferrerResult!=null){
				InferMain inferMain = new InferMain();
				String commandId = event.getCommand().getId();
				if("org.abs-models.abs.plugin.projectaddalllocations".equals(commandId)){
					inferMain.setConfig(InferMain.Config.values());
				} else if("org.abs-models.abs.plugin.projectaddclasslocations".equals(commandId)){
					inferMain.setConfig(InferMain.Config.CLASSES);
				} else if("org.abs-models.abs.plugin.projectaddfieldlocations".equals(commandId)){
					inferMain.setConfig(InferMain.Config.FIELDS);
				} else if("org.abs-models.abs.plugin.projectaddfunctionlocations".equals(commandId)){
					inferMain.setConfig(InferMain.Config.FUNCTIONS);
				} else if("org.abs-models.abs.plugin.projectaddinterfacelocations".equals(commandId)){
					inferMain.setConfig(InferMain.Config.INTERFACES);
				}
				try {
					inferMain.writeInferenceResultsBack(locationTypeInferrerResult);
					try {
						project.refreshLocal(IResource.DEPTH_INFINITE, null);
					} catch (CoreException e) {
					}
				} catch (IOException e) {
					MessageDialog.openError(Display.getDefault().getActiveShell(), "Error while inserting locations", 
							"An error occurred while inserting locations!\n"+e.getLocalizedMessage());
				}
			}
		}
		return null;
	}

}
