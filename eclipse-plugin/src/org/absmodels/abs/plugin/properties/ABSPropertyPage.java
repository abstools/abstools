/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.absmodels.abs.plugin.properties;

import static org.absmodels.abs.plugin.util.Constants.DEFAULT_LOCATION_TYPE;
import static org.absmodels.abs.plugin.util.Constants.LOCATION_TYPECHECK;
import static org.absmodels.abs.plugin.util.Constants.LOCATION_TYPE_OVERLAY;
import static org.absmodels.abs.plugin.util.Constants.LOCATION_TYPE_PRECISION;
import static org.absmodels.abs.plugin.util.Constants.PRODUCT_TYPECHECK;
import static org.absmodels.abs.plugin.util.UtilityFunctions.getAbsNature;
import static org.absmodels.abs.plugin.util.UtilityFunctions.syncPreferenceStore;

import org.eclipse.core.resources.IProject;
import org.eclipse.jface.preference.BooleanFieldEditor;
import org.eclipse.jface.preference.ComboFieldEditor;
import org.eclipse.jface.preference.IPersistentPreferenceStore;
import org.eclipse.jface.util.IPropertyChangeListener;
import org.eclipse.jface.util.PropertyChangeEvent;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.ui.dialogs.PropertyPage;

import abs.frontend.typechecker.locationtypes.LocationType;
import abs.frontend.typechecker.locationtypes.infer.LocationTypeInferrerExtension.LocationTypingPrecision;

/**
 * Property page for project specific ABS properties, i.e. location type checking.
 * @author tfischer
 */
public class ABSPropertyPage extends PropertyPage{

	private BooleanFieldEditor locationTypecheckEditor;
	private BooleanFieldEditor typeCheckProductsEditor;
	private ComboFieldEditor defaultLocationTypeEditor;
	private IProject project;
	private IPersistentPreferenceStore prefstore;
	private BooleanFieldEditor locationTypeOverlayEditor;
    private ComboFieldEditor locationTypePrecisionEditor;
	
	public ABSPropertyPage(){
		super();
	}
	
	@Override
	protected Control createContents(Composite parent) {
		project = (IProject)getElement();
		
		GridLayout gridLayout = new GridLayout(1, false);
		parent.setLayout(gridLayout);
		
//		ArrayList<String[]> locations = new ArrayList<String[]>();
//		for(LocationType ltype : LocationType.ALLUSERTYPES){
//			locations.add(new String[]{ltype.toString(),ltype.toString()});
//		}
		
		prefstore = getAbsNature(project).getProjectPreferenceStore();
		
		locationTypecheckEditor = new BooleanFieldEditor(LOCATION_TYPECHECK, "Enable location type checking", createContainer(parent));
		locationTypecheckEditor.setPreferenceStore(prefstore);
		locationTypecheckEditor.load();
		
		typeCheckProductsEditor = new BooleanFieldEditor(PRODUCT_TYPECHECK, "Enable products type checking", createContainer(parent));
		typeCheckProductsEditor.setPreferenceStore(prefstore);
		typeCheckProductsEditor.load();
		
		final Composite locationTypeOverlayContainer = createContainer(parent);
		locationTypeOverlayEditor = new BooleanFieldEditor(LOCATION_TYPE_OVERLAY, "Show overlays", locationTypeOverlayContainer);
		locationTypeOverlayEditor.setPreferenceStore(prefstore);
		locationTypeOverlayEditor.load();

		final Composite defaultLocationContainer = createContainer(parent);
		
		defaultLocationTypeEditor = createComboFieldEditor(defaultLocationContainer, DEFAULT_LOCATION_TYPE, "Default location type", (Object[])LocationType.ALLUSERTYPES);
		
		final Composite locationTypePrecisionContainer = createContainer(parent);
        
		locationTypePrecisionEditor = createComboFieldEditor(locationTypePrecisionContainer, LOCATION_TYPE_PRECISION, "Location type precision", (Object[])LocationTypingPrecision.values());
		
		if(!locationTypecheckEditor.getBooleanValue()){
			defaultLocationTypeEditor.setEnabled(false,defaultLocationContainer);
			locationTypePrecisionEditor.setEnabled(false,locationTypePrecisionContainer);
			locationTypeOverlayEditor.setEnabled(false, locationTypeOverlayContainer);
		}
		
		locationTypecheckEditor.setPropertyChangeListener(new IPropertyChangeListener() {
			
			@Override
			public void propertyChange(PropertyChangeEvent event) {
			    boolean b = (Boolean) event.getNewValue();
			    defaultLocationTypeEditor.setEnabled(b, defaultLocationContainer);
			    locationTypePrecisionEditor.setEnabled(b, locationTypePrecisionContainer);
			    locationTypeOverlayEditor.setEnabled(b, locationTypeOverlayContainer);
			}
		});
		
		return parent;
	}

    private ComboFieldEditor createComboFieldEditor(
            final Composite defaultLocationContainer, String defaultName, String defaultString, Object... optionValues) {
        String[][] options = new String[optionValues.length][];
		for(int i = 0; i < optionValues.length; i++){
			options[i] = new String[]{optionValues[i].toString(),optionValues[i].toString()};
		}
		ComboFieldEditor editor = new ComboFieldEditor(defaultName, defaultString, options, defaultLocationContainer);
		editor.setPreferenceStore(prefstore);
		editor.load();
        return editor;
    }

	private Composite createContainer(Composite parent) {
	   Composite cont = new Composite(parent, SWT.NONE);
		GridData gridData = new GridData(GridData.FILL_HORIZONTAL);
		cont.setLayoutData(gridData);
	   return cont;
   }
	
	@Override
	protected void performDefaults() {
		locationTypecheckEditor.loadDefault();
		typeCheckProductsEditor.loadDefault();
		defaultLocationTypeEditor.loadDefault();
		locationTypePrecisionEditor.loadDefault();
		locationTypeOverlayEditor.loadDefault();
		super.performDefaults();
	}
	
	@Override
	public boolean performOk() {
		locationTypecheckEditor.store();
		typeCheckProductsEditor.store();
		defaultLocationTypeEditor.store();
		locationTypePrecisionEditor.store();
		locationTypeOverlayEditor.store();
		syncPreferenceStore(prefstore);
		return super.performOk();
	}
	
}
