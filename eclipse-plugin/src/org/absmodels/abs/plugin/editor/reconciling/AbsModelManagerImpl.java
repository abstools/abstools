package org.absmodels.abs.plugin.editor.reconciling;

import static org.absmodels.abs.plugin.util.Constants.DEFAULT_LOCATION_TYPE;
import static org.absmodels.abs.plugin.util.Constants.LOCATION_TYPECHECK;
import static org.absmodels.abs.plugin.util.Constants.LOCATION_TYPE_PRECISION;

import java.util.ArrayList;
import java.util.List;

import org.absmodels.abs.plugin.Activator;
import org.absmodels.abs.plugin.builder.AbsNature;
import org.absmodels.abs.plugin.internal.IncrementalModelBuilder;
import org.absmodels.abs.plugin.util.Constants;
import org.absmodels.abs.plugin.util.CoreControlUnit;
import org.absmodels.abs.plugin.util.CoreControlUnit.ResourceBuildListener;
import org.absmodels.abs.plugin.util.CoreControlUnit.ResourceBuiltEvent;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IncrementalProjectBuilder;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.jface.preference.IPersistentPreferenceStore;

import abs.frontend.analyser.SemanticErrorList;
import abs.frontend.ast.CompilationUnit;
import abs.frontend.ast.Model;
import abs.frontend.parser.Main;
import abs.frontend.typechecker.locationtypes.LocationType;
import abs.frontend.typechecker.locationtypes.infer.LocationTypeInferrerExtension;
import abs.frontend.typechecker.locationtypes.infer.LocationTypeInferrerExtension.LocationTypingPrecision;

public class AbsModelManagerImpl implements AbsModelManager, ResourceBuildListener {

    private AbsNature absNature;

    private Model model;
    private List<ModelChangeListener> changeListeners = new ArrayList<ModelChangeListener>();

    public AbsModelManagerImpl(AbsNature absNature) {
        this.absNature = absNature;
        CoreControlUnit.addResourceBuildListener(this);
        updateModelFromNature();
    }

    @Override
    public synchronized void updateModel(CompilationUnit cu, boolean withTypechecks) {
        boolean added = false;
        for (int i=0; i<model.getNumCompilationUnit(); i++) {
            CompilationUnit c = model.getCompilationUnit(i);
            if (c.getFileName().equals(cu.getFileName())) {
                // update compilation unit:
                model.setCompilationUnit(cu, i);
                added = true;
                break;
            }
            
        }
        if (!added) {
            model.addCompilationUnit(cu);
        }

        Main.exceptionHack(model);
        if (!model.hasParserErrors()) {
            if (withTypechecks) {
                extendedTypechecking();
            }
        }

        notifyChangeListeners();
    }

    private void notifyChangeListeners() {
        for (ModelChangeListener cl : changeListeners) {
            cl.onModelChange(model);
        }
    }

    private void extendedTypechecking() {
        if (absNature.getProject() != null) {
            IPersistentPreferenceStore projectPreferences = absNature.getProjectPreferenceStore();
            IncrementalModelBuilder.flushAll(model);
            model.getTypeExt().clearTypeSystemExtensions();
            boolean dolocationtypecheck = projectPreferences.getBoolean(LOCATION_TYPECHECK);
            if (dolocationtypecheck) {

                String defaultlocationtypeprecision = projectPreferences.getString(LOCATION_TYPE_PRECISION);
                LocationType defaultLocType = LocationType.createFromName(projectPreferences.getString(DEFAULT_LOCATION_TYPE));
                LocationTypeInferrerExtension ltie = new LocationTypeInferrerExtension(model);
                ltie.setDefaultType(defaultLocType);
                ltie.setLocationTypingPrecision(LocationTypingPrecision.valueOf(defaultlocationtypeprecision));
                model.registerTypeSystemExtension(ltie);
            }
        }
        Main.exceptionHack(model);
        SemanticErrorList typeErrors = model.typeCheck();

        updateMarkers(typeErrors);
    }

    private void updateMarkers(SemanticErrorList typeErrors) {
        // update markers
        try {
            if (absNature.getProject() == null) { 
                return;
            }
            absNature.getProject().deleteMarkers(Constants.TYPECHECK_MARKER_TYPE, true, IResource.DEPTH_INFINITE);
            absNature.createMarkers(typeErrors);
        } catch (CoreException e) {
            Activator.logException(e);
        }
    }

    @Override
    public synchronized Model getModel() {
        return model;
    }

    @Override
    public synchronized CompilationUnit getCompilationUnit(String absoluteFilePath) {
        if (model == null) {
        	/* Project hasn't been built yet. */
        	try {
				absNature.getProject().build(IncrementalProjectBuilder.FULL_BUILD, null);
			} catch (CoreException e) {
				Activator.logException(e);
				return null;
			}
        	updateModelFromNature();
        }
        assert model != null;
        for (CompilationUnit cu : model.getCompilationUnits()) {
            if (cu.getFileName().equals(absoluteFilePath)) {
                return cu;
            }
        }
        return null;
    }



    @Override
    public synchronized void resourceBuilt(ResourceBuiltEvent builtevent) {
        updateModelFromNature();
    }



    private void updateModelFromNature() {
        Model build = absNature.getCompleteModel();
        if (build != null && model != build) {
            model = build.parseTreeCopy();
            // XXX Check if parseTreeCopy does the right thing.
            Main.exceptionHack(model);
            notifyChangeListeners();
        }
    }

}
