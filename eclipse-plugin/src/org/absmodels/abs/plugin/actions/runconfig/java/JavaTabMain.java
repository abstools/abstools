package org.absmodels.abs.plugin.actions.runconfig.java;

import static org.absmodels.abs.plugin.util.UtilityFunctions.showErrorMessage;
import static org.absmodels.abs.plugin.util.UtilityFunctions.standardExceptionHandling;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import org.absmodels.abs.plugin.actions.runconfig.AbstractTab;
import org.absmodels.abs.plugin.builder.AbsNature;
import org.absmodels.abs.plugin.util.Images;
import org.absmodels.abs.plugin.util.UtilityFunctions;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.debug.core.ILaunchConfiguration;
import org.eclipse.debug.core.ILaunchConfigurationWorkingCopy;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Listener;

import abs.backend.tests.ABSTestRunnerGenerator;
import abs.frontend.ast.CompilationUnit;
import abs.frontend.ast.Model;
import abs.frontend.ast.ModuleDecl;

/**
 * The first Tab for the java launch configuration 
 */
public class JavaTabMain extends AbstractTab {
    
    private Combo runTargetDropDown;

    private Button sequenceDiagramCheckbox;

    private List<RunTarget> runTargets;

    
    @Override
    public void createControl(Composite parent) {
        TabListener myListener = new TabListener(this);
        Composite comp = new Composite(parent, SWT.NONE);
        
        setCompositeLayout(comp);
        createProjectDropDownMenu(myListener, comp);
        createProductDropDownMenu(myListener, comp);
        createRunTargetDropDownMenu(myListener, comp);
        
        
        createSequenceDiagramCheckbox(myListener, comp);
        
        
        
        setControl(comp);
    }
    
   

    protected void createProjectDropDownMenu(TabListener myListener, Composite comp) {
        super.createProjectDropDownMenu(myListener, comp);
        // add listener to change the run target selection
        projectDropDown.addListener(SWT.Selection, new Listener() {
            @Override
            public void handleEvent(Event event) {
                fillRunTargetMenu(null);
            }
        });
    }

    

    private void createSequenceDiagramCheckbox(TabListener myListener, Composite comp) {
        sequenceDiagramCheckbox = createCheckButton(comp, "Draw sequence diagram");
        sequenceDiagramCheckbox.addListener(SWT.Selection, myListener);
    }
    
    private String getSelectedItem(Combo combo) {
        int index = combo.getSelectionIndex();
        if (index >= 0) {
            return combo.getItem(index);
        } else {
            return "";
        }
    }
    
    private RunTarget getSelectedRunTarget() {
        int index = runTargetDropDown.getSelectionIndex();
        if (index >= 0) {
            return runTargets.get(index);
        } else {
            return null;
        }
    }

    private void fillRunTargetMenu(String preSelected) {
        int selected = runTargetDropDown.getSelectionIndex();
        if (preSelected != null) {
            selected = 0;
        }
        runTargetDropDown.removeAll();
        IProject proj = getSelectedProject();
        runTargets = getRunTargets(proj);
        
        int i = 0;
        for (RunTarget runTarget : runTargets) {
            
            runTargetDropDown.add(runTarget.toString());
            if (runTarget.toString().equals(preSelected)) {
                selected = i;
            }
            i++;
        } 
        runTargetDropDown.select(selected);
        runTargetDropDown.setEnabled(runTargets.size() > 0);
    }


    private List<RunTarget> getRunTargets(IProject proj) {
        AbsNature n = UtilityFunctions.getAbsNature(proj);
        if (n == null) {
            return Collections.emptyList();
        }
        Model m = n.getCompleteModel();
        if (m == null) {
            return Collections.emptyList();
        }
        List<RunTarget> result = new ArrayList<RunTarget>();
        for (CompilationUnit cu : m.getCompilationUnits()) {
            for (ModuleDecl module : cu.getModuleDecls()) {
                if (module.getBlockOpt().hasChildren()) {
                    if (module.getName().equals(ABSTestRunnerGenerator.RUNNER_MAIN)) {
                        // do not show generated unit test file
                        continue;
                    }
                    result.add(new RunTargetModule(module));
                }
                // TODO add unit test classes
//                for (Decl d : module.getDecls()) {
//                    if (d.isInterface()) {
//                        InterfaceDecl i = (InterfaceDecl) d;
//                        for (Annotation annotation : i.getAnnotations()) {
//                            if (annotation.getValue() )
//                        }
//                    }
//                }
            }
        }
        result.add(new RunTargetUnitTests());
        return result;
    }


    private void createRunTargetDropDownMenu(TabListener myListener, Composite comp) {
        Group group = createGroup(comp, "Run Target", 1, 1, GridData.FILL_HORIZONTAL);

        runTargetDropDown = new Combo(group, SWT.DROP_DOWN | SWT.BORDER | SWT.READ_ONLY);
        GridData gridData = new GridData();
        gridData.widthHint = 200;
        runTargetDropDown.setLayoutData(gridData);

        runTargetDropDown.addListener(SWT.Selection, myListener);
        
    }


    @Override
    public void setDefaults(ILaunchConfigurationWorkingCopy configuration) { 
        new JavaLaunchConfig(configuration).setDefaults();
    }


    @Override
    public void initializeFrom(ILaunchConfiguration configuration) {
        JavaLaunchConfig cfg = new JavaLaunchConfig(configuration);
        try {
            fillProjectDropDownMenue(cfg.getProjectName());
            fillProductDropDownMenue(cfg.getProductName());
            fillRunTargetMenu(cfg.getRunTarget());
            sequenceDiagramCheckbox.setSelection(cfg.getDrawSequenceDiagram());
            initProject(configuration);
        } catch (CoreException e) {
            standardExceptionHandling(e);
            showErrorMessage("Fatal error!");
        }
                  
    }

    @Override
    public void performApply(ILaunchConfigurationWorkingCopy configuration) {
        JavaLaunchConfig cfg = new JavaLaunchConfig(configuration);
        try {
            cfg.setProjectName(getSelectedItem(projectDropDown));
            cfg.setProductName(getSelectedProductName());
            cfg.setRunTarget(getSelectedItem(runTargetDropDown));
            RunTarget runTarget = getSelectedRunTarget();
            if (runTarget != null) {
            	runTarget.setConfig(cfg);
            }
            cfg.setDrawSequenceDiagram(sequenceDiagramCheckbox.getSelection());
        } catch (CoreException e) {
            standardExceptionHandling(e);
            showErrorMessage("Fatal error!");
        }
    }





    



    @Override
    public String getName() {
        return "ABS Java Backend";
    }

    @Override
    public boolean isValid(ILaunchConfiguration launchConfig) {
        boolean res = super.isValid(launchConfig);
        if (res) {
            setErrorMessage(null);
        } else {
        	if (getSelectedRunTarget() == null) {
        		setErrorMessage("No run target selected");
        		return false;
        	}
        }
        return res;
    }

    private void setCompositeLayout(Composite comp) {
        GridLayout gridLayout = new GridLayout(1, false);
        gridLayout.verticalSpacing = 8;
        comp.setLayout(gridLayout);
    }

    
    @Override
    public String getId() {
        return "org.abs-models.abs.plugin.javaTab1";
    }
    
    @Override
    public Image getImage() {
        return Images.MODULE_IMAGE;
    }

}
