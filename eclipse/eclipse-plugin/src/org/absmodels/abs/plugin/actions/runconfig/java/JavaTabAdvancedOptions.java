package org.absmodels.abs.plugin.actions.runconfig.java;

import static org.absmodels.abs.plugin.util.UtilityFunctions.showErrorMessage;
import static org.absmodels.abs.plugin.util.UtilityFunctions.standardExceptionHandling;

import org.absmodels.abs.plugin.actions.runconfig.AbstractTab;
import org.absmodels.abs.plugin.util.Images;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.debug.core.ILaunchConfiguration;
import org.eclipse.debug.core.ILaunchConfigurationWorkingCopy;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;


/**
 * Tab for setting advanced options 
 */
public class JavaTabAdvancedOptions extends AbstractTab {


    private Button compileBeforeCheckbox;
    private Button ignoreMissingFLIClassesCheckbox;

    @Override
    public void createControl(Composite parent) {
        TabListener myListener = new TabListener(this);
        Composite comp = new Composite(parent, SWT.NONE);
        setCompositeLayout(comp);
        
        createCompileBeforeCheckbox(myListener, comp);
        createFLIignoreMissingClassesCheckbox(myListener, comp);
        
        setControl(comp);

    }

    private void createFLIignoreMissingClassesCheckbox(TabListener myListener, Composite comp) {
        ignoreMissingFLIClassesCheckbox = createCheckButton(comp, "Ignore missing [Foreign] classes");
        ignoreMissingFLIClassesCheckbox.addListener(SWT.Selection, myListener);
    }

    private void createCompileBeforeCheckbox(TabListener myListener, Composite comp) {
        compileBeforeCheckbox = createCheckButton(comp, "Skip compilation before run");
        compileBeforeCheckbox.addListener(SWT.Selection, myListener);
    }
    

    private void setCompositeLayout(Composite comp) {
        GridLayout gridLayout = new GridLayout(1, false);
        gridLayout.verticalSpacing = 8;
        comp.setLayout(gridLayout);
    }

    @Override
    public void setDefaults(ILaunchConfigurationWorkingCopy configuration) {
        new JavaLaunchConfig(configuration).setDefaults();
    }

    @Override
    public void initializeFrom(ILaunchConfiguration configuration) {
        JavaLaunchConfig cfg = new JavaLaunchConfig(configuration);
        try {
            compileBeforeCheckbox.setSelection( ! cfg.getCompileBefore());
            ignoreMissingFLIClassesCheckbox.setSelection(cfg.getIgnoreMissingFLIClasses());
        } catch (CoreException e) {
            standardExceptionHandling(e);
            showErrorMessage("Fatal error!");
        }
    }


    @Override
    public void performApply(ILaunchConfigurationWorkingCopy configuration) {
        JavaLaunchConfig cfg = new JavaLaunchConfig(configuration);
        cfg.setCompileBefore( ! compileBeforeCheckbox.getSelection());
        cfg.setIgnoreMissingFLIClasses(ignoreMissingFLIClassesCheckbox.getSelection());
    }

    

    @Override
    public String getName() {
        return "Advanced";
    }

    @Override
    public boolean isValid(ILaunchConfiguration launchConfig) {
        return true;
    }
    
    @Override
    public String getId() {
        return "org.abs-models.abs.plugin.javaTab4";
    }

    @Override
    public Image getImage() {
        return Images.MAIN_BLOCK_IMAGE;
    }

}
