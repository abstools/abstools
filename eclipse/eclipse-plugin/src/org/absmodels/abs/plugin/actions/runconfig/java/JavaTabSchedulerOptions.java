package org.absmodels.abs.plugin.actions.runconfig.java;

import static org.absmodels.abs.plugin.actions.runconfig.java.EclipseScheduler.HISTORY;
import static org.absmodels.abs.plugin.actions.runconfig.java.EclipseScheduler.MANUAL;
import static org.absmodels.abs.plugin.actions.runconfig.java.EclipseScheduler.RANDOM;
import static org.absmodels.abs.plugin.util.UtilityFunctions.showErrorMessage;
import static org.absmodels.abs.plugin.util.UtilityFunctions.standardExceptionHandling;

import java.io.File;

import org.absmodels.abs.plugin.actions.runconfig.AbstractTab;
import org.absmodels.abs.plugin.util.Images;
import org.absmodels.abs.plugin.util.UtilityFunctions;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.debug.core.ILaunchConfiguration;
import org.eclipse.debug.core.ILaunchConfigurationWorkingCopy;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.FileDialog;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;


/**
 * tab for choosing a scheduler and scheduler options 
 */
public class JavaTabSchedulerOptions extends AbstractTab {

    private Button manualSchedulerBtn;
    private Button randomSchedulerBtn;
    private Button historySchedulerBtn;
    private Button runAutomaticallyCheckbox;
    private Button useFifoSemanticsCheckbox;
    private Button useFixedSeed;
    private Text seedNumber;
    private Text historyFileName;
    private Button historyButton;

    @Override
    public void createControl(Composite parent) {
        TabListener myListener = new TabListener(this);
        Composite comp = new Composite(parent, SWT.NONE);
        setCompositeLayout(comp);
        
        createSchedulerSelection(comp, myListener);
        createOptions(comp, myListener);
        createFifoCheckbox(comp, myListener);
        
        
        setControl(comp);

    }

    

    private void createRunAutomaticallyCheckbox(Composite comp, TabListener myListener) {
        runAutomaticallyCheckbox = createCheckButton(comp, "Run automatically");
        runAutomaticallyCheckbox.setImage(Images.DEBUGGER_RESUME);
        runAutomaticallyCheckbox.addListener(SWT.Selection, myListener);
        
    }
    
    private void createFifoCheckbox(Composite comp, TabListener myListener) {
        Group group = createGroup(comp, "Advanced", 1, 1, GridData.FILL_HORIZONTAL);
        GridLayout layout = (GridLayout) group.getLayout();
        layout.verticalSpacing = 8;
        
        useFifoSemanticsCheckbox = createCheckButton(group, "Guarantee message ordering between two COGs");
        useFifoSemanticsCheckbox.addListener(SWT.Selection, myListener);
    }

    private void setCompositeLayout(Composite comp) {
        GridLayout gridLayout = new GridLayout(1, false);
        gridLayout.verticalSpacing = 8;
        comp.setLayout(gridLayout);
    }

    private void createSchedulerSelection(Composite comp, TabListener myListener) {
        Group group = createGroup(comp, "Scheduler", 1, 1, GridData.FILL_HORIZONTAL);
        
        
        // manual scheduler:
        manualSchedulerBtn = createRadioButton(group, "Run with manual scheduler");
        manualSchedulerBtn.setImage(Images.DEBUGGER_INTERACTIVE);
        manualSchedulerBtn.setSelection(true);
        manualSchedulerBtn.addListener(SWT.Selection, myListener);
        manualSchedulerBtn.setLayoutData(new GridData(GridData.FILL_HORIZONTAL));
        
        // randomScheduler
        randomSchedulerBtn = createRadioButton(group, "Run with random scheduler");
        randomSchedulerBtn.setImage(Images.DEBUGGER_RANDOM);
        randomSchedulerBtn.addListener(SWT.Selection, myListener);
        randomSchedulerBtn.setLayoutData(new GridData(GridData.FILL_HORIZONTAL));
        
        
        
        // run history:
        historySchedulerBtn = createRadioButton(group, "Replay history");
        historySchedulerBtn.setImage(Images.DEBUGGER_HISTORY);
        historySchedulerBtn.addListener(SWT.Selection, myListener);
        historySchedulerBtn.setLayoutData(new GridData(GridData.FILL_HORIZONTAL));

        
    }

    private void createOptions(Composite comp, TabListener myListener) {
        Group group = createGroup(comp, "Options", 1, 1, GridData.FILL_HORIZONTAL);
        GridLayout layout = (GridLayout) group.getLayout();
        layout.verticalSpacing = 8;
        
        createRunAutomaticallyCheckbox(group, myListener);
        
        createRandomSeedChooser(group, myListener);
        
        createHistorFileChooser(group, myListener);
        
        
        
    }



    



    private void createRandomSeedChooser(Group comp, TabListener myListener) {
        GridData gridData = new GridData();
        gridData.widthHint = 100;
        gridData.verticalAlignment = SWT.TOP;
        Composite seedComp = new Composite(comp, SWT.NONE);
        GridLayout layout = new GridLayout(2, false);
        layout.marginWidth = 0;
        layout.marginHeight = 0;
        seedComp.setLayout(layout);
        
        useFixedSeed = createCheckButton(seedComp, "Use fixed seed:");
        useFixedSeed.addListener(SWT.Selection, myListener);
        
        seedNumber = new Text(seedComp, SWT.SINGLE | SWT.BORDER);
        seedNumber.setText("0");
        seedNumber.addListener(SWT.Modify, myListener);   
        gridData.widthHint = 100;
        seedNumber.setLayoutData(gridData);
    }



    private void createHistorFileChooser(Composite comp, TabListener myListener) {
        Composite historyComp = new Composite(comp, SWT.FILL);
        GridData g = new GridData();
        g.horizontalAlignment = SWT.FILL;
        g.grabExcessHorizontalSpace = true;
        historyComp.setLayoutData(g);
        
        GridLayout layout = new GridLayout(2, false);
//        layout.marginLeft = 0;
        layout.marginWidth = 0;
        layout.marginHeight = 0;
        layout.verticalSpacing = 0;
        
        Label historyLabel = new Label(historyComp, SWT.NONE);
        historyLabel.setText("History File:");
        GridData gridData = new GridData();
        gridData.horizontalSpan = 2;
        historyLabel.setLayoutData(gridData);
        
        historyComp.setLayout(layout);
        historyFileName = new Text(historyComp, SWT.SINGLE | SWT.BORDER);
        historyFileName.addListener(SWT.Modify, myListener);
        
        gridData = new GridData();
//        gridData.verticalAlignment = SWT.TOP;
        gridData.widthHint = 100;
        gridData.horizontalAlignment = SWT.FILL;
        gridData.grabExcessHorizontalSpace = true;
        historyFileName.setLayoutData(gridData);
        
        historyButton = new Button(historyComp, SWT.PUSH);
        historyButton.setText("Browse");
        gridData = new GridData();
        //gridData.widthHint = 100;
        historyButton.setLayoutData(gridData);
        historyButton.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
              FileDialog historyDialog = new FileDialog(getShell(), SWT.OPEN);
              String historyFile = historyDialog.open();
              if (historyFile != null) {
                historyFileName.setText(historyFile);
              }
            }
          });
    }
    
    
    @Override
    public void setDefaults(ILaunchConfigurationWorkingCopy configuration) {
        new JavaLaunchConfig(configuration).setDefaults();
    }

    @Override
    public void initializeFrom(ILaunchConfiguration configuration) {
        JavaLaunchConfig cfg = new JavaLaunchConfig(configuration);
        try {
            EclipseScheduler scheduler = cfg.getScheduler();
            selectScheduler(scheduler);
            seedNumber.setText(cfg.getRandomSeed() + "");
            useFixedSeed.setSelection(cfg.hasFixedRandomSeed());
            historyFileName.setText(cfg.getHistoryFile());
            runAutomaticallyCheckbox.setSelection(cfg.getRunAutomatically());
            useFifoSemanticsCheckbox.setSelection(cfg.getUseFifoSemantics());
        } catch (CoreException e) {
            standardExceptionHandling(e);
            showErrorMessage("Fatal error!");
        }
        
        updateUI();
    }

    private void selectScheduler(EclipseScheduler scheduler) {
        historySchedulerBtn.setSelection(scheduler == HISTORY);
        manualSchedulerBtn.setSelection(scheduler == MANUAL);
        randomSchedulerBtn.setSelection(scheduler == RANDOM);
    }

    private EclipseScheduler getSelectedScheduler() {
       if (historySchedulerBtn.getSelection()) {
           return EclipseScheduler.HISTORY;
       } 
       if (randomSchedulerBtn.getSelection()) {
           return EclipseScheduler.RANDOM;
       }
       return EclipseScheduler.MANUAL;
    }

    @Override
    public void performApply(ILaunchConfigurationWorkingCopy configuration) {
        JavaLaunchConfig cfg = new JavaLaunchConfig(configuration);
        cfg.setScheduler(getSelectedScheduler());
        if (randomSchedulerBtn.getSelection() && useFixedSeed.getSelection()) {
            cfg.setRandomSeed(seedNumber.getText());
        } else {
            cfg.unsetRandomSeed();
        }
        if (historySchedulerBtn.getSelection()) {
            cfg.setHistoryFile(historyFileName.getText());
        } else {
            cfg.setHistoryFile("");
        }
        if (!manualSchedulerBtn.getSelection()) {
            cfg.setRunAutomatically(runAutomaticallyCheckbox.getSelection());
        } else {
            cfg.setRunAutomatically(false);
        }
        cfg.setUseFifoSemantics(useFifoSemanticsCheckbox.getSelection());
    }

    

    @Override
    public String getName() {
        return "Scheduler Options";
    }

    @Override
    public boolean isValid(ILaunchConfiguration configuration) {
        JavaLaunchConfig cfg = new JavaLaunchConfig(configuration);
        try {
            // check if history file exists and has correct format
            if (cfg.getScheduler() == HISTORY) {
                if (!new File(cfg.getHistoryFile()).exists()) {
                    setErrorMessage("Could not find history file.");
                    return false;
                }
            }
            // check if seedNumber is a number
            if (cfg.hasFixedRandomSeed()) {
                if (!UtilityFunctions.isNumber(cfg.getRandomSeedString())) {
                    setErrorMessage("Random seed must be a number.");
                    return false;
                }
            }
            setErrorMessage(null);
            return true;
        } catch (CoreException e) {
            UtilityFunctions.standardExceptionHandling(e);
            setErrorMessage("Unexpected problem.");
            return false;
        }
    }

    @Override
    protected void updateLaunchConfigurationDialog() {
        super.updateLaunchConfigurationDialog();
        
        updateUI();
    }

    private void updateUI() {
        EclipseScheduler s = getSelectedScheduler();
        
        useFixedSeed.setEnabled(s == RANDOM);
        seedNumber.setEnabled(s == RANDOM && useFixedSeed.getSelection());
        
        historyFileName.setEnabled(s == HISTORY);
        historyButton.setEnabled(s == HISTORY);
        
        runAutomaticallyCheckbox.setEnabled(s != MANUAL);
    }
    
    @Override
    public String getId() {
        return "org.abs-models.abs.plugin.javaTab2";
    }
    
    @Override
    public Image getImage() {
        return Images.DEBUGGER_RESUME;
    }
    

}
