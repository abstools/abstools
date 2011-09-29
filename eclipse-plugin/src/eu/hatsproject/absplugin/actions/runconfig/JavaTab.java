/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package eu.hatsproject.absplugin.actions.runconfig;

import static eu.hatsproject.absplugin.util.Constants.*;
import static eu.hatsproject.absplugin.util.UtilityFunctions.showErrorMessage;
import static eu.hatsproject.absplugin.util.UtilityFunctions.standardExceptionHandling;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.debug.core.ILaunchConfiguration;
import org.eclipse.debug.core.ILaunchConfigurationWorkingCopy;
import org.eclipse.jface.dialogs.InputDialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.DirectoryDialog;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.FileDialog;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;

import eu.hatsproject.absplugin.actions.runconfig.RunConfigEnums.DebuggerObserver;
import eu.hatsproject.absplugin.actions.runconfig.RunConfigEnums.DebuggerScheduler;

/**
 * Representing a LaunchConfigurationTab to run / debug generated Java files
 */
public class JavaTab extends AbstractTab {
	private Combo schedulerDropDown;
	
	private Button compileCheckButton;
	private Button externalProcessCheckButton;
	private Button debugModeCheckButton;
	private Button testExecutionButton;

	private Text otherArgsText;
	private Text seedNumber;
	private Text historyText;
	private org.eclipse.swt.widgets.List observerList;
	private org.eclipse.swt.widgets.List classPathList;

    private Long seed;

	@Override
	public void createControl(Composite parent) {
		TabListener myListener = new TabListener(this);
		Composite comp = new Composite(parent, SWT.NONE);

	    setCompositeLayout(comp);
	    createProjectDropDownMenu(myListener, comp);
	    createProductDropDownMenu(myListener, comp);
	    createObserverList(myListener, comp);
	    createClassPathList(myListener, comp);
	    Group schedulerGroup = createSchedulerDropDownMenu(myListener, comp);
	    createSchedulerOptions(myListener, schedulerGroup);
	    createOtherOptions(myListener, comp);
	    createAdditionalArgs(myListener, comp);
	    
		setControl(comp);
	}


	@Override
	public void setDefaults(ILaunchConfigurationWorkingCopy configuration) { 
		configuration.setAttribute(RUNCONFIG_DEBUGGER_RANDOMSEED, "-Dabs.randomseed=0");
		configuration.setAttribute(RUNCONFIG_PROJECT_NAME_ATTRIBUTE, getDefaultProjectName());
		configuration.setAttribute(RUNCONFIG_PRODUCT_NAME_ATTRIBUTE, (String)null);
		ArrayList<String> tempList = new ArrayList<String>();
		for (DebuggerObserver observer : DebuggerObserver.values()) {
			if(observer.getDefaultSelection()){
				tempList.add(observer.getClassName());
			}
		}
		configuration.setAttribute(RUNCONFIG_TEST_EXECUTION, false);
		configuration.setAttribute(RUNCONFIG_DEBUGGER_OBSERVER_LIST, tempList);
		
		configuration.setAttribute(RUNCONFIG_DEBUGGER_COMPILE_BEFORE, DEBUGGER_COMPILE_BEFORE_DEFAULT);
		configuration.setAttribute(RUNCONFIG_DEBUGGER_OTHER_ARGS_ATTRIBUTE, DEBUGGER_ARGS_OTHER_DEFAULT);
		configuration.setAttribute(RUNCONFIG_DEBUGGER_USE_EXTERNAL, RUNCONFIG_DEBUGGER_USE_EXTERNAL_DEFAULT);
		configuration.setAttribute(RUNCONFIG_DEBUGGER_DEBUG_MODE, RUNCONFIG_DEBUGGER_DEBUG_MODE_DEFAULT);
		if(RUNCONFIG_DEBUGGER_DEBUG_MODE_DEFAULT){
			configuration.setAttribute(RUNCONFIG_DEBUGGER_SCHEDULER_ATTRIBUTE, DebuggerScheduler.ECLIPSE.toString());
		} else {
			configuration.setAttribute(RUNCONFIG_DEBUGGER_SCHEDULER_ATTRIBUTE, DebuggerScheduler.getDefaultScheduler().toString());
		}
	}


	@Override
	public void initializeFrom(ILaunchConfiguration configuration) {
		try {
			initProject(configuration);
			initObserverList(configuration);
			initClassPathList(configuration);
			initCheckButtons(configuration);
			initSchedulerDropDown(configuration);
			initSchedulerOptions(configuration);
			initOtherArgsField(configuration);
			scheduleUpdateJob();
			
		} catch (CoreException e) {
			standardExceptionHandling(e);
			showErrorMessage("Fatal error!");
		}
		
	}

	@Override
	public void performApply(ILaunchConfigurationWorkingCopy configuration) {
	    final String sched = getSelectedScheduler();
	    if(sched!=null && DebuggerScheduler.valueOf(sched).equals(DebuggerScheduler.RANDOM) && validateSeed()){
	        configuration.setAttribute(RUNCONFIG_DEBUGGER_RANDOMSEED, getSeedCommand(getSeed()));
	    }

	    ArrayList<String> observerClassNames = new ArrayList<String>();
	    for (String string : observerList.getItems()) {
	        observerClassNames.add(getObserverClassName(string));
	    }
	    ArrayList<String> classPathNames = new ArrayList<String>();
	    for (String string : classPathList.getItems()) {
	        classPathNames.add(string);
	    }
	    configuration.setAttribute(RUNCONFIG_TEST_EXECUTION, testExecutionButton.getSelection());
	    configuration.setAttribute(RUNCONFIG_DEBUGGER_OBSERVER_LIST, observerClassNames);
	    configuration.setAttribute(RUNCONFIG_DEBUGGER_CLASSPATH_LIST, classPathNames);
	    configuration.setAttribute(RUNCONFIG_PROJECT_NAME_ATTRIBUTE, getSelectedProjectName());
	    configuration.setAttribute(RUNCONFIG_PRODUCT_NAME_ATTRIBUTE, getSelectedProductName());
	    configuration.setAttribute(RUNCONFIG_DEBUGGER_SCHEDULER_ATTRIBUTE, sched);
	    configuration.setAttribute(RUNCONFIG_DEBUGGER_COMPILE_BEFORE, compileCheckButton.getSelection());
	    configuration.setAttribute(RUNCONFIG_DEBUGGER_OTHER_ARGS_ATTRIBUTE, otherArgsText.getText());
	    configuration.setAttribute(RUNCONFIG_DEBUGGER_DEBUG_MODE, debugModeCheckButton.getSelection());
	    configuration.setAttribute(RUNCONFIG_DEBUGGER_USE_EXTERNAL, externalProcessCheckButton.getSelection());
	}

	private long getSeed() {
            String seedString = seedNumber.getText();
            long seed = Long.valueOf(seedString);
            if (seed <= 0) throw new NumberFormatException();
            return seed;
	}

	private boolean validateSeed() {
	    final String sched = getSelectedScheduler();
	    try{
	        if(sched!=null && DebuggerScheduler.valueOf(sched).equals(DebuggerScheduler.RANDOM)){
	            seed = getSeed();
	            if (seed > 0){
	                return true;
	            }
	        }
	    } catch (NumberFormatException e){
	        setErrorMessage("Seed is invalid - must be a positive number / long value");
	    }
	    return false;
	}

	@Override
	public String getName() {
		return "ABS Java Backend";
	}

	@Override
	public boolean isValid(ILaunchConfiguration launchConfig) {
		/* Don't try to start, you'll only get an exception immediately. */
	        boolean res = super.isValid(launchConfig);
	        res &= validateSeed();
	        if (res) {
	            setErrorMessage(null);
	        }
		return res;
	}

	//---create methods------------------------
	
	private void createCheckButtons(TabListener myListener,
			Composite comp) {
		
		testExecutionButton = new Button(comp, SWT.CHECK);
		testExecutionButton.setText("Execute ABSUnit tests");
		testExecutionButton.addListener(SWT.Selection, myListener);
		
		debugModeCheckButton = new Button(comp, SWT.CHECK);
		debugModeCheckButton.setText("Enable debug mode");
		debugModeCheckButton.addListener(SWT.Selection, myListener);
		
	    compileCheckButton = new Button(comp, SWT.CHECK);
	    compileCheckButton.setText("Generate Java files first");
	    compileCheckButton.setToolTipText("compile");
	    compileCheckButton.addListener(SWT.Selection, myListener);
		
		externalProcessCheckButton = new Button(comp, SWT.CHECK);
		externalProcessCheckButton.setText("Execute debugger in external process");
		externalProcessCheckButton.setToolTipText("do not switch into debug perspective");
		externalProcessCheckButton.addListener(SWT.Selection, myListener);
		externalProcessCheckButton.addSelectionListener(new SelectionListener() {
			
			@Override
			public void widgetSelected(SelectionEvent e) {
				setOtherArgsEnablement();
				addOrRemoveEclipseScheduler(); 
				
			}
			
			@Override
			public void widgetDefaultSelected(SelectionEvent e) {
				widgetSelected(e);
				
			}
			
			private void addOrRemoveEclipseScheduler() {
				if(schedulerDropDown!=null){
					if(externalProcessCheckButton.getSelection()){
						removeSchedulerFromDropDown(DebuggerScheduler.ECLIPSE);
					} else {
						schedulerDropDown.add(DebuggerScheduler.ECLIPSE.getUserReadableName());
					}
				}
			}
		});
	}
	
	private void createObserverList(TabListener myListener, Composite comp) {
		Group group = createGroup(comp, "Observer", 2, 1, GridData.FILL_HORIZONTAL);
		observerList = new org.eclipse.swt.widgets.List(group, SWT.SINGLE | SWT.BORDER | SWT.V_SCROLL);
		GridData gridData1 = new GridData();
		gridData1.widthHint =  400;
		gridData1.heightHint = 150;
		observerList.setLayoutData(gridData1);
		
		Composite subComp = new Composite(group, SWT.NONE);
		GridLayout gridLayout = new GridLayout(1, false);
	    gridLayout.verticalSpacing = 8;
	    subComp.setLayout(gridLayout);
	    
	    GridData gridData2 = new GridData();
		gridData2.widthHint = 150;
		
		Button addButton = new Button(subComp, SWT.PUSH);
		addButton.setText("Add new observer");
		addButton.addSelectionListener(new SelectionListener() {
			
			@Override
			public void widgetSelected(SelectionEvent e) {
				widgetDefaultSelected(e);
				
			}
			
			@Override
			public void widgetDefaultSelected(SelectionEvent e) {
				InputDialog dialog = new InputDialog(Display.getDefault().getActiveShell(), "Add new System Observer", "Please type in the class name", "example.systemObserver", null);
				dialog.open();
				String input = dialog.getValue();
				if(input != null) {
					observerList.add(input +" - New observer");
					updateLaunchConfigurationDialog();
				}
				
			}
		});
		addButton.setLayoutData(gridData2);
		
		
		for (final DebuggerObserver debuggerObserver : DebuggerObserver.values()) {
			Button addObserverButton = new Button(subComp, SWT.PUSH);
			addObserverButton.setText("Add "+debuggerObserver.getUserReadableName());
			addObserverButton.addSelectionListener(new SelectionListener() {
				
				@Override
				public void widgetSelected(SelectionEvent e) {
					widgetDefaultSelected(e);
					
				}
				
				@Override
				public void widgetDefaultSelected(SelectionEvent e) {
					observerList.add(debuggerObserver.getClassName()+" - "+debuggerObserver.getUserReadableName());
					updateLaunchConfigurationDialog();
				}
			});
			addObserverButton.setLayoutData(gridData2);
		}
		
		Button removeButton = new Button(subComp, SWT.PUSH);
		removeButton.setText("Remove selected observer");
		removeButton.addSelectionListener(new SelectionListener() {
			
			@Override
			public void widgetSelected(SelectionEvent e) {
				widgetDefaultSelected(e);
			}
			
			@Override
			public void widgetDefaultSelected(SelectionEvent e) {
				int[] indices = observerList.getSelectionIndices();
				if(indices != null){
					observerList.remove(indices);
					updateLaunchConfigurationDialog();
				}
			}
		});
		removeButton.setLayoutData(gridData2);	
	}
	
	private void createClassPathList(TabListener myListener, Composite comp) {
        Group group = createGroup(comp, "Classpath", 2, 1, GridData.FILL_HORIZONTAL);
        classPathList = new org.eclipse.swt.widgets.List(group, SWT.SINGLE | SWT.BORDER | SWT.V_SCROLL);
        GridData gridData1 = new GridData();
        gridData1.widthHint =  400;
        gridData1.heightHint = 150;
        classPathList.setLayoutData(gridData1);
        
        Composite subComp = new Composite(group, SWT.NONE);
        GridLayout gridLayout = new GridLayout(1, false);
        gridLayout.verticalSpacing = 8;
        subComp.setLayout(gridLayout);
        
        GridData gridData2 = new GridData();
        gridData2.widthHint = 150;
        
        Button addButton = new Button(subComp, SWT.PUSH);
        addButton.setText("Add new classpath entry");
        addButton.addSelectionListener(new SelectionListener() {
            
            @Override
            public void widgetSelected(SelectionEvent e) {
                widgetDefaultSelected(e);
                
            }
            
            @Override
            public void widgetDefaultSelected(SelectionEvent e) {
                DirectoryDialog dialog = new DirectoryDialog(Display.getDefault().getActiveShell(), SWT.APPLICATION_MODAL);
                String input = dialog.open();
                if(input != null) {
                    classPathList.add(input);
                    updateLaunchConfigurationDialog();
                }
                
            }
        });
        addButton.setLayoutData(gridData2);
        
        Button removeButton = new Button(subComp, SWT.PUSH);
        removeButton.setText("Remove selected classpath entry");
        removeButton.addSelectionListener(new SelectionListener() {
            
            @Override
            public void widgetSelected(SelectionEvent e) {
                widgetDefaultSelected(e);
            }
            
            @Override
            public void widgetDefaultSelected(SelectionEvent e) {
                int[] indices = classPathList.getSelectionIndices();
                if(indices != null){
                    classPathList.remove(indices);
                    updateLaunchConfigurationDialog();
                }
            }
        });
        removeButton.setLayoutData(gridData2);  
    }
	
	private Group createSchedulerDropDownMenu(TabListener myListener,
			Composite comp) {
		Group group = createGroup(comp, "Scheduler", 1, 1, GridData.FILL_HORIZONTAL);
	    schedulerDropDown = new Combo(group, SWT.DROP_DOWN | SWT.READ_ONLY | SWT.BORDER);
		GridData gridData = new GridData();
		gridData.widthHint = 200;
		schedulerDropDown.setLayoutData(gridData);
	    schedulerDropDown.addListener(SWT.Selection, myListener);
	    schedulerDropDown.addSelectionListener(new SelectionListener() {
			
			@Override
			public void widgetSelected(SelectionEvent e) {
				widgetDefaultSelected(e);
			}

			@Override
			public void widgetDefaultSelected(SelectionEvent e) {
				checkRandomSeed();
				checkSystemObserver();
			}
			
			/**
			 * enables / disables random seed field
			 */
			private void checkRandomSeed() {
				if(isSchedulerSelected(DebuggerScheduler.RANDOM)){
					seedNumber.setEnabled(true);
				} else {
					seedNumber.setEnabled(false);
				}
			}
			
			/**
			 * Eclipse scheduler without eclipse observer is not useful.
			 */
			private void checkSystemObserver(){
				if(isSchedulerSelected(DebuggerScheduler.ECLIPSE)){
					String eclipseObserver = DebuggerObserver.ECLIPSE.getClassName();
					for (String observerListName : observerList.getItems()) {
						if(getObserverClassName(observerListName).equals(eclipseObserver)){
							return;
						}
					}
					observerList.add(eclipseObserver + " - "+ DebuggerObserver.ECLIPSE.getUserReadableName());
				}
			}
			
			private boolean isSchedulerSelected(DebuggerScheduler scheduler){
				int index = schedulerDropDown.getSelectionIndex();
				String item = schedulerDropDown.getItem(index);
				if(item.equals(scheduler.getUserReadableName())){
					return true;
				} else {
					return false;
				}
			}
			
		});
	    return group;
	}
	
	private void createSchedulerOptions(TabListener myListener, Group group) {
		Composite comp = new Composite(group, SWT.NONE);
		comp.setLayout(new GridLayout(3, false));
		GridData gridData = new GridData();
		gridData.widthHint = 100;
		gridData.verticalAlignment = SWT.TOP;
	    
		Label seedLabel = new Label(comp, SWT.NULL);
		seedLabel.setText("Random seed: ");
		seedLabel.setLayoutData(gridData);
		seedNumber = new Text(comp, SWT.SINGLE | SWT.BORDER);
		gridData = new GridData();
		gridData.widthHint = 200;
		gridData.heightHint = 20;
		gridData.verticalAlignment = SWT.TOP;
		seedNumber.setLayoutData(gridData);
		seedNumber.addListener(SWT.Modify, myListener);
		seedNumber.setEnabled(false);
		Label c = new Label(comp, SWT.NULL);
		c.setLayoutData(gridData);
		
		Label historyLabel = new Label(comp, SWT.NULL);
		historyLabel.setText("History: ");
		gridData = new GridData();
		gridData.widthHint = 100;
		gridData.verticalAlignment = SWT.TOP;
		historyLabel.setLayoutData(gridData);

		historyText = new Text(comp, SWT.SINGLE | SWT.BORDER);
		gridData = new GridData();
		gridData.widthHint = 200;
		historyText.setLayoutData(gridData);
		historyText.addListener(SWT.Modify, myListener);
		historyText.setEnabled(false);
		
		Button historyButton = new Button(comp, SWT.PUSH);
		historyButton.setText("Browse");
		gridData = new GridData();
		gridData.widthHint = 60;
		gridData.verticalAlignment = SWT.FILL;
		historyButton.setLayoutData(gridData);
		historyButton.addSelectionListener(new SelectionAdapter() {
	        @Override
			public void widgetSelected(SelectionEvent event) {
	          FileDialog historyDialog = new FileDialog(getShell(), SWT.OPEN);
	          String historyFile = historyDialog.open();
	          if (historyFile != null) {
	            historyText.setText(historyFile);
	          }
	        }
	      });
		historyButton.setEnabled(false);
	}
	
	private void createOtherOptions(TabListener myListener, Composite comp) {
		Group group = createGroup(comp, "Options", 1, 1,  GridData.FILL_HORIZONTAL);
		createCheckButtons(myListener, group);
	}
	
	private void createAdditionalArgs(TabListener myListener, Composite comp) {
		Group group = createGroup(comp, "Additional Arguments", 1, 1,  GridData.FILL_HORIZONTAL);
		createOtherArgsField(myListener, group);
	}
	
	private void createOtherArgsField(TabListener myListener, Composite comp) {
		GridData gridData = new GridData(GridData.HORIZONTAL_ALIGN_FILL);
	    gridData.grabExcessHorizontalSpace = true;
	    
	    otherArgsText = new Text(comp,
	    		SWT.WRAP 
	    		| SWT.MULTI
		        | SWT.BORDER
		        | SWT.H_SCROLL
		        | SWT.V_SCROLL);
		gridData = new GridData(GridData.HORIZONTAL_ALIGN_FILL
				| GridData.VERTICAL_ALIGN_FILL);
		gridData.minimumHeight = 100;
	    gridData.grabExcessHorizontalSpace = true;
	    gridData.grabExcessVerticalSpace = true;

	    otherArgsText.setLayoutData(gridData);
	    otherArgsText.addListener(SWT.Modify, myListener);
	}
	
	
	//---init methods----------------------------------
	
	private void initCheckButtons(ILaunchConfiguration configuration) throws CoreException {
		compileCheckButton.setSelection(configuration.getAttribute(RUNCONFIG_DEBUGGER_COMPILE_BEFORE, DEBUGGER_COMPILE_BEFORE_DEFAULT));
		externalProcessCheckButton.setSelection(configuration.getAttribute(RUNCONFIG_DEBUGGER_USE_EXTERNAL, RUNCONFIG_DEBUGGER_USE_EXTERNAL_DEFAULT));
		debugModeCheckButton.setSelection(configuration.getAttribute(RUNCONFIG_DEBUGGER_DEBUG_MODE, RUNCONFIG_DEBUGGER_DEBUG_MODE_DEFAULT));
	}
	
	private void initObserverList(ILaunchConfiguration configuration) throws CoreException {
		observerList.removeAll();
		@SuppressWarnings("unchecked") //must be String, see org.eclipse.debug.core.ILaunchConfigurationWorkingCopy.setAttribute(String attributeName, List value)
		List<String> observers = configuration.getAttribute(RUNCONFIG_DEBUGGER_OBSERVER_LIST, new ArrayList<String>());
		if(observers != null){
			for (String observerClassName : observers) {
				boolean wasFound = false;
				for(DebuggerObserver debuggerObserver : DebuggerObserver.values()){
					if(observerClassName.equals(debuggerObserver.getClassName())){
						observerList.add(observerClassName + " - "+debuggerObserver.getUserReadableName());
						wasFound = true;
						break;
					}	
				}
				if(!wasFound){
					observerList.add(observerClassName + " - New observer");
				}
			}
		}

	}

	private void initClassPathList(ILaunchConfiguration configuration) throws CoreException {
        classPathList.removeAll();
        @SuppressWarnings("unchecked") //must be String, see org.eclipse.debug.core.ILaunchConfigurationWorkingCopy.setAttribute(String attributeName, List value)
        List<String> cpentries = configuration.getAttribute(RUNCONFIG_DEBUGGER_CLASSPATH_LIST, new ArrayList<String>());
        if(cpentries != null){
            for (String cpentry : cpentries) {
                classPathList.add(cpentry);
            }
        }

    }

	private void initSchedulerDropDown(ILaunchConfiguration configuration) throws CoreException {
		String selectedSchedulerString = configuration.getAttribute(RUNCONFIG_DEBUGGER_SCHEDULER_ATTRIBUTE, DebuggerScheduler.getDefaultScheduler().toString());
		DebuggerScheduler selectedScheduler = DebuggerScheduler.valueOf(selectedSchedulerString);
		DebuggerScheduler[] scheduler = DebuggerScheduler.values();
		schedulerDropDown.removeAll();
		for (int i = 0; i < scheduler.length; i++) {
			schedulerDropDown.add(scheduler[i].getUserReadableName());
			if(selectedScheduler.equals(scheduler[i])){
				schedulerDropDown.select(i);
				if(selectedScheduler.equals(DebuggerScheduler.RANDOM)){
					seedNumber.setEnabled(true);
				} else {
					seedNumber.setEnabled(false);
				}
			}
		}
		if(externalProcessCheckButton.getSelection()){
			removeSchedulerFromDropDown(DebuggerScheduler.ECLIPSE);
		}
	}
	
	private void removeSchedulerFromDropDown(DebuggerScheduler scheduler){
		boolean wasSelected = schedulerDropDown.getItem(schedulerDropDown.getSelectionIndex()).equals(scheduler.getUserReadableName());
		schedulerDropDown.remove(scheduler.getUserReadableName());
		if(wasSelected){
			schedulerDropDown.select(0);
		}
	}

	private void initSchedulerOptions(ILaunchConfiguration configuration) throws CoreException {
		seedNumber.setText("");
		String seedString = configuration.getAttribute(RUNCONFIG_DEBUGGER_RANDOMSEED, "");
		if(seedString == null || seedString.equals("")){
			seedNumber.setText("0");
		} else{
			long seed = getSeedNumber(seedString);
			seedNumber.setText(String.valueOf(seed));
		}
	}
	
	private void initOtherArgsField(ILaunchConfiguration configuration) throws CoreException {
		otherArgsText.setText(configuration.getAttribute(RUNCONFIG_DEBUGGER_OTHER_ARGS_ATTRIBUTE, DEBUGGER_ARGS_OTHER_DEFAULT));
		setOtherArgsEnablement();
	}
	
	private void setOtherArgsEnablement(){
		if(externalProcessCheckButton.getSelection()){
			otherArgsText.setEnabled(true);
		} else {
			otherArgsText.setEnabled(false);
		}
	}

	private void setCompositeLayout(Composite comp) {
		GridLayout gridLayout = new GridLayout(1, false);
	    gridLayout.verticalSpacing = 8;
	    comp.setLayout(gridLayout);
	}
	
	
	//---get and help functions----------------------------------
	
	/**
	 * from "package.name - user readable name" to "package.name"
	 * 
	 * @param observerString
	 * @return
	 */
	private String getObserverClassName(String observerString){
		int endIndex = observerString.indexOf("-");
		String tempClassNames = observerString;
		if(endIndex > 0){
			tempClassNames = observerString.substring(0,endIndex);
		}
		return tempClassNames.trim();
	}
	
	private String getSelectedScheduler() {
		int index = schedulerDropDown.getSelectionIndex();
		if(index>=0) {
			String userReadableString = schedulerDropDown.getItem(index);
			return DebuggerScheduler.valueOfUserReadableName(userReadableString).toString();
		} else {
			return DebuggerScheduler.getDefaultScheduler().toString();
		}
		
	}
	
	private String getSeedCommand(long seed) {
		return "-Dabs.randomseed="+seed;
	}
	
	private long getSeedNumber(String s) {
		return Long.valueOf(s.replaceFirst("-Dabs.randomseed=",""));
	}
	
}
