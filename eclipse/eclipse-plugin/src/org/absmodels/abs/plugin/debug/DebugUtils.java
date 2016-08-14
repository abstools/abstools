/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.absmodels.abs.plugin.debug;

import static org.absmodels.abs.plugin.debug.DebugUtils.getSchedulerRef;
import static org.absmodels.abs.plugin.util.Constants.ABS_DEBUG_VARIABLE_VIEW;
import static org.absmodels.abs.plugin.util.Constants.ABS_DEBUG_VIEW;
import static org.absmodels.abs.plugin.util.Constants.DEFAULT_SCHEDULER;

import java.util.List;

import org.absmodels.abs.plugin.debug.model.Debugger;
import org.absmodels.abs.plugin.debug.scheduling.SchedulingStrategy;
import org.absmodels.abs.plugin.debug.views.debugview.DebugView;
import org.absmodels.abs.plugin.debug.views.variablesview.VariableView;
import org.absmodels.abs.plugin.editor.ABSEditor;
import org.absmodels.abs.plugin.util.Constants;
import org.absmodels.abs.plugin.util.UtilityFunctions;
import org.absmodels.abs.plugin.util.Constants.Scheduler;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.swt.widgets.Display;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IEditorReference;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.PlatformUI;

import abs.backend.java.debugging.TaskInfo;
import abs.backend.java.observing.ObjectView;
import abs.backend.java.observing.TaskStackFrameView;
import abs.backend.java.observing.TaskView;


/**
 * Convenience methods used by classes in the debug package.
 * @author fstrauss, mweber, tfischer
 *
 */
public class DebugUtils {
	private static Scheduler scheduler = DEFAULT_SCHEDULER;
	public static IAction resume;
	public static IAction terminate;
	public static IAction executeSingleStep;
	public static IAction executeNSteps;
	public static IAction stepOver;
	public static IAction runToLine;
	public static IAction saveHistory; 
	public static IAction selectScheduler;
	public static IAction schedulerMenu;
	
	public static SchedulingStrategy schedulerRef;
	
	private static DebugView debugView;
	public static volatile boolean highlightStep = true;
	private static volatile TaskInfo lastHighlightInfo = null;
    private static boolean runAutomatically;
    private static String historyFile;
    private static  IProject currentProject;
	
	public static void enableHightlighting(){
		highlightStep = true;
		if(lastHighlightInfo!=null){
			highlightLine(lastHighlightInfo);
		}
		getSchedulerRef().highlightNextStep();
	}
	
	public static void disableHighlighting(){
		highlightStep = false;
		removeAllHighlighting();
	}
	
	/**
	 * Sets the enablement of resume, singleStep, NSteps, stepOver and runToLine buttons.
	 * @param enabled if true the five buttons are enabled, otherwise disabled
	 */
	private static void setStepButtonEnablement(boolean enabled){
		executeSingleStep.setEnabled(enabled);
		executeNSteps.setEnabled(enabled);
		stepOver.setEnabled(enabled);
		runToLine.setEnabled(enabled);
		resume.setEnabled(enabled);
	}

	/**
	 * Disables ExecuteNSteps, resume and terminate buttons. Enables saveHistory button. Checks, if an action exists for
	 * the currently in the debug tree selected element and sets enablement of singleStep button accordingly.
	 */
	public static void refreshButtonEnablement(){
		if (terminate == null) {
			return;
		}
		terminate.setEnabled(isDebuggerRunning());
		schedulerMenu.setEnabled(isDebuggerRunning());
		saveHistory.setEnabled(true);
		
		//Set enablement for step buttons explicitly
		setStepButtonEnablement(false);
		List<TaskView> schedulableTasks;
		
		switch(scheduler){
		case interactive:
			Object selectedObject = getDebugViewerSelection();
			schedulableTasks = getSchedulerRef().getSchedulableTasks();
			if(selectedObject instanceof TaskView){
				if(schedulableTasks != null){
					for(TaskView taskView : schedulableTasks){
						if (taskView == selectedObject){
							setStepButtonEnablement(true);
						}
					}
				}
			} else if(selectedObject instanceof TaskStackFrameView){
				if(schedulableTasks != null){
					for(TaskView taskView : schedulableTasks){
						if (taskView == ((TaskStackFrameView)selectedObject).getStack().getTask()){
							setStepButtonEnablement(true);
						}
					}
				}
			} break;
		case random: 
		case replay:
			schedulableTasks = getSchedulerRef().getSchedulableTasks();
			if(schedulableTasks != null && ! schedulableTasks.isEmpty()){
				setStepButtonEnablement(true);
			} break;
		default: 
			//disable all buttons
			setStepButtonEnablement(false);
			terminate.setEnabled(false);
			saveHistory.setEnabled(false);
		}
	}

    private static boolean isDebuggerRunning() {
        return getDebugger() != null && getDebugger().isRunning();
    }

    
	
	public static void highlightLine(final IPath path, final int n){
		Display.getDefault().asyncExec(new Runnable() {
			
			@Override
			public void run() {
				ABSEditor editor = UtilityFunctions.openABSEditorForFile(path, currentProject);
				editor.highlightLine(n);
			}
		});
		
	}
	
	public static void highlightLine(TaskInfo info){
		lastHighlightInfo = info;
		if(highlightStep){
			int line = info.getCurrentLine() - 1;
			String curFilepath = info.getCurrentFile();
			if(curFilepath!=null)
				highlightLine(new Path(curFilepath), line);
		}
	}
	
	public static void removeHighlighting(TaskInfo info){
		if(lastHighlightInfo==info){
			lastHighlightInfo = null;
		}
		if(highlightStep){
			String curFilePath = info.getCurrentFile();
			if(curFilePath!=null)
				removeHighlighting(new Path(curFilePath));
		}
	}
	
	public static void removeHighlighting(final IPath path){
		Display.getDefault().asyncExec(new Runnable() {
			
			@Override
			public void run() {
			    ABSEditor editor = UtilityFunctions.openABSEditorForFile(path, currentProject);
				editor.removeHighlighting();
			}
		});
	}
	
	public static void removeAllHighlighting(){
		Display.getDefault().asyncExec(new Runnable() {
			
			@Override
			public void run() {
				IEditorReference[] editorReferences = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getActivePage().getEditorReferences();
				for (IEditorReference iEditorReference : editorReferences) {
					IEditorPart editor = iEditorReference.getEditor(false);
					if(editor instanceof ABSEditor){
						ABSEditor abseditor = (ABSEditor) editor;
						abseditor.removeHighlighting();
					}
				}
			}
		});
	}
	
	private static DebugView getDebugView(){		
		IWorkbenchPage activePage = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getActivePage();
		debugView = (DebugView)activePage.findView(ABS_DEBUG_VIEW);
		if (debugView == null) {
			// open view if it does not exist yet
			try {
				debugView = (DebugView) activePage.showView(ABS_DEBUG_VIEW);
			} catch (PartInitException e) {
				e.printStackTrace();
				UtilityFunctions.showErrorMessage("Could not open debug view.");
			}
		}
		return debugView;		
	}
	
	/**
	 * Gets the Viewer of the Debug View.
	 * @return The viewer of the debug view, if a view exists, null otherwise.
	 */
	public static Viewer getDebugViewer(){
		DebugView debugView = getDebugView();
		if(debugView == null){
			return null;
		} else{
			return debugView.getViewer();
		}
	}
	
	//TODO: use this method instead of getting debugViewer and do getSelection there
	public static Object getDebugViewerSelection(){
		Viewer debugViewer = getDebugViewer();
		if(debugViewer == null){
			return null;
		} 
		IStructuredSelection selection = (IStructuredSelection)debugViewer.getSelection();
		if(selection == null){
			return null;
		} 
		return selection.getFirstElement();
	}
	
	/**
	 * Refreshes the display of the tree viewer of the debug view
	 */
	public static void refreshDebugViewer(){
		Display.getDefault().asyncExec(new Runnable() {
			@Override
			public void run(){
				getDebugViewer().refresh();
			}
		});
	}
	
	/**
	 * @return The debugger of the debug view if a debug view exists, null otherwise
	 */
	public static Debugger getDebugger(){
		if(getDebugView() == null){
			return null;
		} else{
			return getDebugView().getDebugger();
		}
	}
	
	public static SchedulingStrategy getSchedulerRef(){
		//TODO 
		if(schedulerRef != null){
			return schedulerRef;
		} else {
			//TODO show error message;
			System.err.println("SchedulingStrategy not found");
			return null;
		}
	}

	private static VariableView getVariableView(){
		return (VariableView)PlatformUI.
		getWorkbench()
		.getActiveWorkbenchWindow()
		.getActivePage()
		.findView(ABS_DEBUG_VARIABLE_VIEW);		
	}
	
	/**
	 * Loads the current selection of the debug view in the variable view, but does not open the view.
	 * If no ObjectView or TaskStackFrame is selected, the variable view will be empty.
	 */
	public static void refreshVariableView() {
		refreshVariableView(getDebugViewerSelection());
	}
	
	/**
	 * Loads a specific object in the variable view, but does not open the view.
	 * @param o The object to be shown in the variable view.
	 */
	public static void refreshVariableView(Object o) {
		VariableView view = getVariableView();
		if(view != null){
			if(o instanceof TaskStackFrameView || o instanceof ObjectView){
				getVariableView().getViewer().setInput(o);
			} else if(o instanceof TaskView){
				if(((TaskView)o).getStack().hasFrames()){
					getVariableView().getViewer().setInput(((TaskView)o).getStack().getCurrentFrame());
				} else {
					getVariableView().getViewer().setInput(null);
				}
			}
		}
	}
	
	/**
	 * Opens the variable view and load a given object to it.
	 * @param o Object to be shown in the newly opened view
	 */
	public static void openVariableView(Object o){
		openVariableView();
		refreshVariableView(o);
	}
	
	/**
	 * Opens the variable view without showing an object.
	 */
	public static void openVariableView(){
		try {
			PlatformUI
			.getWorkbench()
			.getActiveWorkbenchWindow()
			.getActivePage()
			.showView(Constants.ABS_DEBUG_VARIABLE_VIEW);
		} catch (PartInitException e) {
			UtilityFunctions.showErrorMessage("Fatal Error while opening variable view: " + e.getMessage());
		}
	}
	
	/**
	 * Sets the selection of the TreeViewer of the DebugView to the next schedulable task.
	 */
	public static void selectNextTask(){
		if(getSchedulerRef().getSchedulableTasks() != null){
			setDebugTreeSelection(getSchedulerRef().getSchedulableTasks().get(0));
		}
	}

	/**
	 * Sets the selection of the TreeViewer of the DebugView to a given object
	 * @param o The object to be selected by the TreeViewer
	 */
	public static void setDebugTreeSelection(final Object o){
		Display.getDefault().asyncExec(new Runnable() {
			@Override
			public void run() { getDebugViewer().setSelection(new StructuredSelection(o)); }
		});
	}

    public static Scheduler getScheduler() {
        return scheduler;
    }
    
    public static void setScheduler(Scheduler s) {
        scheduler = s;
        Display.getDefault().asyncExec(new Runnable() {
            
            @Override
            public void run() {
                if(getSchedulerRef() != null)
                    getSchedulerRef().updateScheduler();
                
            }
        });
        
    }

    public static void setCurrentProject(IProject project) {
        currentProject = project;
    }
    
    public static void setRunAutomatically(boolean ra) {
        runAutomatically = ra;
    }

    public static boolean getRunAutomatically() {
        return runAutomatically;
    }

    public static void setHistoryFile(String hf) {
        historyFile = hf;        
    }
    
    public static String getHistoryFile() {
        return historyFile;
    }
}
