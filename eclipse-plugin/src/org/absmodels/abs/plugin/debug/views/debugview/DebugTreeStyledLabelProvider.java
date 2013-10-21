/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.absmodels.abs.plugin.debug.views.debugview;


import static org.absmodels.abs.plugin.debug.DebugUtils.getDebugger;
import static org.absmodels.abs.plugin.debug.DebugUtils.getSchedulerRef;
import static org.absmodels.abs.plugin.util.Constants.STYLER_BLACK;
import static org.absmodels.abs.plugin.util.Constants.STYLER_GREY;
import static org.absmodels.abs.plugin.util.Images.*;

import java.util.List;

import org.absmodels.abs.plugin.debug.model.Objects;
import org.absmodels.abs.plugin.debug.model.Tasks;
import org.eclipse.jface.viewers.ILabelProvider;
import org.eclipse.jface.viewers.StyledCellLabelProvider;
import org.eclipse.jface.viewers.StyledString;
import org.eclipse.jface.viewers.ViewerCell;
import org.eclipse.swt.graphics.Image;

import abs.backend.java.debugging.COGInfo;
import abs.backend.java.debugging.DebugModel;
import abs.backend.java.debugging.TaskState;
import abs.backend.java.observing.COGView;
import abs.backend.java.observing.MethodView;
import abs.backend.java.observing.ObjectView;
import abs.backend.java.observing.TaskStackFrameView;
import abs.backend.java.observing.TaskView;

/**
 * StyledLabelProvider for the TreeViewer of the DebugView. This class provides colored labels and icons 
 * for the debug tree reflecting the state of tasks, COGs and other elements of the debugged program.
 * @author tfischer
 */
public class DebugTreeStyledLabelProvider extends StyledCellLabelProvider implements ILabelProvider{

	public DebugTreeStyledLabelProvider(){
		super(StyledCellLabelProvider.COLORS_ON_SELECTION);
	}
	
	@Override
	public Image getImage(Object element) {
		
		DebugModel model = getDebugger().getModel();
		if(element instanceof DebugModel){
			return DEBUGGER_PROGRAM;
		} else if(element instanceof COGView){
			if(((COGView)element).getScheduler().getSchedulableTasks().size() > 0){
				return DEBUGGER_COG;
			} else{
				return DEBUGGER_COG_INACTIVE;
			}
		} else if(element instanceof TaskView){
			if(model.getTaskInfo((TaskView)element).getState() == TaskState.READY){
				return DEBUGGER_TASK_READY;
			} else if(model.getTaskInfo((TaskView)element).getState() == TaskState.SUSPENDED){
				return DEBUGGER_TASK_SUSPENDED;
			} else if(model.getTaskInfo((TaskView)element).getState() == TaskState.RUNNING){
				return DEBUGGER_TASK_RUNNING;
			} else if(model.getTaskInfo((TaskView)element).getState() == TaskState.FINISHED){
				return DEBUGGER_TASK_FINISHED;
			} else if(model.getTaskInfo((TaskView)element).getState() == TaskState.DEADLOCKED){
				return DEBUGGER_TASK_DEADLOCKED;
			} else if(model.getTaskInfo((TaskView)element).getState() == TaskState.ASSERTION_FAILED){
				return DEBUGGER_TASK_ASSERTION_FAILED;
			} else if(model.getTaskInfo((TaskView)element).getState() == TaskState.EXCEPTION){
				return DEBUGGER_TASK_EXCEPTION;
			} else if(model.getTaskInfo((TaskView)element).getState() == TaskState.BLOCKED){
				return DEBUGGER_TASK_BLOCKED;
			} else{
				return null;
			}
		} else if(element instanceof Tasks){
			if(((Tasks)element).getCOG().getScheduler().getSchedulableTasks().size() > 0){
				return DEBUGGER_TASKS;
			} else{
				return DEBUGGER_TASKS_INACTIVE;
			}
		} else if(element instanceof TaskStackFrameView){
			return DEBUGGER_STACK_FRAME;
		} else if(element instanceof Objects){
			return DEBUGGER_OBJECTS;
		} else if(element instanceof ObjectView){
			return DEBUGGER_OBJECT;
		} else{
			return null;
		}
	}

	@Override
	public String getText(Object element) {
		return getLabel(element).toString();		
	}

	@Override
	public void update(ViewerCell cell) {
		
		Object obj = cell.getElement();

		StyledString styledString = getLabel(obj);

		cell.setText(styledString.toString());
		cell.setStyleRanges(styledString.getStyleRanges());
		cell.setImage(getImage(obj));
		super.update(cell);
	}
	
	private COGInfo getCOGInfo(COGView cog){
		return getDebugger().getModel().getCOGInfo(cog);
	}
	
	private ObjectView getInitialObject(COGView cog){
		return getCOGInfo(cog).getInitialObject();
	}
	
	private StyledString getLabel(Object element){
		if (element instanceof TaskView){
			String s = "Task " + ((TaskView)element).getID() + " ("+((TaskView)element).getMethodName()+")";
			List<TaskView> schedulableTasks = getSchedulerRef().getSchedulableTasks();
			if(schedulableTasks!=null){
				for(TaskView taskView : schedulableTasks){
					if(taskView.getID() == ((TaskView)element).getID()){
						return new StyledString(s, STYLER_BLACK);
					}
				}
			}
			return new StyledString(s, STYLER_GREY);
		} else if (element instanceof COGView){
			COGView cog = (COGView) element;
			String s = "COG " + cog.getID() + " (" + getInitialObject(cog).getClassName() + " " + getInitialObject(cog).getID()+ ")"; 
			//for color of the string, check if there is a task that can be stepped
			if(cog.getScheduler().getSchedulableTasks().size() > 0){
				return new StyledString(s, STYLER_BLACK);
			} else{
				return new StyledString(s, STYLER_GREY);
			}
		} else if (element instanceof Tasks){
			String s = "Tasks (" + getCOGInfo(((Tasks)element).getCOG()).getTasks().size() + ")";
			//for color of the string, check if there is a task that can be stepped
			if(((Tasks)element).getCOG().getScheduler().getSchedulableTasks().size() > 0){
				return new StyledString(s, STYLER_BLACK);
			} else{
				return new StyledString(s, STYLER_GREY);
			}
		} else if (element instanceof Objects){
			return new StyledString("Objects (" + ((Objects)element).getObjects().size() + ")", STYLER_BLACK);
		} else if(element instanceof TaskStackFrameView){
			TaskStackFrameView tsv = ((TaskStackFrameView)element);
			MethodView mv = tsv.getMethod();
			return new StyledString("StackFrame (" + mv.toString() + ")", STYLER_BLACK);
		} else if(element instanceof ObjectView){
			ObjectView obj = (ObjectView)element;
			return new StyledString(obj.getClassName() + " " + obj.getID(), STYLER_BLACK);
		} else if(element instanceof DebugModel){
			return new StyledString(getDebugger().getProjectName(), STYLER_BLACK);
		} else {
			return new StyledString("UnknownObject", STYLER_GREY);
		}
	}
}
