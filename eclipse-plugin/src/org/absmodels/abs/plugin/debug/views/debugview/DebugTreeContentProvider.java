/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.absmodels.abs.plugin.debug.views.debugview;

import static org.absmodels.abs.plugin.debug.DebugUtils.getDebugger;
import static org.absmodels.abs.plugin.util.Constants.EMPTY_OBJECT_ARRAY;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;

import org.absmodels.abs.plugin.debug.DebugUtils;
import org.absmodels.abs.plugin.debug.model.Objects;
import org.absmodels.abs.plugin.debug.model.Tasks;
import org.eclipse.jface.viewers.ITreeContentProvider;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.swt.widgets.Display;

import abs.backend.java.debugging.COGInfo;
import abs.backend.java.debugging.DebugModel;
import abs.backend.java.debugging.TaskInfo;
import abs.backend.java.observing.COGView;
import abs.backend.java.observing.TaskStackFrameView;
import abs.backend.java.observing.TaskStackView;
import abs.backend.java.observing.TaskView;

/**
 * The TreeContentProvider for the TreeViewer of the DebugView. This class provides
 * the structural information about the debug tree, i.e. what the children of specific 
 * elements are.
 * @author tfischer
 */
public class DebugTreeContentProvider implements ITreeContentProvider{

	private final class COGViewComparator implements Comparator<COGView> {
		@Override
		public int compare(COGView o1, COGView o2) {
			return ((Integer)o1.getID()).compareTo(o2.getID());
		}
	}

	@Override
	public Object[] getChildren(Object parentElement) {
		
		DebugModel model = getDebugger().getModel();
		
		if(model != null && parentElement != null){
			if(parentElement instanceof DebugModel){
				List<COGView> cogs = ((DebugModel)parentElement).getCOGs();
				Collections.sort(cogs, new COGViewComparator());
				return cogs.toArray();
			} else if(parentElement instanceof COGView){
				COGView cog = (COGView)parentElement;
				Tasks t = getDebugger().getTasks(cog); 
				Objects o = getDebugger().getObjects(cog);
				return new Object[] {t, o};
			} else if (parentElement instanceof Tasks){
				return model.getTasks(((Tasks)parentElement).getCOG()).toArray();
			} else if (parentElement instanceof Objects){
				return ((Objects)parentElement).getObjects().toArray();
			} else if(parentElement instanceof TaskView){
				TaskView t = (TaskView)parentElement;
				TaskStackView s = t.getStack();
				ArrayList<TaskStackFrameView> list = new ArrayList<TaskStackFrameView>(s.getFrames());
				Collections.reverse(list);
				return list.toArray();
			} 
		}
		return EMPTY_OBJECT_ARRAY;
	}

	@Override
	public Object[] getElements(Object elem) {
		if (elem instanceof Object[]) {
			return (Object[]) elem;
		} 
		return EMPTY_OBJECT_ARRAY;
	}

	
	@Override
	public Object getParent(Object element) {
	//for working auto expansion, getParent needs to be implemented here.
		if(element != null){
			if(element instanceof TaskStackFrameView){
				return ((TaskStackFrameView)element).getStack().getTask();
			} else if(element instanceof TaskView){
				return DebugUtils.getDebugger().getTasks(((TaskView)element).getCOG());
			} else if(element instanceof Tasks){
				return ((Tasks)element).getCOG();
			} else if(element instanceof COGView){
				return DebugUtils.getDebugger().getModel();
			} 	
		}
		return null;
	}

	@Override
	public boolean hasChildren(Object element) {
		
		DebugModel model = getDebugger().getModel();
		
		if(model != null && element != null){
			if(element instanceof DebugModel){
				return ((DebugModel)element).getCOGs().size() > 0;
			} else if(element instanceof Tasks){
				return model.getTasks(((Tasks)element).getCOG()).size() > 0;
			} else if(element instanceof Objects){
				return ((Objects)element).getObjects().size() > 0;
			} else if(element instanceof COGView){
				return true;
			} else if(element instanceof TaskView){
				TaskView task = (TaskView)element;
				if(task.getStack() == null){
					return false;
				} else if(task.getStack().getFrames() == null){
					return false;
				} else{
					return ((TaskView)element).getStack().getFrames().size() > 0;
				}
			}
		}
		return false;
	}

	@Override
	public void dispose() { }
	
	/**
	 * @exception SWTException might get thrown by asyncExec, if the reciever has been disposed
	 */
	@Override
	public void inputChanged(final Viewer viewer, Object arg1, final Object arg2) { 
		Display.getDefault().asyncExec(new Runnable() {
			@Override
			public void run() {
				if(arg2 instanceof Object[] && ((Object[])arg2).length == 1 
						&& ((Object[])arg2)[0] != null && ((Object[])arg2)[0] instanceof DebugModel){
					DebugModel debugModel = (DebugModel)((Object[])arg2)[0];
					List<COGInfo> cogs = debugModel.getCOGInfos();
					if(!cogs.isEmpty()){
						List<TaskInfo> tasks = cogs.get(0).getTasks();
						if(!tasks.isEmpty()){
							TaskView taskView = tasks.get(0).getTaskView();
							((TreeViewer)viewer).setSelection(new StructuredSelection(taskView));
						}
					}
				}
			}
		});
	}
}
