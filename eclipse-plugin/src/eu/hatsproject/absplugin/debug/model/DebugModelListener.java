package eu.hatsproject.absplugin.debug.model;

import static eu.hatsproject.absplugin.debug.DebugUtils.highlightLine;
import static eu.hatsproject.absplugin.debug.DebugUtils.refreshDebugViewer;
import static eu.hatsproject.absplugin.debug.DebugUtils.removeHighlighting;
import eu.hatsproject.absplugin.debug.DebugUtils;
import abs.backend.java.debugging.COGInfo;
import abs.backend.java.debugging.TaskInfo;

/**
 * Listener reacting on creation and changes of tasks and COGs
 * @author tfischer
 */
public class DebugModelListener implements abs.backend.java.debugging.DebugModelListener {
	
	TaskInfo lastTask;

	@Override
	public void taskInfoRemoved(TaskInfo line) { 
	    if(DebugUtils.highlightStep)
	    	refreshDebugViewer();
	}
	
	@Override
	public void taskInfoChanged(TaskInfo line) {
	    if(DebugUtils.highlightStep){
			DebugUtils.getSchedulerRef().setLastTask(line);
			switch(line.getState()){
			case FINISHED:
			case EXCEPTION:
				removeHighlighting(line);
				break;
			default:
				highlightLine(line);
				break;
			}
			refreshDebugViewer(); 
	    }
	}
	
	@Override
	public void taskInfoAdded(TaskInfo line) { 
	    if(DebugUtils.highlightStep)
	    	refreshDebugViewer(); 
	}
	
	@Override
	public void cogCreated(COGInfo info) {
	    if(DebugUtils.highlightStep)
	    	refreshDebugViewer();
	}
	
	@Override
	public void cogChanged(COGInfo info) { 
	    if(DebugUtils.highlightStep)
	    	refreshDebugViewer();
	}
}