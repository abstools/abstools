package eu.hatsproject.absplugin.debug.model;

import abs.backend.java.observing.COGView;

/**
 * This class is necessary to group tasks in the tree viewer of the debug view.
 * @see eu.hatsproject.absplugin.debug.views.debugview.DebugTreeContentProvider
 * @author tfischer
 */
public class Tasks {
	private COGView cog;
	
	public Tasks(COGView cog){
		this.cog = cog;
	}
	
	public COGView getCOG(){
		return cog;
	}
}
