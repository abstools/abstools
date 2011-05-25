package eu.hatsproject.absplugin.navigator.actionProvider;

import static eu.hatsproject.absplugin.navigator.NavigatorUtils.updateDependencies;

import org.eclipse.jface.action.Action;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.TreeSelection;
import org.eclipse.jface.window.IShellProvider;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.actions.BuildAction;
import org.eclipse.ui.actions.RefreshAction;

/**
 * An action for refresh the package dependencies 
 * @author pwong
 *
 */
public class RightClickAction extends Action implements ISelectionChangedListener {
	
	private ISelection selection;
	private final Shell shell;
	
	private final IShellProvider provider = new IShellProvider() {
		public Shell getShell() {
			return shell;
		}
	};
	
	public RightClickAction(Shell shell, ISelection iSelection) {
		super("Refresh");
		this.shell = shell;
		this.selection = iSelection;
	}
	
	public void selectionChanged(SelectionChangedEvent event) {
		selection = event.getSelection();
	}
	
	@Override
	public void run(){
		if (selection != null && selection instanceof TreeSelection){
			updateDependencies((TreeSelection)selection);
			RefreshAction action = new RefreshAction(provider);
			action.refreshAll();
		}
	}

}
