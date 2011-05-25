package eu.hatsproject.absplugin.navigator.actionProvider;

import org.eclipse.jface.action.IMenuManager;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.navigator.CommonActionProvider;
import org.eclipse.ui.navigator.ICommonActionExtensionSite;

/**
 * A provider the {@link RightClickAction} that provides a menu
 * item on the context menu of the package container for refresh.
 * @author pwong
 *
 */
public class RefreshDependenciesProvider extends CommonActionProvider {
	
	private RightClickAction refresh;
	private ICommonActionExtensionSite aSite;
	
	@Override
	public void init(ICommonActionExtensionSite aSite){
		super.init(aSite);
		this.aSite = aSite;
		Shell shell = aSite.getViewSite().getShell();
		refresh = new RightClickAction(shell,aSite.getStructuredViewer().getSelection());
		aSite.getStructuredViewer().addSelectionChangedListener(refresh);
	}
	
	@Override
	public void fillContextMenu(IMenuManager menu) {
		super.fillContextMenu(menu);
		menu.add(refresh);
	}
	
	@Override
	public void dispose(){
		aSite.getStructuredViewer().removeSelectionChangedListener(refresh);
	}
	
}
