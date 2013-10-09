package costabs.preferences;

import org.eclipse.jface.preference.PreferenceDialog;
import org.eclipse.jface.preference.PreferenceManager;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Shell;

public class CostabsPreferenceDialog extends PreferenceDialog{

	public CostabsPreferenceDialog(Shell parentShell, PreferenceManager manager) {
		super(parentShell, manager);
	}
	
}
