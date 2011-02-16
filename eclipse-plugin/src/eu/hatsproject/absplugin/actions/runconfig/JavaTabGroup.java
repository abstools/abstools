package eu.hatsproject.absplugin.actions.runconfig;

import org.eclipse.debug.ui.AbstractLaunchConfigurationTabGroup;
import org.eclipse.debug.ui.ILaunchConfigurationDialog;
import org.eclipse.debug.ui.ILaunchConfigurationTab;

public class JavaTabGroup extends AbstractLaunchConfigurationTabGroup {

	@Override
	public void createTabs(ILaunchConfigurationDialog dialog, String mode) {
		//you have to set some tabs (not null) .. JavaTab is configured in plugin extension and must not be added
		ILaunchConfigurationTab[] tabs = new ILaunchConfigurationTab[] {};
		setTabs(tabs);
	}

}
