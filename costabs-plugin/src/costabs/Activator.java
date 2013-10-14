package costabs;

import java.io.File;

import org.eclipse.core.resources.IResourceChangeEvent;
import org.eclipse.core.resources.IWorkspace;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.*;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.ui.IPartListener2;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchPart;
import org.eclipse.ui.IWorkbenchPartReference;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.plugin.AbstractUIPlugin;
import org.osgi.framework.Bundle;
import org.osgi.framework.BundleContext;

import costabs.console.CostabsShellCommand;
import costabs.listeners.PartListener;
import costabs.listeners.ResourceChangeListener;
import costabs.structures.CostabsConstants;
import costabs.utils.SourceUtils;

/**
 * The activator class controls the plug-in life cycle
 */
public class Activator extends AbstractUIPlugin {


	// The shared instance
	private static Activator plugin;

	/**
	 * The constructor
	 */
	public Activator() {
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.ui.plugin.AbstractUIPlugin#start(org.osgi.framework.BundleContext)
	 */
	public void start(BundleContext context) throws Exception {
		super.start(context);
		plugin = this;
		Bundle petbundle = Platform.getBundle(CostabsConstants.PLUGIN_ID); 
		File dir = FileLocator.getBundleFile(petbundle);
		assert dir.isDirectory() : dir;
		File exe = new File(dir,CostabsConstants.EXEC_DIR);
		assert exe.exists() : dir;
		CostabsShellCommand.COSTABS_EXECUTABLE_PATH = exe.getAbsolutePath() + "/";
		//Runtime.getRuntime().exec("chmod +x " + CostabsShellCommand.COSTABS_EXECUTABLE_PATH + "*");
		String cmd[] = { "bash", "-c", "chmod +x " + CostabsShellCommand.COSTABS_EXECUTABLE_PATH + "*" };
		
		Runtime.getRuntime().exec(cmd);
		
//		Runtime.getRuntime().exec("chmod +x " + CostabsShellCommand.COSTABS_EXECUTABLE_PATH + "costabs_static");
//		Runtime.getRuntime().exec("chmod +x " + CostabsShellCommand.COSTABS_EXECUTABLE_PATH + "deadlock_static");
//		Runtime.getRuntime().exec("chmod +x " + CostabsShellCommand.COSTABS_EXECUTABLE_PATH + "mhp_static");
			
		IWorkspace workspace = ResourcesPlugin.getWorkspace();
		ResourceChangeListener listener = new ResourceChangeListener();
		workspace.addResourceChangeListener(listener, IResourceChangeEvent.POST_BUILD);

		IWorkbench iworkbench = PlatformUI.getWorkbench();
		IWorkbenchWindow iworkbenchwindow = iworkbench.getActiveWorkbenchWindow();
		IWorkbenchPage iworkbenchpage = iworkbenchwindow.getActivePage();
//		adding a listener
		PartListener pl = new PartListener();
		iworkbenchpage.addPartListener(pl);

	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.ui.plugin.AbstractUIPlugin#stop(org.osgi.framework.BundleContext)
	 */
	public void stop(BundleContext context) throws Exception {
		plugin = null;
		super.stop(context);
	}

	/**
	 * Returns the shared instance
	 *
	 * @return the shared instance
	 */
	public static Activator getDefault() {
		return plugin;
	}

	/**
	 * Returns an image descriptor for the image file at the given
	 * plug-in relative path
	 *
	 * @param path the path
	 * @return the image descriptor
	 */
	public static ImageDescriptor getImageDescriptor(String path) {
		return imageDescriptorFromPlugin(CostabsConstants.PLUGIN_ID, path);
	}

	public static void logException(Exception e) {
		getDefault().getLog().log(new Status(IStatus.ERROR, CostabsConstants.PLUGIN_ID, e.getMessage(), e));
	}
}
