package costabs;

import java.io.File;
import java.io.IOException;

import org.eclipse.core.runtime.FileLocator;
import org.eclipse.core.runtime.Platform;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.ui.plugin.AbstractUIPlugin;
import org.osgi.framework.Bundle;
import org.osgi.framework.BundleContext;

import costabs.console.CostabsShellCommand;


/**
 * The activator class controls the plug-in life cycle
 */
public class Activator extends AbstractUIPlugin {

	// The plug-in ID
	public static final String PLUGIN_ID = "eu.hatsproject.costabs";

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
		//prepareCostabsExecutable();
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
		return imageDescriptorFromPlugin(PLUGIN_ID, path);
	}
	
	public static void prepareCostabsExecutable() {
		try {
			// Get the path to the plugin (a jarfile path or a directory path)
			Bundle petbundle = Platform.getBundle(PLUGIN_ID);
			File jarFile = FileLocator.getBundleFile(petbundle).getAbsoluteFile();
			String pluginPlace = jarFile.getAbsolutePath();

			// Create the instance for costabs executable from the directory path
			File costabsExePlace = new File(jarFile,"costabs_exe");

			// If the executable exists, it means that we are in development form
			// else we are in a jar build
			CostabsShellCommand shell = new CostabsShellCommand();
			if (costabsExePlace.exists()) {
				CostabsShellCommand.COSTABS_EXECUTABLE_PATH = costabsExePlace.getAbsolutePath();
			}
			else {				
				// Extract the jar file in the Eclipse directory;
				shell.executeCommand("jar xf " + pluginPlace + " costabs_exe");
				
				// Set the path to the executable
				pluginPlace = jarFile.getParent();
				CostabsShellCommand.COSTABS_EXECUTABLE_PATH = pluginPlace + "/costabs_exe";
				
				// Move the executable in Eclipse directory to plugins directory
				// and set permission to execute it
				shell.executeCommand("mv costabs_exe " + pluginPlace);
				shell.executeCommand("chmod +x " + pluginPlace + "/costabs_exe");
			}

		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}
}
