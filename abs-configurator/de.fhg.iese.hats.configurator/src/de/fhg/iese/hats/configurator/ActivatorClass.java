package de.fhg.iese.hats.configurator;
import org.osgi.framework.BundleContext;

import de.ovgu.featureide.fm.ui.AbstractUIPlugin;

public class ActivatorClass extends AbstractUIPlugin {

	public static final String PLUGIN_ID = "de.fhg.iese.hats.configurator";

	private static ActivatorClass plugin;
	
	@Override
	public String getID() {
		return PLUGIN_ID;
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.ui.plugin.AbstractUIPlugin#start(org.osgi.framework.BundleContext)
	 */
	public void start(BundleContext context) throws Exception {
		super.start(context);
		plugin = this;
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
	public static ActivatorClass getDefault() {
		return plugin;
	}

}

