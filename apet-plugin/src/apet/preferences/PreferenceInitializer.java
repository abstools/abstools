package apet.preferences;

import org.eclipse.core.runtime.preferences.AbstractPreferenceInitializer;
import org.eclipse.jface.preference.IPreferenceStore;

import apet.Activator;


/**
 * Class used to initialize default preference values.
 */
public class PreferenceInitializer extends AbstractPreferenceInitializer {

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.eclipse.core.runtime.preferences.AbstractPreferenceInitializer#initializeDefaultPreferences()
	 */
	public void initializeDefaultPreferences() {
		IPreferenceStore store = Activator.getDefault().getPreferenceStore();
		store.setDefault(PreferenceConstants.KL, 1);
		store.setDefault(PreferenceConstants.KS, 8);
		store.setDefault(PreferenceConstants.KSTR, 1);
		store.setDefault(PreferenceConstants.PRUNING, "3");
		store.setDefault(PreferenceConstants.MAXQL, "0");
		store.setDefault(PreferenceConstants.SCHED_POLICY, "fifo");
		store.setDefault(PreferenceConstants.MAX_PRIOR, 3);
		store.setDefault(PreferenceConstants.NUM_OR_CONST, "num");
		store.setDefault(PreferenceConstants.DOM_MIN, 0);
		store.setDefault(PreferenceConstants.DOM_MAX, 10);
		store.setDefault(PreferenceConstants.ALIASING, false);
		store.setDefault(PreferenceConstants.TRACING, false);
		store.setDefault(PreferenceConstants.VERBOSITY, "2");
		store.setDefault(PreferenceConstants.SEL_CRIT, "all_paths");
	}

}
