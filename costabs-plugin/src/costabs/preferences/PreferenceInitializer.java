package costabs.preferences;

import org.eclipse.core.runtime.preferences.AbstractPreferenceInitializer;
import org.eclipse.jface.preference.IPreferenceStore;

import costabs.Activator;


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

		store.setDefault(PreferenceConstants.PSIZE_ABST, "term_size");
		store.setDefault(PreferenceConstants.PCOST_MODEL, "steps");
		store.setDefault(PreferenceConstants.PCOST_CENTER, "no");
		store.setDefault(PreferenceConstants.PDEBUG_MODE, "no");
		store.setDefault(PreferenceConstants.PASYMPTOTIC, "no");
		store.setDefault(PreferenceConstants.PVERBOSITY, "2");
		store.setDefault(PreferenceConstants.PSTDLIB, true);
	}

}
