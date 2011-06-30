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

		store.setDefault(PreferenceConstants.PCOVERAGE_CRITERION, "bck");
		store.setDefault(PreferenceConstants.PCOVERAGE_CRITERION_NUM, 3);
		store.setDefault(PreferenceConstants.PNUMERIC, "constraint");

		store.setDefault(PreferenceConstants.PRANGEMIN, -10);
		store.setDefault(PreferenceConstants.PRANGEMAX, 10);

		store.setDefault(PreferenceConstants.PTEST_CASE_GENERATION, false);

		store.setDefault(PreferenceConstants.PREFERENCES_ALIASING, false);

		store.setDefault(PreferenceConstants.PLABELING, "max");

		store.setDefault(PreferenceConstants.PVERBOSITY, "2");

		store.setDefault(PreferenceConstants.PCLP,false);

		store.setDefault(PreferenceConstants.PTRACING,"statements");

	}

}
