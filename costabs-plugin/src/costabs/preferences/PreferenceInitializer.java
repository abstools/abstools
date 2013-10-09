package costabs.preferences;

import org.eclipse.core.runtime.preferences.AbstractPreferenceInitializer;
import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.jface.preference.StringFieldEditor;

import costabs.Activator;
import costabs.beans.Analyses;
import costabs.beans.Analysis;
import costabs.beans.Option;
import costabs.console.ConsoleHandler;
import costabs.dialogs.DialogPrinter;
import costabs.exceptions.CostabsException;
import costabs.structures.CostabsConstants;


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

		Analyses analyses;
		try {
			analyses = PreferencesManager.getInstance().getAnalyses();

			for(int i = 0; i < analyses.getAnalyses().size();  i ++) {
				Analysis analysis = analyses.getAnalyses().get(i);

				for (int j = 0; j < analysis.getOptions().getOptions().size(); j++) {
					Option option = analysis.getOptions().getOptions().get(j);
					String optid = PreferencesManager.getInstance().getOptionId(analysis.getAnalysisId(),option.getOptname());
//					String optid = analysis.getAnalysisId() + "_" + option.getOptname();
					if (PreferencesManager.getInstance().isBooleanOption(optid)) {
						if (CostabsConstants.BOOLEAN_TRUE.equals(option.getDefaultValue())) {
							store.setDefault(optid, true);
						}
						else {
							store.setDefault(optid, false);
						}
					}
					else {
						store.setDefault(optid, option.getDefaultValue());
					}
				}
			}
		} catch (CostabsException e) {
			DialogPrinter.logError(new Exception ("Preferences cannot be initialized. Error: " + e.getMessage(), e));
		}

	}

}
