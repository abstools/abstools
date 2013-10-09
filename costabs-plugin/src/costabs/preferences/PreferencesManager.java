package costabs.preferences;

import java.io.IOException;
import java.io.InputStream;
import java.net.URL;
import java.util.HashMap;

import org.eclipse.core.runtime.FileLocator;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.Platform;
import org.osgi.framework.Bundle;

import costabs.beans.Analyses;
import costabs.beans.Analysis;
import costabs.beans.Option;
import costabs.exceptions.CostabsException;
import costabs.structures.CostabsConstants;
import costabs.structures.CostabsXMLFrontend;


public class PreferencesManager {

	private Analyses analyses;

	private static PreferencesManager manager;

	private HashMap<String, String> optionsTypes;
	
	private PreferencesManager() throws CostabsException {
		Bundle bundle = Platform.getBundle(CostabsConstants.PLUGIN_ID);
		Path path = new Path(CostabsConstants.OPTIONS_FILE);
		URL fileURL = FileLocator.find(bundle, path, null);
		
		try {
			InputStream in = fileURL.openStream();
			analyses = CostabsXMLFrontend.readAnalyses(in);
			saveOptionTypes();
			
		} catch (IOException e) {
			throw new CostabsException("Cannot read the analyses file '" + CostabsConstants.OPTIONS_FILE+ "'. The plugin will not work properly", e);
		}
	}

	private void saveOptionTypes () {
		optionsTypes = new HashMap<String, String>();
		for(Analysis analysis: analyses.getAnalyses()){
			if (analysis.getOptions() == null) {
				continue;
			}
			for(Option option: analysis.getOptions().getOptions()){
				String optid = getOptionId(analysis.getAnalysisId(),option.getOptname());
				optionsTypes.put(optid, option.getType());
			}
		}
	}
	
	public static PreferencesManager getInstance () throws CostabsException {
		if (PreferencesManager.manager == null) {
			PreferencesManager.manager = new PreferencesManager ();
		}
		return PreferencesManager.manager;
	}

	public Analyses getAnalyses() {
		return analyses;
	}

	public Analysis getAnalysis(String idAnalysis) throws CostabsException {
		for(Analysis analysis: analyses.getAnalyses()){
			if (idAnalysis.equals(analysis.getAnalysisId())) {
				return analysis;
			}
		}
		throw new CostabsException("The analysis " + idAnalysis + " was not found in the list of available analyses");
	}
	
	public String getOptionId (String analysis, String option) {
		return analysis + "_" + option;
	}
	
	public String getOptionType (String optId) {
		return optionsTypes.get(optId);
	}
	
	public boolean isBooleanOption (String optString) {
		if ("boolean".equals(optionsTypes.get(optString))) {
			return true;
		}
		else {
			return false;
		}
	}
	
	
}
