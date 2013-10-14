package costabs.preferences;



import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

import org.eclipse.jface.preference.BooleanFieldEditor;
import org.eclipse.jface.preference.ComboFieldEditor;
import org.eclipse.jface.preference.FieldEditor;
import org.eclipse.jface.preference.IntegerFieldEditor;
import org.eclipse.jface.preference.PreferencePage;
import org.eclipse.jface.preference.RadioGroupFieldEditor;
import org.eclipse.jface.preference.ScaleFieldEditor;
import org.eclipse.jface.preference.StringFieldEditor;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.TabFolder;
import org.eclipse.swt.widgets.TabItem;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPreferencePage;

import costabs.Activator;
import costabs.beans.Analyses;
import costabs.beans.Analysis;
import costabs.beans.Option;
import costabs.beans.OptionValue;
import costabs.console.ConsoleHandler;
import costabs.dialogs.DialogPrinter;
import costabs.exceptions.CostabsException;

/**
 * This class represents a preference page that
 * is contributed to the Preferences dialog. By 
 * subclassing <samp>FieldEditorPreferencePage</samp>, we
 * can use the field support built into JFace that allows
 * us to create a page that is small and knows how to 
 * save, restore and apply itself.
 * <p>
 * This page is used to modify preferences only. They
 * are stored in the preference store that belongs to
 * the main plug-in class. That way, preferences can
 * be accessed directly via the preference store.
 */

public class CostabsPreferences extends PreferencePage implements IWorkbenchPreferencePage {

	public static final String OPT_BOOLEAN = "boolean";
	public static final String OPT_COMBO = "combo";
	public static final String OPT_INTEGER = "integer";
	public static final String OPT_RADIO = "radio";
	public static final String OPT_STRING = "string";
	public static final String OPT_SCALE = "scale";

	private HashMap<String,ArrayList<FieldEditor>> fields;

	private String anSelected;


	private void init () {
		setPreferenceStore(Activator.getDefault().getPreferenceStore());

		try {
			PreferencesManager.getInstance(); // Reading and initializing the options file
		} catch (CostabsException e) {
			DialogPrinter.logError(new Exception ("Error while loading the Costabs plugin, an error has ocurred while reading the options: " + e.getMessage()));
		} 
		fields = new HashMap<String, ArrayList<FieldEditor>>();
	}

	public CostabsPreferences() {
		super("SACO Preferences");
		init();
	}

	/**
	 * Creates the preferences window showing the tab related to idAnalysis
	 * @param idAnalysis The analysis options to be shown
	 */
	public CostabsPreferences(String anSelected) {
		super("SACO Preferences");
		init();
		this.anSelected = anSelected;
	}


	@Override
	protected Control createContents(Composite parent) {
		Analyses analyses;
		try {
			analyses = PreferencesManager.getInstance().getAnalyses();
			
			TabFolder tabs = new TabFolder(parent, 1 | SWT.V_SCROLL | SWT.H_SCROLL);

			int selected = 0; 
			for(int i = 0; i < analyses.getAnalyses().size(); i++)	 {
				TabItem tabItem = new TabItem(tabs, SWT.NONE);
				tabItem.setText(analyses.getAnalyses().get(i).getDescription().getShortdesc() );

				Composite form = new Composite(tabs, SWT.NONE);
				tabItem.setControl(form);	
				ArrayList<FieldEditor> lfields = createFields(analyses.getAnalyses().get(i),form);
				this.fields.put(analyses.getAnalyses().get(i).getDescription().getShortdesc(), lfields);
				if (analyses.getAnalyses().get(i).getAnalysisId().equals(anSelected)) {
					selected = i;
				}
			}
			tabs.setSelection(selected);
			
			return new Composite(parent, SWT.NULL);
		} catch (CostabsException e) {
			DialogPrinter.logError(new Exception ("Error while loading the Costabs plugin, an error has ocurred while reading the options: " + e.getMessage()));
			return null;
		}

	}

	private ArrayList<FieldEditor> createFields (Analysis analysis, Composite tab) throws CostabsException {
		ArrayList<FieldEditor> fields = new ArrayList<FieldEditor>();
		List<Option> options = analysis. getOptions().getOptions();

		for(int i = 0; i < options.size(); i ++) {
			FieldEditor fe = createFieldEditor(analysis.getAnalysisId(),options.get(i), tab);
			if (fe.getNumberOfControls() == 1) {
				fe.fillIntoGrid(tab, 1);
			}
			fe.setPage(this);
			fe.setPreferenceStore(getPreferenceStore());
			fe.load();
			fields.add(fe);
		}

		return fields;
	}

	private FieldEditor createFieldEditor (String analysis, Option opt, Composite parent) throws CostabsException{
		FieldEditor fe = null;
		
		String optid = PreferencesManager.getInstance().getOptionId(analysis, opt.getOptname());

		String type = opt.getType();

		if (OPT_BOOLEAN.equals(type)) {
			fe = new BooleanFieldEditor(optid, opt.getDescription().getShortdesc(), parent);
		}
		else if (OPT_INTEGER.equals(type)) {
			fe = new IntegerFieldEditor(optid, opt.getDescription().getShortdesc(), parent);
		}
		else if (OPT_RADIO.equals(type)) {
			String [][] options = createValues (opt);
			fe = new RadioGroupFieldEditor(optid, opt.getDescription().getShortdesc(),1, options, parent);
		}
		else if (OPT_STRING.equals(type)) {
			fe = new StringFieldEditor(optid, opt.getDescription().getShortdesc(), parent);
		}
		else if (OPT_SCALE.equals(type)) {
			fe = new ScaleFieldEditor(optid, opt.getDescription().getShortdesc(), parent);
		}
		else if (OPT_COMBO.equals(type)) {
			String [][] options = createValues (opt);
			fe = new ComboFieldEditor(optid, opt.getDescription().getShortdesc(), options, parent);
		}
		else {
			ConsoleHandler.write("Preference option type " + type + " not found for analysis " + analysis);
			new StringFieldEditor(optid, opt.getDescription().getShortdesc(), parent);
		}
		return fe;
	}


	private String [][] createValues (Option option) {
		List<OptionValue> values = option.getValues().getValues();
		String [][] strs = new String[values.size()][2];
		for (int i = 0; i < option.getValues().getValues().size(); i++) {
			String [] st = new String[2];
			st[0] = values.get(i).getDescription().getShortdesc(); 
			st[1] = values.get(i).getVname();
			strs[i] = st;
		}
		return strs;
	}

	protected void performDefaults() {
		for(String key: fields.keySet()) {
			ArrayList<FieldEditor> fes = fields.get(key);
			for(FieldEditor fe: fes) {
				fe.loadDefault();
			}
		}
		super.performDefaults();
	}

	public boolean performOk() {

		for(String key: fields.keySet()) {
			ArrayList<FieldEditor> fes = fields.get(key);
			for(FieldEditor fe: fes) {
				fe.store();
			}
		}
		return true;
	}

	public void performApply () {
		for(String key: fields.keySet()) {
			ArrayList<FieldEditor> fes = fields.get(key);
			for(FieldEditor fe: fes) {
				fe.store();
			}
		}
	}


	@Override
	public void init(IWorkbench workbench) {
	}

}
