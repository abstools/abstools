package apet.preferences;



import org.eclipse.jface.util.PropertyChangeEvent;
import org.eclipse.jface.preference.*;
import org.eclipse.ui.IWorkbenchPreferencePage;
import org.eclipse.ui.IWorkbench;

import apet.Activator;


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

public class ApetPreferences
extends FieldEditorPreferencePage
implements IWorkbenchPreferencePage {
	RadioGroupFieldEditor coverageCriterion;
	IntegerFieldEditor coverage;
	IntegerFieldEditor minRange;
	IntegerFieldEditor maxRange;
	BooleanFieldEditor aliasing;
	RadioGroupFieldEditor verbosity;
	RadioGroupFieldEditor tracing;
	RadioGroupFieldEditor numeric;

	public ApetPreferences() {
		super(GRID);
		setPreferenceStore(Activator.getDefault().getPreferenceStore());
	}

	/**
	 * Creates the field editors. Field editors are abstractions of
	 * the common GUI blocks needed to manipulate various types
	 * of preferences. Each field editor knows how to save and
	 * restore itself.
	 */

	public boolean performOk() {
		boolean success = true;
		coverageCriterion.store();
		coverage.store();
		minRange.store();
		maxRange.store();
		aliasing.store();
		verbosity.store();
		tracing.store();
		return success;
	}

	public void propertyChange(PropertyChangeEvent event) {
		super.propertyChange(event);
		if(numeric.equals(event.getSource())){
			if (((String)event.getNewValue()).equals("num")){
				minRange.setEnabled(true, this.getFieldEditorParent());
				maxRange.setEnabled(true, this.getFieldEditorParent());
			} else{
				minRange.setEnabled(false, this.getFieldEditorParent());
				maxRange.setEnabled(false, this.getFieldEditorParent());
			}
		}

	}


	public void createFieldEditors() {

		coverageCriterion = new RadioGroupFieldEditor(PreferenceConstants.PCOVERAGE_CRITERION,"Coverage criterion:",
				1,new String[][] { { "Block-k", "bck" }, {"Depth-k", "dpk" }}, getFieldEditorParent());
		addField(coverageCriterion);

		coverage = new IntegerFieldEditor(PreferenceConstants.PCOVERAGE_CRITERION_NUM, "Coverage:",getFieldEditorParent());
		addField(coverage);

		numeric = new RadioGroupFieldEditor(PreferenceConstants.PNUMERIC,"Concrete test-cases or path-constraints:",
				1,new String[][] { { "Concrete (In this case a range must be especified below) ", "num" }, {"Path-constraints", "constraint" }}, getFieldEditorParent());

		addField(numeric);
		minRange=new IntegerFieldEditor(PreferenceConstants.PRANGEMIN, "From",getFieldEditorParent());
		maxRange=new IntegerFieldEditor(PreferenceConstants.PRANGEMAX, "To",getFieldEditorParent());

		minRange.setValidRange(Integer.MIN_VALUE, Integer.MAX_VALUE);
		minRange.setErrorMessage("The lower bound must be an integer");
		addField(minRange);
		maxRange.setValidRange(Integer.MIN_VALUE, Integer.MAX_VALUE);
		maxRange.setErrorMessage("The upper bound must be an integer");
		addField(maxRange);

		if (Activator.getDefault().getPreferenceStore().getString(PreferenceConstants.PNUMERIC).equals("num")){
			minRange.setEnabled(true, this.getFieldEditorParent());
			maxRange.setEnabled(true, this.getFieldEditorParent());
		} else{
			minRange.setEnabled(false, this.getFieldEditorParent());
			maxRange.setEnabled(false, this.getFieldEditorParent());
		}

		aliasing = new BooleanFieldEditor(
				PreferenceConstants.REFERENCES_ALIASING,
				"References aliasing",
				getFieldEditorParent());
		addField(aliasing);

		verbosity = new RadioGroupFieldEditor(PreferenceConstants.PVERBOSITY,"Verbosity:",
				4,new String[][] { { "0", "0" }, {"1", "1" }, {"2", "2" }}, getFieldEditorParent());
		addField(verbosity);

		tracing = new RadioGroupFieldEditor(PreferenceConstants.PTRACING,"Tracing:",
				4,new String[][] { { "no", "none" }, {"yes", "statements" }}, getFieldEditorParent());
		addField(tracing);
	}

	public void init(IWorkbench workbench) {

	}

}
