package costabs.preferences;



import org.eclipse.jface.preference.*;
import org.eclipse.ui.IWorkbenchPreferencePage;
import org.eclipse.ui.IWorkbench;

import costabs.Activator;


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

public class CostabsPreferences
extends FieldEditorPreferencePage
implements IWorkbenchPreferencePage {
	//RadioGroupFieldEditor sizeNorm;
	RadioGroupFieldEditor tdPrecision;
	BooleanFieldEditor enableSabu;
	RadioGroupFieldEditor fieldAbstraction;
	RadioGroupFieldEditor verbosity;
	BooleanFieldEditor stdLib;

	RadioGroupFieldEditor generateJUnit;
	public CostabsPreferences() {
		super(GRID);
		setPreferenceStore(Activator.getDefault().getPreferenceStore());
	}

	public void createFieldEditors() {

		addField(new RadioGroupFieldEditor(PreferenceConstants.PCOST_MODEL,"Cost model:",
				1,new String[][] { { "Termination", "termination" }, {"Steps", "steps" }, 
				{"Memory", "memory" }, {"Objects", "objects" }, {"Task level", "task_level" },
				{"User-defined", "user" }}, getFieldEditorParent()));
		addField(new RadioGroupFieldEditor(PreferenceConstants.PCOST_CENTER,"Enable cost centers:",
				1,new String[][] { { "Yes", "yes" }, {"No", "no" }}, getFieldEditorParent()));
		addField(new RadioGroupFieldEditor(PreferenceConstants.PSIZE_ABST,"Size norm:",
				1,new String[][] { { "Term size", "term_size" }, {"Term depth", "term_depth" }}, getFieldEditorParent()));
		
		addField(new RadioGroupFieldEditor(PreferenceConstants.PDEBUG_MODE,"Debugging mode:",
				1,new String[][] { { "Yes", "yes" }, {"No", "no" }}, getFieldEditorParent()));
		
		addField(new RadioGroupFieldEditor(PreferenceConstants.PASYMPTOTIC,"Asymptotic bounds:",
				1,new String[][] { { "Yes", "yes" }, {"No", "no" }}, getFieldEditorParent()));
		
		addField(new RadioGroupFieldEditor(PreferenceConstants.PVERBOSITY,"Verbosity:",
				1,new String[][] { {"0", "0"}, {"1", "1"}, {"2", "2"}}, getFieldEditorParent()));
	}

	public void init(IWorkbench workbench) {
	}
	
}
