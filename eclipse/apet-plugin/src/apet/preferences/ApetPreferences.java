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
	IntegerFieldEditor kl;
	IntegerFieldEditor ks;
	IntegerFieldEditor kstr;
	IntegerFieldEditor min;
	IntegerFieldEditor max;
	IntegerFieldEditor maxPrior;
	BooleanFieldEditor aliasing;
	BooleanFieldEditor tracing;
	RadioGroupFieldEditor pruning;
	RadioGroupFieldEditor maxQL;
	RadioGroupFieldEditor sched;
	RadioGroupFieldEditor verbosity;
	RadioGroupFieldEditor numOrConst;
	RadioGroupFieldEditor selCrit;

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
		kl.store();
		ks.store();
		kstr.store();
		pruning.store();
		tracing.store();
		maxPrior.store();
		maxQL.store();
		sched.store();
		numOrConst.store();
		min.store();
		max.store();
		if (aliasing != null) aliasing.store();
		verbosity.store();
		selCrit.store();
		return success;
	}

	/*
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
	 */

	public void createFieldEditors() {
		kl = new IntegerFieldEditor(PreferenceConstants.KL, "Limit on loop iterations:",getFieldEditorParent());
		addField(kl);
		ks = new IntegerFieldEditor(PreferenceConstants.KS, "Limit on task switchings per object:",getFieldEditorParent());
		addField(ks);
		kstr = new IntegerFieldEditor(PreferenceConstants.KSTR, "Recursion limit on string builtins:",getFieldEditorParent());
		addField(kstr);
		
		numOrConst = new RadioGroupFieldEditor(PreferenceConstants.NUM_OR_CONST,"Concrete test-cases or path-constraints:",
				4,new String[][] { { "Concrete","num" }, {"Path-constraints", "constraint" }}, getFieldEditorParent());
		addField(numOrConst);
		
		min=new IntegerFieldEditor(PreferenceConstants.DOM_MIN, "Integer domain minimum:",getFieldEditorParent());
		max=new IntegerFieldEditor(PreferenceConstants.DOM_MAX, "Integer domain maximum:",getFieldEditorParent());
		min.setValidRange(Integer.MIN_VALUE, Integer.MAX_VALUE);
		min.setErrorMessage("The domain minimum must be an integer");
		addField(min);
		max.setValidRange(Integer.MIN_VALUE, Integer.MAX_VALUE);
		max.setErrorMessage("The domain maximum must be an integer");
		addField(max);
		if (Activator.getDefault().getPreferenceStore().getString(PreferenceConstants.NUM_OR_CONST).equals("num")){
			min.setEnabled(true, this.getFieldEditorParent());
			max.setEnabled(true, this.getFieldEditorParent());
		} else {
			min.setEnabled(false, this.getFieldEditorParent());
			max.setEnabled(false, this.getFieldEditorParent());
		}
		
		selCrit = new RadioGroupFieldEditor(PreferenceConstants.SEL_CRIT,"Selection criterion:",
				4,new String[][] { { "All paths","all_paths" }, {"All local paths", "alp" }}, getFieldEditorParent());
		addField(selCrit);
		
		maxQL = new RadioGroupFieldEditor(PreferenceConstants.MAXQL,"Maximum number of task interleavings (max. queue length):",
				4,new String[][] { { "0", "0" }, {"1", "1" }, {"2", "2" }}, getFieldEditorParent());
		addField(maxQL);

		pruning = new RadioGroupFieldEditor(PreferenceConstants.PRUNING,"Pruning in task interleavings:",
				4,new String[][] { { "3", "3" }, {"2", "2" }, {"1", "1" }}, getFieldEditorParent());
		addField(pruning);
		
		sched = new RadioGroupFieldEditor(PreferenceConstants.SCHED_POLICY,"Scheduling policy:",
				4,new String[][] { { "fifo", "fifo" }, {"lifo", "lifo" }, {"prior", "prior" }}, getFieldEditorParent());
		addField(sched);

		maxPrior = new IntegerFieldEditor(PreferenceConstants.MAX_PRIOR, "Number of priorites:",getFieldEditorParent());
		addField(maxPrior);
						
		aliasing = new BooleanFieldEditor(PreferenceConstants.ALIASING,
				"References aliasing", getFieldEditorParent());
		addField(aliasing);

		tracing = new BooleanFieldEditor(PreferenceConstants.TRACING,
				"Compute/display the trace associated to each test case", getFieldEditorParent());
		addField(tracing);
		
		verbosity = new RadioGroupFieldEditor(PreferenceConstants.VERBOSITY,"Verbosity:",
				4,new String[][] { { "0", "0" }, {"1", "1" }, {"2", "2" }}, getFieldEditorParent());
		addField(verbosity);
	}

	public void init(IWorkbench workbench) {

	}

}
