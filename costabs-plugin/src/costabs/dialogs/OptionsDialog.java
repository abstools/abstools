package costabs.dialogs;


import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.layout.RowLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;

import costabs.Activator;
import costabs.console.ConsoleHandler;
import costabs.preferences.CostabsOptions;
import costabs.preferences.PreferenceConstants;


public class OptionsDialog extends Dialog {

	private Button[] costModelSelection;
	private Button[] costCentersSelection;
	private Button[] sizeAbstractionSelection;
	private Button[] debugModeSelection;
	private Button[] asymptoticSelection;
	private Button[] verbositySelection;

	public OptionsDialog(Shell parentShell) {
		super(parentShell);
	}

	protected Control createDialogArea(Composite parent) {


		Composite composite = (Composite)super.createDialogArea(parent);
		try {
			RowLayout verticalLayout = new RowLayout();
			verticalLayout.type = SWT.VERTICAL;
			verticalLayout.marginWidth = 10;
			verticalLayout.marginHeight = 10;
			composite.setLayout(verticalLayout);
			
			createOptionsPanel(composite);
			createOptionsListeners();
			
			setOptions();
		}
		catch (Exception e) {
			ConsoleHandler.write("Error creating Dialog ");
		}

		return composite;
	}
	
	private void createOptionsPanel(Composite composite) {
		
		Composite costModelPanel = new Composite(composite,SWT.NONE);
		costModelSelection = new Button[CostabsOptions.COST_MODEL.length];
		createOption(costModelPanel, costModelSelection, CostabsOptions.COST_MODEL_TITLE, CostabsOptions.COST_MODEL);
		
		Composite costCentersPanel = new Composite(composite,SWT.NONE);
		costCentersSelection = new Button[CostabsOptions.COST_CENTERS.length];
		createOption(costCentersPanel, costCentersSelection, CostabsOptions.COST_CENTERS_TITLE, CostabsOptions.COST_CENTERS);
		
		Composite sizeAbstractionPanel = new Composite(composite,SWT.NONE);
		sizeAbstractionSelection = new Button[CostabsOptions.SIZE_NORM.length];
		createOption(sizeAbstractionPanel, sizeAbstractionSelection, CostabsOptions.SIZE_NORM_TITLE, CostabsOptions.SIZE_NORM);
		
		Composite debugModePanel = new Composite(composite,SWT.NONE);
		debugModeSelection = new Button[CostabsOptions.DEBUG_MODE.length];
		createOption(debugModePanel, debugModeSelection, CostabsOptions.DEBUG_MODE_TITLE, CostabsOptions.DEBUG_MODE);
		
		Composite asymptoticPanel = new Composite(composite,SWT.NONE);
		asymptoticSelection = new Button[CostabsOptions.ASYMPTOTIC.length];
		createOption(asymptoticPanel, asymptoticSelection, CostabsOptions.ASYMPTOTIC_TITLE, CostabsOptions.ASYMPTOTIC);
		
		Composite verbosityPanel = new Composite(composite,SWT.NONE);
		verbositySelection = new Button[CostabsOptions.VERBOSITY.length];
		createOption(verbosityPanel, verbositySelection, CostabsOptions.VERBOSITY_TITLE, CostabsOptions.VERBOSITY);
		
	}
	
	private void createOption(Composite c, Button[] buttons, String title, String[] options) {
		
		RowLayout verticalLayout = new RowLayout();
		verticalLayout.type = SWT.VERTICAL;
		c.setLayout(verticalLayout);
		
		Label label = new Label (c, SWT.NULL);
		label.setText(title + ":");
		
		Composite optionsPanel = new Composite(c,SWT.NONE);
		optionsPanel.setLayout(new RowLayout());
		
		for (int i = 0; i < options.length; i++) {
			buttons[i] = new Button(optionsPanel, SWT.RADIO);
			buttons[i].setText(options[i]);
		}
	}
	
	private void createOptionsListeners() {
		
		for (int i = 0; i < costModelSelection.length; i++)
			costModelSelection[i].addSelectionListener(new SelectCostModelChangeListener(this, i, costModelSelection[i]));
		
		for (int i = 0; i < costCentersSelection.length; i++)
			costCentersSelection[i].addSelectionListener(new SelectCostCentersChangeListener(this, i, costCentersSelection[i]));
		
		for (int i = 0; i < sizeAbstractionSelection.length; i++)
			sizeAbstractionSelection[i].addSelectionListener(new SelectSizeAbstractionChangeListener(this, i, sizeAbstractionSelection[i]));
		
		for (int i = 0; i < asymptoticSelection.length; i++)
			asymptoticSelection[i].addSelectionListener(new SelectAsymptoticChangeListener(this, i, asymptoticSelection[i]));
		
		for (int i = 0; i < debugModeSelection.length; i++)
			debugModeSelection[i].addSelectionListener(new SelectDebugModeChangeListener(this, i, debugModeSelection[i]));
		
		for (int i = 0; i < verbositySelection.length; i++)
			verbositySelection[i].addSelectionListener(new SelectVerbosityChangeListener(this, i, verbositySelection[i]));
	}
	
	private void setOptions() {
		
		IPreferenceStore store = Activator.getDefault().getPreferenceStore();
		
		String preferenceValue = "";
		
		preferenceValue = store.getString(PreferenceConstants.PCOST_MODEL);
		for (int i = 0; i < CostabsOptions.COST_MODEL_PROLOG.length; i++)			
			costModelSelection[i].setSelection(preferenceValue.equals(CostabsOptions.COST_MODEL_PROLOG[i]));
		
		preferenceValue = store.getString(PreferenceConstants.PCOST_CENTER);
		for (int i = 0; i < CostabsOptions.COST_CENTERS.length; i++)			
			costCentersSelection[i].setSelection(preferenceValue.equals(CostabsOptions.COST_CENTERS_PROLOG[i]));
		
		preferenceValue = store.getString(PreferenceConstants.PSIZE_ABST);
		for (int i = 0; i < CostabsOptions.SIZE_NORM.length; i++)			
			sizeAbstractionSelection[i].setSelection(preferenceValue.equals(CostabsOptions.SIZE_NORM_PROLOG[i]));
		
		preferenceValue = store.getString(PreferenceConstants.PASYMPTOTIC);
		for (int i = 0; i < CostabsOptions.ASYMPTOTIC.length; i++)			
			asymptoticSelection[i].setSelection(preferenceValue.equals(CostabsOptions.ASYMPTOTIC[i]));
		
		preferenceValue = store.getString(PreferenceConstants.PDEBUG_MODE);
		for (int i = 0; i < CostabsOptions.DEBUG_MODE.length; i++)			
			debugModeSelection[i].setSelection(preferenceValue.equals(CostabsOptions.DEBUG_MODE[i]));
		
		preferenceValue = store.getString(PreferenceConstants.PVERBOSITY);
		for (int i = 0; i < CostabsOptions.VERBOSITY.length; i++)			
			verbositySelection[i].setSelection(preferenceValue.equals(CostabsOptions.VERBOSITY[i]));
		
	}

	public Button[] getCostModel() {
		return costModelSelection;
	}
	
	public Button[] getCostCenters() {
		return costCentersSelection;
	}
	
	public Button[] getSizeAbstraction() {
		return sizeAbstractionSelection;
	}
	
	public Button[] getAsymptotic() {
		return asymptoticSelection;
	}
	
	public Button[] getDebugMode() {
		return debugModeSelection;
	}
	
	public Button[] getVerbosity() {
		return verbositySelection;
	}

	@Override
	protected void configureShell(Shell newShell) {
		super.configureShell(newShell);
		newShell.setText("Select the preferences for Costa");
	}

	@Override
	protected void okPressed() {
		this.close();
	}

	@Override
	protected void createButtonsForButtonBar(Composite parent) {
		GridLayout layout = new GridLayout (1,true);
		parent.setLayout(layout);
		parent.pack();

		super.createButtonsForButtonBar(parent);
		this.getButton(IDialogConstants.OK_ID).setText("Analyze");
	}

}

class SelectCostModelChangeListener implements SelectionListener {

	OptionsDialog dialog = null;
	int index;
	Button b;

	public SelectCostModelChangeListener(OptionsDialog dialog, int i, Button b) {
		this.dialog = dialog;
		this.index = i;
		this.b = b;
	}

	public void widgetDefaultSelected(SelectionEvent e) {
	}

	public void widgetSelected(SelectionEvent e) {
		for (int i = 0; i < this.dialog.getCostModel().length; i++) {
			this.dialog.getCostModel()[i].setSelection(false);
		}
		b.setSelection(true);
		
		IPreferenceStore store = Activator.getDefault().getPreferenceStore();
		store.setValue(PreferenceConstants.PCOST_MODEL, CostabsOptions.COST_MODEL_PROLOG[index]);
	}

}

class SelectCostCentersChangeListener implements SelectionListener {

	OptionsDialog dialog = null;
	int index;
	Button b;

	public SelectCostCentersChangeListener(OptionsDialog dialog, int i, Button b) {
		this.dialog = dialog;
		this.index = i;
		this.b = b;
	}

	public void widgetDefaultSelected(SelectionEvent e) {
	}

	public void widgetSelected(SelectionEvent e) {
		for (int i = 0; i < this.dialog.getCostCenters().length; i++) {
			this.dialog.getCostCenters()[i].setSelection(false);
		}
		b.setSelection(true);
		
		IPreferenceStore store = Activator.getDefault().getPreferenceStore();
		store.setValue(PreferenceConstants.PCOST_CENTER, CostabsOptions.COST_CENTERS_PROLOG[index]);
	}

}

class SelectSizeAbstractionChangeListener implements SelectionListener {

	OptionsDialog dialog = null;
	int index;
	Button b;

	public SelectSizeAbstractionChangeListener(OptionsDialog dialog, int i, Button b) {
		this.dialog = dialog;
		this.index = i;
		this.b = b;
	}

	public void widgetDefaultSelected(SelectionEvent e) {
	}

	public void widgetSelected(SelectionEvent e) {
		for (int i = 0; i < this.dialog.getSizeAbstraction().length; i++) {
			this.dialog.getSizeAbstraction()[i].setSelection(false);
		}
		b.setSelection(true);
		
		IPreferenceStore store = Activator.getDefault().getPreferenceStore();
		store.setValue(PreferenceConstants.PSIZE_ABST, CostabsOptions.SIZE_NORM_PROLOG[index]);
	}
}

class SelectAsymptoticChangeListener implements SelectionListener {

	OptionsDialog dialog = null;
	int index;
	Button b;

	public SelectAsymptoticChangeListener(OptionsDialog dialog, int i, Button b) {
		this.dialog = dialog;
		this.index = i;
		this.b = b;
	}

	public void widgetDefaultSelected(SelectionEvent e) {
	}

	public void widgetSelected(SelectionEvent e) {
		for (int i = 0; i < this.dialog.getAsymptotic().length; i++) {
			this.dialog.getAsymptotic()[i].setSelection(false);
		}
		b.setSelection(true);
		
		IPreferenceStore store = Activator.getDefault().getPreferenceStore();
		store.setValue(PreferenceConstants.PASYMPTOTIC, CostabsOptions.ASYMPTOTIC[index]);
	}
}

class SelectDebugModeChangeListener implements SelectionListener {

	OptionsDialog dialog = null;
	int index;
	Button b;

	public SelectDebugModeChangeListener(OptionsDialog dialog, int i, Button b) {
		this.dialog = dialog;
		this.index = i;
		this.b = b;
	}

	public void widgetDefaultSelected(SelectionEvent e) {
	}

	public void widgetSelected(SelectionEvent e) {
		for (int i = 0; i < this.dialog.getDebugMode().length; i++) {
			this.dialog.getDebugMode()[i].setSelection(false);
		}
		b.setSelection(true);
		
		IPreferenceStore store = Activator.getDefault().getPreferenceStore();
		store.setValue(PreferenceConstants.PDEBUG_MODE, CostabsOptions.DEBUG_MODE[index]);
	}
}

class SelectVerbosityChangeListener implements SelectionListener {

	OptionsDialog dialog = null;
	int index;
	Button b;

	public SelectVerbosityChangeListener(OptionsDialog dialog, int i, Button b) {
		this.dialog = dialog;
		this.index = i;
		this.b = b;
	}

	public void widgetDefaultSelected(SelectionEvent e) {
	}

	public void widgetSelected(SelectionEvent e) {
		for (int i = 0; i < this.dialog.getVerbosity().length; i++) {
			this.dialog.getVerbosity()[i].setSelection(false);
		}
		b.setSelection(true);
		
		IPreferenceStore store = Activator.getDefault().getPreferenceStore();
		store.setValue(PreferenceConstants.PVERBOSITY, CostabsOptions.VERBOSITY[index]);
	}
}
