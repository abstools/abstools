package apet.dialogs;


import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.jface.dialogs.ErrorDialog;
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
import org.eclipse.swt.widgets.Text;

import apet.Activator;
import apet.console.ConsoleHandler;
import apet.preferences.ApetOptions;
import apet.preferences.PreferenceConstants;

/*
public class OptionsDialog extends Dialog {

	private Button[] coverageCriterionSelection;
	private Text[] coverageText;
	private Button[] numericPathConstraintsSelection;
	private Text[] fromText;
	private Text[] toText;
	private Button[] aliasingCheck;
	private Button[] verbositySelection;
	private Button[] tracingSelection;

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
		
		Composite coverageCriterionPanel = new Composite(composite,SWT.NONE);
		coverageCriterionSelection = new Button[ApetOptions.COVERAGE_CRITERION_OPTS.length];
		createOption(coverageCriterionPanel, coverageCriterionSelection, ApetOptions.COVERAGE_CRITERION_TITLE, ApetOptions.COVERAGE_CRITERION_OPTS);
		
		Composite coverageBox = new Composite(composite,SWT.NONE);
		coverageText = new Text[1];
		createText(coverageBox,"Coverage",coverageText);
		
		Composite numPanel = new Composite(composite,SWT.NONE);
		numericPathConstraintsSelection = new Button[ApetOptions.NUM_PATH_CONSTRAINTS_OPTS.length];
		createOption(numPanel, numericPathConstraintsSelection, ApetOptions.NUM_PATH_CONSTRAINTS_TITLE, ApetOptions.NUM_PATH_CONSTRAINTS_OPTS);
			
		Composite fromBox = new Composite(composite,SWT.NONE);
		fromText = new Text[1];
		createText(fromBox,"From",fromText);
		
		Composite toBox = new Composite(composite,SWT.NONE);
		toText = new Text[1];
		createText(toBox,"To",toText);
		
		Composite aliasingPanel = new Composite(composite,SWT.NONE);
		aliasingCheck = new Button[ApetOptions.ALIASING_OPTS.length];
		createOptionCheck(aliasingPanel, aliasingCheck, ApetOptions.ALIASING_OPTS);
		
		Composite verbosityPanel = new Composite(composite,SWT.NONE);
		verbositySelection = new Button[ApetOptions.VERBOSITY.length];
		createOption(verbosityPanel, verbositySelection, ApetOptions.VERBOSITY_TITLE, ApetOptions.VERBOSITY);
		
		Composite tracingPanel = new Composite(composite,SWT.NONE);
		tracingSelection = new Button[ApetOptions.TRACING_OPTS.length];
		createOption(tracingPanel, tracingSelection, ApetOptions.TRACING_TITLE, ApetOptions.TRACING_OPTS);
		
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
	
	private void createText(Composite c, String title, Text[] t) {
		
		RowLayout verticalLayout = new RowLayout();
		verticalLayout.type = SWT.HORIZONTAL;
		c.setLayout(verticalLayout);
		
		Label label = new Label (c, SWT.NULL);
		label.setText(title + ":");
		
		t[0] = new Text(c, SWT.BORDER);
	}
	
	private void createOptionCheck(Composite c, Button[] buttons, String[] options) {
		
		RowLayout verticalLayout = new RowLayout();
		verticalLayout.type = SWT.VERTICAL;
		c.setLayout(verticalLayout);
		
		Composite optionsPanel = new Composite(c,SWT.NONE);
		optionsPanel.setLayout(new RowLayout());
		
		for (int i = 0; i < options.length; i++) {
			buttons[i] = new Button(optionsPanel, SWT.CHECK);
			buttons[i].setText(options[i]);
		}
	}
	
	private void createOptionsListeners() {
		
		for (int i = 0; i < coverageCriterionSelection.length; i++)
			coverageCriterionSelection[i].addSelectionListener(new SelectCoverageCriterionChangeListener(this, i, coverageCriterionSelection[i]));
		
		for (int i = 0; i < numericPathConstraintsSelection.length; i++)
			numericPathConstraintsSelection[i].addSelectionListener(new SelectNumPathConstraintsChangeListener(this, i, numericPathConstraintsSelection[i],fromText[0],toText[0]));
		
		for (int i = 0; i < aliasingCheck.length; i++)
			aliasingCheck[i].addSelectionListener(new SelectAliasingChangeListener(this, i, aliasingCheck[i]));
	
		for (int i = 0; i < verbositySelection.length; i++)
			verbositySelection[i].addSelectionListener(new SelectVerbosityChangeListener(this, i, verbositySelection[i]));
			
		for (int i = 0; i < tracingSelection.length; i++)
			tracingSelection[i].addSelectionListener(new SelectTracingChangeListener(this, i, tracingSelection[i]));
	
	}
	
	private void setOptions() {
		
		IPreferenceStore store = Activator.getDefault().getPreferenceStore();
		
		String preferenceValue = "";
		boolean booleanValue = false;
		int integerValue = 0;
		
		preferenceValue = store.getString(PreferenceConstants.PCOVERAGE_CRITERION);
		for (int i = 0; i < ApetOptions.COVERAGE_CRITERION_PROLOG.length; i++)			
			coverageCriterionSelection[i].setSelection(preferenceValue.equals(ApetOptions.COVERAGE_CRITERION_PROLOG[i]));
		
		integerValue = store.getInt(PreferenceConstants.PCOVERAGE_CRITERION_NUM);
		coverageText[0].setText(String.valueOf(integerValue));
		
		preferenceValue = store.getString(PreferenceConstants.PNUMERIC);
		for (int i = 0; i < ApetOptions.NUM_PATH_CONSTRAINTS_PROLOG.length; i++)			
			numericPathConstraintsSelection[i].setSelection(preferenceValue.equals(ApetOptions.NUM_PATH_CONSTRAINTS_PROLOG[i]));
		
		integerValue = store.getInt(PreferenceConstants.PRANGEMIN);
		fromText[0].setText(String.valueOf(integerValue));
		
		integerValue = store.getInt(PreferenceConstants.PRANGEMAX);
		toText[0].setText(String.valueOf(integerValue));
			
		booleanValue = store.getBoolean(PreferenceConstants.REFERENCES_ALIASING);
		for (int i = 0; i < ApetOptions.ALIASING_OPTS.length; i++)			
			aliasingCheck[i].setSelection(booleanValue);
		
	
		preferenceValue = store.getString(PreferenceConstants.PVERBOSITY);
		for (int i = 0; i < ApetOptions.VERBOSITY.length; i++)			
			verbositySelection[i].setSelection(preferenceValue.equals(ApetOptions.VERBOSITY[i]));
		
		preferenceValue = store.getString(PreferenceConstants.PTRACING);
		for (int i = 0; i < ApetOptions.TRACING_PROLOG.length; i++)			
			tracingSelection[i].setSelection(preferenceValue.equals(ApetOptions.TRACING_PROLOG[i]));
		
		fromText[0].setEnabled(numericPathConstraintsSelection[0].getSelection());
		toText[0].setEnabled(numericPathConstraintsSelection[0].getSelection());
	}

	public Button[] getCoverageCriterion() {
		return coverageCriterionSelection;
	}
	
	public Button[] getNumPathConstraints() {
		return numericPathConstraintsSelection;
	}
	
	public Button[] getAliasing() {
		return aliasingCheck;
	}
	
	public Button[] getVerbosity() {
		return verbositySelection;
	}
	
	public Button[] getTracing() {
		return tracingSelection;
	}
	
	@Override
	protected void configureShell(Shell newShell) {
		super.configureShell(newShell);
		newShell.setText("Select the preferences for aPET");
	}

	@Override
	protected void okPressed() {

		IPreferenceStore store = Activator.getDefault().getPreferenceStore();
		
		try {
			if (!coverageText[0].getText().isEmpty())
				store.setValue(PreferenceConstants.PCOVERAGE_CRITERION_NUM, Integer.parseInt(coverageText[0].getText()));
		}
		catch (Exception e) {
			Status status = new Status(IStatus.ERROR, "jpet", 0,
		            "The number for coverage is not valid.", null);
			ErrorDialog.openError(getParentShell(), "JPet Error", "The number for coverage is not valid.", status);
		}
		
		try {
			if (!fromText[0].getText().isEmpty())
				store.setValue(PreferenceConstants.PRANGEMIN, Integer.parseInt(fromText[0].getText()));
		}
		catch (Exception e) {
			Status status = new Status(IStatus.ERROR, "jpet", 0,
		            "The number for the text field From is not valid.", null);
			ErrorDialog.openError(getParentShell(), "JPet Error", "The number for the text field From is not valid.", status);
		}
		
		try {
			if (!toText[0].getText().isEmpty())
				store.setValue(PreferenceConstants.PRANGEMAX, Integer.parseInt(toText[0].getText()));
		}
		catch (Exception e) {
			Status status = new Status(IStatus.ERROR, "jpet", 0,
		            "The number for the text field To is not valid.", null);
			ErrorDialog.openError(getParentShell(), "JPet Error", "The number for the text field To is not valid.", status);
		}
		
		this.close();
	}

	@Override
	protected void createButtonsForButtonBar(Composite parent) {
		GridLayout layout = new GridLayout (1,true);
		parent.setLayout(layout);
		parent.pack();

		super.createButtonsForButtonBar(parent);
		this.getButton(IDialogConstants.OK_ID).setText("Generate");
	}

}

class SelectCoverageCriterionChangeListener implements SelectionListener {

	OptionsDialog dialog = null;
	int index;
	Button b;

	public SelectCoverageCriterionChangeListener(OptionsDialog dialog, int i, Button b) {
		this.dialog = dialog;
		this.index = i;
		this.b = b;
	}

	public void widgetDefaultSelected(SelectionEvent e) {
	}

	public void widgetSelected(SelectionEvent e) {
		for (int i = 0; i < this.dialog.getCoverageCriterion().length; i++) {
			this.dialog.getCoverageCriterion()[i].setSelection(false);
		}
		b.setSelection(true);
		
		IPreferenceStore store = Activator.getDefault().getPreferenceStore();
		store.setValue(PreferenceConstants.PCOVERAGE_CRITERION, ApetOptions.COVERAGE_CRITERION_PROLOG[index]);
	}

}

class SelectNumPathConstraintsChangeListener implements SelectionListener {

	OptionsDialog dialog = null;
	int index;
	Button b;
	Text from, to;

	public SelectNumPathConstraintsChangeListener(OptionsDialog dialog, int i, Button b, Text f, Text t) {
		this.dialog = dialog;
		this.index = i;
		this.b = b;
		this.from = f;
		this.to = t;
	}

	public void widgetDefaultSelected(SelectionEvent e) {
	}

	public void widgetSelected(SelectionEvent e) {
		for (int i = 0; i < this.dialog.getNumPathConstraints().length; i++) {
			this.dialog.getNumPathConstraints()[i].setSelection(false);
		}
		b.setSelection(true);
		
		IPreferenceStore store = Activator.getDefault().getPreferenceStore();
		store.setValue(PreferenceConstants.PNUMERIC, ApetOptions.NUM_PATH_CONSTRAINTS_PROLOG[index]);
		
		from.setEnabled(dialog.getNumPathConstraints()[0].getSelection());
		to.setEnabled(dialog.getNumPathConstraints()[0].getSelection());
	}

}

class SelectAliasingChangeListener implements SelectionListener {

	OptionsDialog dialog = null;
	int index;
	Button b;

	public SelectAliasingChangeListener(OptionsDialog dialog, int i, Button b) {
		this.dialog = dialog;
		this.index = i;
		this.b = b;
	}

	public void widgetDefaultSelected(SelectionEvent e) {
	}

	public void widgetSelected(SelectionEvent e) {
		
		IPreferenceStore store = Activator.getDefault().getPreferenceStore();
		store.setValue(PreferenceConstants.REFERENCES_ALIASING, b.getSelection());
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
		store.setValue(PreferenceConstants.PVERBOSITY, ApetOptions.VERBOSITY[index]);
	}

}


class SelectTracingChangeListener implements SelectionListener {

	OptionsDialog dialog = null;
	int index;
	Button b;

	public SelectTracingChangeListener(OptionsDialog dialog, int i, Button b) {
		this.dialog = dialog;
		this.index = i;
		this.b = b;
	}

	public void widgetDefaultSelected(SelectionEvent e) {
	}

	public void widgetSelected(SelectionEvent e) {
		for (int i = 0; i < this.dialog.getTracing().length; i++) {
			this.dialog.getTracing()[i].setSelection(false);
		}
		b.setSelection(true);
		
		IPreferenceStore store = Activator.getDefault().getPreferenceStore();
		store.setValue(PreferenceConstants.PTRACING, ApetOptions.TRACING_PROLOG[index]);
	}

}
*/