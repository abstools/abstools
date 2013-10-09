package costabs.dialogs;


import java.util.ArrayList;

import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.preference.IPreferenceNode;
import org.eclipse.jface.preference.IPreferencePage;
import org.eclipse.jface.preference.PreferenceDialog;
import org.eclipse.jface.preference.PreferenceManager;
import org.eclipse.jface.preference.PreferenceNode;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.layout.RowLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;

import costabs.beans.Analyses;
import costabs.beans.Analysis;
import costabs.console.ConsoleHandler;
import costabs.exceptions.CostabsException;
import costabs.messages.Messages;
import costabs.preferences.CostabsPreferenceDialog;
import costabs.preferences.CostabsPreferences;
import costabs.preferences.PreferencesManager;

public class OptionsDialog extends Dialog {


	public OptionsDialog(Shell parentShell) {
		super(parentShell);
	}

	ArrayList<Button> analysisButtons;

	String selected;

	private boolean editPreferences;

	@Override
	protected Control createDialogArea(Composite parent) {
		try {
			Composite composite = new Composite(parent, SWT.NONE);

			analysisButtons = new ArrayList<Button>();

			Analyses analyses = PreferencesManager.getInstance().getAnalyses();

			RowLayout verticalLayout = new RowLayout();
			verticalLayout.type = SWT.VERTICAL;
			composite.setLayout(verticalLayout);

			Label label = new Label (composite, SWT.NULL);
			label.setText(Messages.OptionsDialog2_available);

			Composite optionsPanel = new Composite(composite,SWT.NONE);
			optionsPanel.setLayout(new RowLayout(SWT.VERTICAL));

			for(Analysis analysis: analyses.getAnalyses()) {
				Button option = new Button(optionsPanel, SWT.RADIO);
				option.setText(analysis.getDescription().getShortdesc());
				option.setToolTipText(analysis.getDescription().getLongdesc());
				option.setData(analysis.getAnalysisId());
				analysisButtons.add(option);
			}

			return composite;
		}
		catch (CostabsException e) {
			DialogPrinter.printError(getParentShell(), e, "Cannot generate the options dialog");
			return null;
		}
		
	}

	@Override
	protected void configureShell(Shell newShell) {
		super.configureShell(newShell);
		newShell.setText(Messages.OptionsDialog2_2);
	}

	@Override
	protected void cancelPressed() {
		this.close();
		setReturnCode(CANCEL);
	}

	@Override
	protected void okPressed() {
		saveSelected();

		if (this.editPreferences) {
			IPreferencePage page = new CostabsPreferences(selected);
			PreferenceManager mgr = new PreferenceManager();
			IPreferenceNode node = new PreferenceNode("1", page);
			mgr.addToRoot(node);
			CostabsPreferenceDialog dialog = new CostabsPreferenceDialog(getShell(), mgr);
			dialog.create();
			dialog.setMessage(Messages.OptionsDialog2_4);
			dialog.open();

			if (dialog.getReturnCode() == PreferenceDialog.CANCEL) {
				this.editPreferences = false;
				return;
			}
		}

		this.close();
		setReturnCode(OK);
	}

	protected void createButtonsForButtonBar(Composite parent) {
		GridLayout layout = new GridLayout (1,true);
		parent.setLayout(layout);
		parent.pack();

		Button ok = new Button(parent, SWT.PUSH);
		ok.setText(Messages.OptionsDialog2_5);
		GridData data = new GridData(GridData.CENTER);
		ok.setLayoutData(data);
		ok.addSelectionListener(new SelectionAdapter() {
			public void widgetSelected(SelectionEvent event) {
				editPreferences = true;
				okPressed();
			}
		});
		super.createButtonsForButtonBar(parent);
		this.getButton(IDialogConstants.OK_ID).setText(Messages.OptionsDialog2_6);

	}


	private void saveSelected () {
		for(Button b: analysisButtons) {
			if (b.getSelection()) {
				selected = (String)b.getData(); 
			}
		}
	}

	public String getAnalysisSelected () {
		return selected;
	}

}
