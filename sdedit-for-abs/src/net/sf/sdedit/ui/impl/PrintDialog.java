// Copyright (c) 2006 - 2008, Markus Strauch.
// All rights reserved.
// 
// Redistribution and use in source and binary forms, with or without
// modification, are permitted provided that the following conditions are met:
// 
// * Redistributions of source code must retain the above copyright notice, 
// this list of conditions and the following disclaimer.
// * Redistributions in binary form must reproduce the above copyright notice, 
// this list of conditions and the following disclaimer in the documentation 
// and/or other materials provided with the distribution.
// 
// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" 
// AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE 
// IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE 
// ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE 
// LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR 
// CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF 
// SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS 
// INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN 
// CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) 
// ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF 
// THE POSSIBILITY OF SUCH DAMAGE.

package net.sf.sdedit.ui.impl;

import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.GridLayout;
import java.awt.event.ActionEvent;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.io.BufferedOutputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStream;

import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.SwingConstants;
import javax.swing.SwingUtilities;
import javax.swing.border.TitledBorder;

import net.sf.sdedit.config.Configuration;
import net.sf.sdedit.config.ConfigurationManager;
import net.sf.sdedit.config.PrintConfiguration;
import net.sf.sdedit.error.SemanticError;
import net.sf.sdedit.error.SyntaxError;
import net.sf.sdedit.multipage.MultipageExporter;
import net.sf.sdedit.ui.components.ButtonPanel;
import net.sf.sdedit.ui.components.configuration.Bean;
import net.sf.sdedit.ui.components.configuration.ConfigurationDialog;
import net.sf.sdedit.ui.components.configuration.ConfigurationUI;
import net.sf.sdedit.util.UIUtilities;

public class PrintDialog extends ConfigurationDialog implements
		PropertyChangeListener
{
	private String fileType;

	private UserInterfaceImpl ui;

	private JScrollPane preview;

	private MultipageExporter exporter;

	private Bean<PrintConfiguration> printerProperties;
	
	private Bean<PrintConfiguration> copy;
	
	private JLabel scaleLabel;

	public PrintDialog(UserInterfaceImpl ui) {
		super(ui);
		this.ui = ui;
		setModal(true);
		init();
	}

	private void init() {
		JPanel center = new JPanel();
		getContentPane().add(center, BorderLayout.CENTER);
		ButtonPanel buttonPanel = new ButtonPanel();
		buttonPanel.addAction(cancel);
		buttonPanel.addAction(ok, 0, true);
		getContentPane().add(buttonPanel, BorderLayout.SOUTH);
		printerProperties = ConfigurationManager.getPrintConfigurationBean();
		setToCurrentFile();
		ConfigurationUI<PrintConfiguration> cui = new ConfigurationUI<PrintConfiguration>(
				this, printerProperties, null, null, null,
				null);
		cui.hideButtons();
		cui.hideCategoryList();
		printerProperties.addPropertyChangeListener(this);

		center.setLayout(new GridLayout(1, 2));
		center.add(cui);
		JPanel right = new JPanel();
		right.setBorder(new TitledBorder("Preview"));
		right.setLayout(new BorderLayout());
		preview = new JScrollPane();
		right.add(preview);
		scaleLabel = new JLabel("Zoom factor: 100 %");
		scaleLabel.setHorizontalAlignment(SwingConstants.CENTER);
		right.add(scaleLabel, BorderLayout.SOUTH);
		center.add(right);
	}

	public void show(String fileType) {
		this.fileType = fileType.toLowerCase();
		setTitle("Print or export multi-page " + fileType.toUpperCase()
				+ " document");
		setSize(new Dimension(740, 540));
		UIUtilities.centerWindow(this, ui);
		reinitialize();
		copy = printerProperties.copy();
		setVisible(true);
	}
	
	private void setToCurrentFile () {
		File file = ui.getCurrentFile();
		if (file != null) {
			String name = file.getAbsolutePath();
			int dot = name.lastIndexOf('.');
			if (dot >= 0) {
				name = name.substring(0,dot);
			}
			printerProperties.getDataObject().setExportFile(new File(name + ".pdf"));
		}
	}

	private Action cancel = new AbstractAction() {
		{
			putValue(Action.NAME, "Cancel");
		}

		public void actionPerformed(ActionEvent e) {
			printerProperties.takeValuesFrom(copy);
			setVisible(false);
		}
	};

	private Action ok = new AbstractAction() {

		{
			putValue(Action.NAME, "OK");
		}

		public void actionPerformed(ActionEvent e) {
			String command = printerProperties.getDataObject().getAction();
			if (command.equals(PrintConfiguration.EXPORT)) {
				export();
			} else if (command.equals(PrintConfiguration.EXPORT_AND_PRINT)) {
				exportAndPrint();
			} else {
				pipe();
			}
			setVisible(false);
		}
	};

	private File export() {
		File exportFile = printerProperties.getDataObject().getExportFile();
		OutputStream stream = null;
		try {
			stream = new FileOutputStream(exportFile);
			exporter.exportTo(new FileOutputStream(exportFile), fileType);
			return exportFile;
		} catch (IOException e) {
			ui.errorMessage("Cannot export to file "
					+ exportFile.getAbsolutePath()
					+ "\ndue to an exception of type "
					+ e.getClass().getSimpleName() + "\nwith the message: "
					+ e.getMessage());
			return null;
		} finally {
			if (stream != null) {
				try {
					stream.close();
				} catch (IOException ignored) {
					/* ignored */
				}
			}
		}

	}

	private void exportAndPrint() {
		File exportFile = export ();
		if (exportFile != null) {
			String command = printerProperties.getDataObject().getCommand() + " " + exportFile.getAbsolutePath();
			try {
				Process proc = Runtime.getRuntime().exec(command);
				proc.waitFor();
				if (printerProperties.getDataObject().isEraseExportFile()) {
					exportFile.delete();
				}
			} catch (IOException e) {
				ui.errorMessage("Invocation of\n" + command +
						"\nfailed due to an exception of type " + e.getClass().getSimpleName() + "\n" +
						"with the message: " + e.getMessage());
			} catch (InterruptedException e) {
				Thread.currentThread().interrupt();
			}
		}
	}

	private void pipe() {
		OutputStream stream = null;
		try {
			Process process = Runtime.getRuntime().exec(printerProperties.getDataObject().getCommand());
			stream = new BufferedOutputStream(process.getOutputStream());
			exporter.exportTo(stream, fileType);
		} catch (IOException e) {
			ui.errorMessage("Piping to printer command failed due to an exception of type\n" +
					e.getClass().getSimpleName() + " with the message: " + e.getMessage());
			
		} finally {
			if (stream != null) {
				try {
					stream.close();
				} catch (IOException e) {
					/* ignored */
				}
			}
		}
	}

	private void reinitialize() {
		String source = ui.getCode();
		Configuration configuration = ui.getConfiguration().getDataObject();
		exporter = new MultipageExporter(printerProperties.getDataObject(),
				source, configuration);
		try {
			exporter.init();
		} catch (RuntimeException re) {
			throw re;
		} catch (SemanticError se) {
			/* ignored */
		} catch (SyntaxError se) {
			/* ignored */
		}
		int scale = (int) (100 * exporter.getScale());
		scaleLabel.setText("Zoom factor: " + scale + " %");

		preview.setViewportView(exporter);
	}

	public void propertyChange(PropertyChangeEvent evt) {
		String property = evt.getPropertyName();
		if (!property.equals("command") && !property.equals("exportFile")
				&& !property.equals("commandFile")) {
			SwingUtilities.invokeLater(new Runnable() {
				public void run () {
					reinitialize();
				}
			});
		}
		if (property.equals("command")) {
			PrintConfiguration prop = printerProperties.getDataObject();
			String command = prop.getAction();
			if (command.equals(PrintConfiguration.EXPORT_AND_PRINT)) {
				prop.setExportFile(new File(System.getProperty("java.io.tmpdir"), "temp.pdf"));
			} else if (command.equals(PrintConfiguration.EXPORT)){
				setToCurrentFile();
			}
		}
	}
}
