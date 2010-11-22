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
import java.awt.Component;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.Insets;
import java.awt.Toolkit;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.io.File;
import java.net.URL;
import java.util.LinkedList;
import java.util.List;

import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.BorderFactory;
import javax.swing.Icon;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JCheckBoxMenuItem;
import javax.swing.JComponent;
import javax.swing.JEditorPane;
import javax.swing.JFileChooser;
import javax.swing.JFrame;
import javax.swing.JMenuItem;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JSeparator;
import javax.swing.JTabbedPane;
import javax.swing.JToolBar;
import javax.swing.SwingConstants;
import javax.swing.SwingUtilities;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import javax.swing.event.HyperlinkEvent;
import javax.swing.event.HyperlinkListener;
import javax.swing.filechooser.FileFilter;

import net.sf.sdedit.Constants;
import net.sf.sdedit.config.Configuration;
import net.sf.sdedit.config.ConfigurationManager;
import net.sf.sdedit.config.GlobalConfiguration;
import net.sf.sdedit.diagram.Diagram;
import net.sf.sdedit.editor.Actions;
import net.sf.sdedit.error.DiagramError;
import net.sf.sdedit.icons.Icons;
import net.sf.sdedit.ui.PanelPaintDevice;
import net.sf.sdedit.ui.UserInterface;
import net.sf.sdedit.ui.UserInterfaceListener;
import net.sf.sdedit.ui.components.ATabbedPane;
import net.sf.sdedit.ui.components.ATabbedPaneListener;
import net.sf.sdedit.ui.components.AdvancedHelpPanel;
import net.sf.sdedit.ui.components.FullScreen;
import net.sf.sdedit.ui.components.FullScreenListener;
import net.sf.sdedit.ui.components.GrabbableViewport;
import net.sf.sdedit.ui.components.HelpPanel;
import net.sf.sdedit.ui.components.MenuBar;
import net.sf.sdedit.ui.components.OptionDialog;
import net.sf.sdedit.ui.components.ScalePanel;
import net.sf.sdedit.ui.components.buttons.Activator;
import net.sf.sdedit.ui.components.buttons.EnableComponents;
import net.sf.sdedit.ui.components.configuration.Bean;
import net.sf.sdedit.ui.components.configuration.ConfigurationAction;
import net.sf.sdedit.ui.components.configuration.ConfigurationDialog;
import net.sf.sdedit.ui.components.configuration.ConfigurationUI;
import net.sf.sdedit.util.OS;
import net.sf.sdedit.util.Predicate;
import net.sf.sdedit.util.UIUtilities;

@SuppressWarnings("serial")
public final class UserInterfaceImpl extends JFrame implements Constants,
		UserInterface, ChangeListener, FullScreenListener, HyperlinkListener,
		ATabbedPaneListener {

	private JFileChooser fileChooser;

	private ATabbedPane tabbedPane;

	private JPanel bottomPanel;

	private MenuBar menuBar;

	private List<UserInterfaceListener> listeners;

	private FullScreen fullScreen;

	private ScalePanel scalePanel;

	private RedrawThread redrawThread;

	private PrintDialog printDialog;

	private JToolBar toolbar;

	private JTabbedPane configurationPane;

	private ConfigurationDialog preferencesDialog;

	private EnableComponents enableComponents;

	private ConfigurationUI<GlobalConfiguration> globalConfigurationUI;

	private ConfigurationUI<Configuration> localConfigurationUI;

	static {
		String laf = ConfigurationManager.getGlobalConfiguration()
				.getLookAndFeel();
		LookAndFeelManager.instance().changeTo(laf);
		if (OS.TYPE != OS.Type.WINDOWS) {
			GrabbableViewport.setHandCursorIcon(Icons.getIcon("grabbing"));
		}
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see sd.edit.ui.UserInterface#showAboutDialog(java.net.URL)
	 */
	public void showAboutDialog(URL aboutURL) {
		new AboutDialog(this, aboutURL).setVisible(true);
	}

	public UserInterfaceImpl() {
		super();
		LookAndFeelManager.instance().setFont(
				ConfigurationManager.getGlobalConfiguration().getGuiFont());
		setDefaultCloseOperation(JFrame.DO_NOTHING_ON_CLOSE);
		enableComponents = new EnableComponents();

		preferencesDialog = new ConfigurationDialog(this);
		preferencesDialog.setTitle("Preferences");
		preferencesDialog.getContentPane().setLayout(new BorderLayout());
		preferencesDialog.setModal(true);
		preferencesDialog.setSize(new Dimension(675, 475));
		LookAndFeelManager.instance().register(preferencesDialog);

		configurationPane = new JTabbedPane();
		preferencesDialog.getContentPane().add(configurationPane,
				BorderLayout.CENTER);
		globalConfigurationUI = new ConfigurationUI<GlobalConfiguration>(
				preferencesDialog,
				ConfigurationManager.getGlobalConfigurationBean(),
				ConfigurationManager.GLOBAL_DEFAULT,
				null,
				"Restore defaults|Changes the current global preferences so that they are equal to the default preferences",
				"<html>In this tab you can change global preferences. On exit, they are stored in the"
						+ " file <tt>"
						+ Constants.GLOBAL_CONF_FILE.getAbsolutePath()
						+ "</tt>.");
		globalConfigurationUI.setBorder(BorderFactory.createEmptyBorder(15, 15,
				0, 15));
		preferencesDialog.addConfigurationUI(globalConfigurationUI);

		ConfigurationUI<Configuration> defaultCUI = new ConfigurationUI<Configuration>(
				preferencesDialog,
				ConfigurationManager.getDefaultConfigurationBean(),
				ConfigurationManager.LOCAL_DEFAULT,
				null,
				"Restore defaults|Changes the initial preferences (to be used for newly created diagrams) such that they are equal to the default settings",
				"<html>This tab is for adjusting the (initial) preferences that are used for"
						+ " newly created diagrams. They are stored along with the global preferences.");
		defaultCUI.setBorder(BorderFactory.createEmptyBorder(15, 15, 0, 15));
		preferencesDialog.addConfigurationUI(defaultCUI);

		localConfigurationUI = new ConfigurationUI<Configuration>(
				preferencesDialog,
				ConfigurationManager.createNewDefaultConfiguration(),
				ConfigurationManager.getDefaultConfigurationBean(),
				"Save as initial|Saves the current diagram's preferences as the initial preferences (to be used for all newly created diagrams)",
				"Restore initial|Changes the current diagram's preferences such that they are equal to the initial preferences",
				"<html>This tab is for changing the preferences for the diagram"
						+ " currently being displayed.<br>They will be stored "
						+ " when the diagram is saved as an <tt>.sdx</tt>-file.");
		localConfigurationUI.setBorder(BorderFactory.createEmptyBorder(15, 15,
				0, 15));

		for (JPanel panel : localConfigurationUI.getCategoryPanels()) {
			LookAndFeelManager.instance().registerOrphan(panel);
		}

		preferencesDialog.addConfigurationUI(localConfigurationUI);

		configurationPane.add("Global preferences", globalConfigurationUI);
		configurationPane.add("Initial diagram preferences", defaultCUI);
		configurationPane.addTab("Current diagram preferences",
				localConfigurationUI);

		for (JPanel panel : globalConfigurationUI.getCategoryPanels()) {
			LookAndFeelManager.instance().registerOrphan(panel);
		}
		for (JPanel panel : defaultCUI.getCategoryPanels()) {
			LookAndFeelManager.instance().registerOrphan(panel);
		}

		listeners = new LinkedList<UserInterfaceListener>();
		redrawThread = new RedrawThread(this);
		redrawThread.start();
		menuBar = new MenuBar();
		toolbar = new JToolBar();
		toolbar.setBorder(BorderFactory.createEmptyBorder(5, 5, 5, 5));
		toolbar.setFloatable(false);

	}

	public void addListener(UserInterfaceListener listener) {
		listeners.add(listener);
	}

	public void addCategory(String category, String icon) {
		ImageIcon imageIcon = icon == null || icon.equals("") ? null : Icons
				.getIcon(icon);
		menuBar.addMenu(category, imageIcon);
	}

	public void addAction(String category, Action action, Activator activator) {
		if (action != null) {
			String iconName = (String) action.getValue(Actions.ICON_NAME);
			if (iconName != null) {
				Icon icon = Icons.getIcon(iconName);
				action.putValue(Action.SMALL_ICON, icon);
			}
		}
		JMenuItem item = menuBar.addAction(category, action, -1);
		if (activator != null) {
			registerComponent(item, activator);
		}
	}

	public void addConfigurationAction(String category,
			ConfigurationAction<?> action, Activator activator) {
		JCheckBoxMenuItem item = MenuBar.makeMenuItem(action.getValue(
				Action.NAME).toString(), JCheckBoxMenuItem.class);
		menuBar.addItem(category, item);
		item.setToolTipText(action.getValue(Action.SHORT_DESCRIPTION)
				.toString());
		item.setIcon((Icon) action.getValue(Action.SMALL_ICON));

		if (activator != null) {
			registerComponent(item, activator);
		}
		item.addActionListener(action);
		enableComponents.registerConfigurationAction(action, item);
	}

	void registerComponent(JComponent comp, Activator activator) {
		enableComponents.registerButton(comp, activator);
	}

	void enableComponents() {
		enableComponents.enableComponents();
	}

	public void help(String title, String path, boolean navigation) {
		for (int i = 0; i < tabbedPane.getTabCount(); i++) {
			if (!(tabbedPane.getComponentAt(i) instanceof Tab)) {
				if (tabbedPane.getTitleAt(i).equals(title)
						|| tabbedPane.getTitleAt(i).startsWith(title + "-")) {
					tabbedPane.setSelectedIndex(i);
					return;
				}
			}
		}
		JComponent help;
		if (navigation) {
			help = new AdvancedHelpPanel(getClass().getResource(path), this);
		} else {
			help = new JScrollPane(new HelpPanel(getClass().getResource(path),
					this).getPane());
		}
		tabbedPane.addTab(help, title, Icons.getIcon("help"));
	}

	protected void fireCodeChanged(final boolean checkSyntaxOnly) {
		for (UserInterfaceListener listener : listeners) {
			listener.codeChanged(checkSyntaxOnly);
		}
	}

	protected void fireCurrentTabClosing() {
		for (UserInterfaceListener listener : listeners) {
			listener.currentTabClosing();
		}
	}

	public void fireHyperlinkClicked(String hyperlink) {
		for (UserInterfaceListener listener : listeners) {
			listener.hyperlinkClicked(hyperlink);
		}
	}

	public void currentTabClosing() {
		fireCurrentTabClosing();
	}

	private final Activator nonEmptyDiagramActivator = new Activator() {
		public boolean isEnabled() {
			return !isDiagramBlank();
		}
	};

	@SuppressWarnings("serial")
	public void showUI() {
		setIconImage(Icons.getIcon("icon").getImage());
		fileChooser = new JFileChooser();
		fileChooser.setCurrentDirectory(new File(System
				.getProperty("user.home")));
		Container pane = getContentPane();
		pane.setLayout(new BorderLayout());

		tabbedPane = new ATabbedPane();
		tabbedPane.addChangeListener(this);
		tabbedPane.addListener(this);

		pane.add(tabbedPane, BorderLayout.CENTER);

		pane.add(toolbar, BorderLayout.NORTH);

		bottomPanel = new JPanel();
		bottomPanel.setLayout(new BorderLayout());

		scalePanel = new ScalePanel(false);
		Dimension spSize = new Dimension(140, 24);

		scalePanel.setMaximumSize(spSize);
		scalePanel.setMinimumSize(spSize);
		scalePanel.setPreferredSize(spSize);

		scalePanel.setOpaque(false);
		addToolbarSeparator();
		toolbar.add(scalePanel);

		registerComponent(scalePanel, nonEmptyDiagramActivator);

		addToolbarSeparator();

		addToToolbar(scalePanel.normalSizeAction, nonEmptyDiagramActivator);
		addToToolbar(scalePanel.fitHeightAction, nonEmptyDiagramActivator);
		addToToolbar(scalePanel.fitWidthAction, nonEmptyDiagramActivator);
		addToToolbar(scalePanel.fitWindowAction, nonEmptyDiagramActivator);

		pane.add(bottomPanel, BorderLayout.SOUTH);

		int screenWidth = Toolkit.getDefaultToolkit().getScreenSize().width;
		int screenHeight = Toolkit.getDefaultToolkit().getScreenSize().height;

		int width = (int) (0.8 * screenWidth);
		int height = (int) (0.8 * screenHeight);

		int left = (int) (0.1 * screenWidth);
		int top = (int) (0.1 * screenHeight);

		setSize(width, height);
		setLocation(left, top);

		fullScreen = new FullScreen();
		fullScreen.addListener(this);
		LookAndFeelManager.instance().register(fullScreen);

		setJMenuBar(menuBar);
		LookAndFeelManager.instance().register(this);

		// printDialog.loadProfiles();
		ConfigurationManager.getGlobalConfigurationBean()
				.addPropertyChangeListener(new PropertyChangeListener() {
					public void propertyChange(PropertyChangeEvent evt) {
						enableComponents();
					}
				});

		enableComponents();
		setVisible(true);

	}

	/**
	 * This method is called when the state of the ATabbedPane changes, i. e.
	 * when the tab to be displayed changes.
	 * 
	 * @param e
	 */
	public void stateChanged(ChangeEvent e) {
		setTitle(tabbedPane.getCurrentTitle()
				+ " - Quick Sequence Diagram Editor");
		Action closeAction = menuBar.getActionByName("[F12]&Close");
		if (closeAction != null) {
			closeAction.setEnabled(tabbedPane.getTabCount() > 1);
		}
		final Tab tab = currentTab();
		if (tab != null) {
			tab.leaveFilterMode();
			scalePanel.setScalable(tab.getZoomPane());
			fireCodeChanged(false);
			SwingUtilities.invokeLater(new Runnable() {
				public void run() {
					tab.getTextArea().requestFocusInWindow();
				}
			});
		}
		enableComponents();
	}

	public void setTabTitle(String title) {
		tabbedPane.setTabTitle(title);
		setTitle(title + " - Quick Sequence Diagram Editor");
	}

	public String addTab(String tabTitle, Bean<Configuration> configuration) {
		Tab tab = new Tab(this, redrawThread, ConfigurationManager
				.getGlobalConfiguration().getEditorFont(), configuration);
		for (UserInterfaceListener listener : listeners) {
			if (listener.getPanelPaintDeviceListener() != null) {
				tab.addPanelPaintDeviceListener(listener
						.getPanelPaintDeviceListener());
			}
		}
		String uniqueTitle = tabbedPane.addTab(tab, tabTitle);
		currentTab().layout(
				configuration.getDataObject().isVerticallySplit() ? 1 : 0);
		enableComponents();
		return uniqueTitle;
	}

	public Tab currentTab() {
		Component comp = tabbedPane.getSelectedComponent();
		if (comp instanceof Tab) {
			return (Tab) comp;
		}
		return null;
	}

	public File getCurrentFile() {
		return currentTab().getFile();
	}

	public void setCurrentFile(File file) {
		currentTab().setFile(file);
		if (file != null) {
			tabbedPane.setToolTipTextAt(tabbedPane.getSelectedIndex(), file
					.getAbsolutePath());
		}
	}

	public boolean removeCurrentTab(boolean check) {
		setErrorStatus(false, "", -1, -1);
		boolean flag = tabbedPane.removeCurrentTab(check);
		enableComponents();
		return flag;
	}

	public void addToToolbar(Action action, Activator activator) {
		JButton button = toolbar.add(makeToolbarAction(action));
		button.setBorder(BorderFactory.createEmptyBorder(1, 1, 1, 5));
		button.setOpaque(false);
		button.setMargin(new Insets(1, 1, 1, 1));
		if (activator != null) {
			registerComponent(button, activator);
		}
	}

	public void addToolbarSeparator() {
		JSeparator sep = new JSeparator();
		sep.setOrientation(SwingConstants.VERTICAL);
		Dimension size = new Dimension(10, 25);
		sep.setMinimumSize(size);
		sep.setPreferredSize(size);
		sep.setMaximumSize(size);
		toolbar.add(sep);
	}

	public static Action makeToolbarAction(final Action action) {

		return new AbstractAction() {
			{
				putValue(Action.NAME, action.getValue(Action.NAME));
				putValue(Action.SHORT_DESCRIPTION, action
						.getValue(Action.SHORT_DESCRIPTION));
				String iconName = (String) action.getValue(Actions.ICON_NAME);
				if (iconName != null) {
					Icon icon = Icons.getIcon("large/" + iconName);
					putValue(Action.SMALL_ICON, icon);
				} else {
					putValue(Action.SMALL_ICON, action
							.getValue(Action.SMALL_ICON));
				}

			}

			public void actionPerformed(ActionEvent e) {
				action.actionPerformed(e);
			}
		};
	}

	public void configure(boolean local) {

		Bean<Configuration> conf = getConfiguration();
		if (conf != null) {
			localConfigurationUI.setBean(conf);
			localConfigurationUI.setEnabled(true);
		} else {
			localConfigurationUI.setEnabled(false);
		}

		configurationPane.setSelectedIndex(local ? 2 : 0);

		UIUtilities.centerWindow(preferencesDialog, this);
		preferencesDialog.setVisible(true);
	}

	public void redraw() {
		if (fullScreen.isVisible()) {
			SwingUtilities.invokeLater(new Runnable() {
				public void run() {
					fullScreen.getContentPane().repaint();
				}
			});
			SwingUtilities.invokeLater(new Runnable() {
				public void run() {
					fullScreen.getZoomPane().getScrollPane().revalidate();

				}
			});
			return;
		}
		Tab tab = currentTab();
		if (tab != null) {
			tab.redraw();
		}

	}

	public void fullScreen() {
		if (fullScreen == null) {
			errorMessage("Full-screen mode is not supported\nby your graphics environment.");
			return;
		}
		Diagram diag = getDiagram();
		if (diag != null) {
			PanelPaintDevice ppd = (PanelPaintDevice) diag.getPaintDevice();
			setVisible(false);
			fullScreen.display(ppd.getPanel());
			fullScreen.getZoomPane().fitSize();
		}
	}

	public void fullScreenModeLeft() {
		setVisible(true);
		Tab tab = currentTab();
		if (tab != null) {
			scalePanel.setScalable(tab.getZoomPane());
			// scalePanel.setScale(fullScreen.getZoomPane().getScale());
		}
		SwingUtilities.invokeLater(new Runnable() {
			public void run() {
				requestFocus();
			}
		});

	}

	public void clearDisplay() {
		Tab tab = currentTab();
		if (tab != null) {
			tab.clear();
		}
		enableComponents();
	}

	public void home() {
		Tab tab = currentTab();
		if (tab != null) {
			tab.home();
		}
	}

	public String getCode() {
		Tab tab = currentTab();
		if (tab != null) {
			return tab.getCode();
		}
		return "";
	}

	public void setCode(String text) {
		Tab tab = currentTab();
		if (tab != null) {
			tab.setCode(text);
			tab.home();
		}
		enableComponents();
		if (!ConfigurationManager.getGlobalConfiguration().isAutoUpdate()) {
			fireCodeChanged(false);
		}
	}

	public void setStatus(final String status) {
		Tab tab = currentTab();
		if (tab != null) {
			tab.setStatus(status);

		}
	}

	public void setQuitAction(final Action action) {
		addWindowListener(new WindowAdapter() {
			@Override
			public void windowClosing(WindowEvent e) {
				action.actionPerformed(new ActionEvent(this,
						ActionEvent.ACTION_PERFORMED, ""));
			}
		});
	}

	public int confirmOrCancel(String message) {
		int o = JOptionPane.showConfirmDialog(this, message, "Confirmation",
				JOptionPane.YES_NO_CANCEL_OPTION);
		switch (o) {
		case JOptionPane.YES_OPTION:
			return 1;
		case JOptionPane.NO_OPTION:
			return 0;
		default:
			return -1;
		}
	}

	public boolean confirm(String message) {
		return confirmOrCancel(message) == 1;
	}

	private static abstract class MyFileFilter extends FileFilter {
		String suffix;
	}

	public File[] getFiles(boolean open, boolean multiple, String message,
			String file, final File directory, String... filter) {
		if (directory == null) {
			fileChooser = new JFileChooser(fileChooser.getCurrentDirectory());
		} else {
			fileChooser = new JFileChooser(directory);
		}
		if (file != null) {
			fileChooser.setSelectedFile(new File(directory, file));

		}
		fileChooser.setMultiSelectionEnabled(multiple);

		if (filter.length > 0) {
			for (int i = 0; i < filter.length; i += 2) {
				final String description = filter[i];
				final String suffix = filter[i + 1].toLowerCase();
				MyFileFilter fileFilter = new MyFileFilter() {

					@Override
					public boolean accept(File f) {
						if (f.isDirectory()) {
							return true;
						}
						String name = f.getName().toLowerCase();
						return name.endsWith(suffix);
					}

					@Override
					public String getDescription() {
						return description;
					}
				};
				fileFilter.suffix = suffix;
				fileChooser.addChoosableFileFilter(fileFilter);
			}

			if (file != null) {
				int dot = file.lastIndexOf('.');
				if (dot >= 0) {
					String type = file.substring(dot + 1);
					for (FileFilter _filter : fileChooser
							.getChoosableFileFilters()) {
						if (_filter instanceof MyFileFilter) {
							if (((MyFileFilter) _filter).suffix.equals(type)) {
								fileChooser.setFileFilter(_filter);
								break;
							}

						}
					}
				}
			}
		}
		fileChooser.setDialogTitle(message);
		int ret;
		if (open) {
			ret = fileChooser.showOpenDialog(this);
		} else {
			ret = fileChooser.showSaveDialog(this);
		}
		if (ret == JFileChooser.APPROVE_OPTION) {
			if (multiple) {
				if (fileChooser.getSelectedFiles() == null
						|| fileChooser.getSelectedFiles().length == 0) {
					return null;
				}
				return fileChooser.getSelectedFiles();
			}
			File selectedFile = fileChooser.getSelectedFile();
			if (!open) {
				FileFilter selectedFilter = fileChooser.getFileFilter();
				if (selectedFilter instanceof MyFileFilter) {
					String type = ((MyFileFilter) selectedFilter).suffix;
					selectedFile = UIUtilities.affixType(selectedFile, type);
				}
			}
			return new File[] { selectedFile };

		}
		return null;
	}

	public void message(String msg) {
		JOptionPane.showMessageDialog(this, msg);
	}

	public void errorMessage(String msg) {
		JOptionPane.showMessageDialog(this, msg, "Error",
				JOptionPane.ERROR_MESSAGE);
	}

	public void moveCursorToPosition(int position) {
		currentTab().moveCursorToPosition(position);
	}

	public void setErrorStatus(final boolean warning, final String stat,
			final int begin, final int end) {
		SwingUtilities.invokeLater(new Runnable() {
			public void run() {
				Tab tab = currentTab();
				if (tab != null) {
					tab.setError(warning, stat, begin, end);
				}
			}
		});
	}

	public boolean isClean() {
		Tab tab = currentTab();
		if (tab != null) {
			return tab.isClean();
		}
		return true;
	}

	public void setClean() {
		Tab tab = currentTab();
		if (tab != null) {
			tab.setClean();
		}
	}

	public void undo() {
		Tab tab = currentTab();
		if (tab != null) {
			tab.undo();
		}
	}

	public void redo() {
		Tab tab = currentTab();
		if (tab != null) {
			tab.redo();
		}
	}

	public int getNumberOfTabs() {
		return tabbedPane.getTabCount();
	}

	public void appendText(final String tabTitle, final String text) {
		for (int i = 0; i < tabbedPane.getTabCount(); i++) {
			if (tabbedPane.getTitleAt(i).equals(tabTitle)) {
				if (tabbedPane.getComponentAt(i) instanceof Tab) {
					Tab tab = (Tab) tabbedPane.getComponentAt(i);
					tab.append(text);
				}
			}
		}
	}

	public String getString(String question, String initialValue) {
		return JOptionPane.showInputDialog(this, question, initialValue);
	}

	// TODO
	// change into addToggleAction
	public void addPredicateAction(String category, String name,
			String description, String tooltip, Icon icon,
			final Predicate predicate, boolean initialValue) {

		final JCheckBoxMenuItem checkBox = MenuBar.makeMenuItem(name,
				JCheckBoxMenuItem.class);
		checkBox.setIcon(icon);
		checkBox.setText(description);
		checkBox.setToolTipText(tooltip);
		checkBox.setSelected(initialValue);
		checkBox.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				predicate.set(checkBox.isSelected());
				fireCodeChanged(false);
			}

		});
		menuBar.addItem(category, checkBox);
	}

	/**
	 * @see javax.swing.event.HyperlinkListener#hyperlinkUpdate(javax.swing.event.HyperlinkEvent)
	 */
	public void hyperlinkUpdate(HyperlinkEvent event) {

		if (event.getEventType() == HyperlinkEvent.EventType.ACTIVATED) {
			if (event.getURL().toString().startsWith("http")) {
				return;
			}
			if (event.getURL().toString().endsWith("sdx")) {
				String file = event.getURL().toString();
				file = file.substring(file.lastIndexOf('/') + 1);
				fireHyperlinkClicked("example:" + file);
			} else if (event.getURL().toString().indexOf('#') > 0) {
				try {
					JEditorPane pane = (JEditorPane) event.getSource();
					pane.setPage(event.getURL());
				} catch (Exception e) {
					/* empty */
				}
			} else if (event.getURL().toString().endsWith("html")) {

				String file = event.getURL().toString();
				file = file.substring(file.lastIndexOf('/') + 1);
				fireHyperlinkClicked("help:" + file);
			}
		}
	}

	public void removeAction(String category, Action action) {
		menuBar.removeAction(category, action);
	}

	public boolean selectTabWith(File file) {
		for (int i = 0; i < tabbedPane.getTabCount(); i++) {
			if (tabbedPane.getComponentAt(i) instanceof Tab) {
				Tab tab = (Tab) tabbedPane.getComponentAt(i);
				if (tab.getFile() != null && tab.getFile().equals(file)) {
					tabbedPane.setSelectedIndex(i);
					return true;
				}
			}
		}
		return false;
	}

	public void nextTab() {
		int t = tabbedPane.getSelectedIndex();
		t = (t + 1) % tabbedPane.getTabCount();
		tabbedPane.setSelectedIndex(t);
	}

	public void previousTab() {
		int t = tabbedPane.getSelectedIndex();
		if (t == 0) {
			t = tabbedPane.getTabCount() - 1;
		} else {
			t--;
		}
		tabbedPane.setSelectedIndex(t);
	}

	public void showPrintDialog(String filetype) {
		if (printDialog == null) {
			printDialog = new PrintDialog(this);
		}
		printDialog.show(filetype);
	}

	public void exit() {
		// printDialog.saveProfiles();
	}

	public ScalePanel getScalePanel() {
		return scalePanel;
	}

	public void layout(int layout) {
		final Tab tab = currentTab();
		if (tab != null) {
			tab.layout(layout);
			SwingUtilities.invokeLater(new Runnable() {
				public void run() {
					tab.getTextArea().requestFocusInWindow();
				}
			});
		}
	}

	public void enterFilterMode() {
		Tab tab = currentTab();
		if (tab != null) {
			tab.enterFilterMode();
		}
	}

	public void leaveFilterMode() {
		Tab tab = currentTab();
		if (tab != null) {
			tab.leaveFilterMode();
		}
	}

	public void toggleFilterMode() {
		Tab tab = currentTab();
		if (tab != null) {
			tab.toggleFilterMode();
		}
	}

	public Bean<Configuration> getConfiguration() {
		Tab tab = currentTab();
		return tab == null ? null : tab.getConfiguration();
	}

	public boolean isDiagramBlank() {
		Tab tab = currentTab();
		if (tab == null) {
			return true;
		}
		Diagram diagram = tab.getDiagram();
		if (diagram != null) {
			return ((PanelPaintDevice) diagram.getPaintDevice()).isBlank();
		}
		return true;
	}

	public boolean isDiagramTabSelected() {
		return currentTab() != null;
	}

	public Diagram renderDiagram() {
		Tab tab = currentTab();
		if (tab == null) {
			return null;
		}
		tab.renderDiagram();
		return tab.getDiagram();
	}

	public Diagram getDiagram() {
		Tab tab = currentTab();
		return tab == null ? null : tab.getDiagram();
	}

	public DiagramError getDiagramError() {
		Tab tab = currentTab();
		return tab == null ? null : tab.getDiagramError();
	}

	public List<Tab> getTabs() {
		List<Tab> tabs = new LinkedList<Tab>();
		for (int i = 0; i < tabbedPane.getTabCount(); i++) {
			if (tabbedPane.getComponentAt(i) instanceof Tab) {
				tabs.add((Tab) tabbedPane.getComponentAt(i));
			}
		}
		return tabs;
	}

	public String getOption(String text, String... options) {
		OptionDialog optionDialog = new OptionDialog(this,
				"Please choose an option", Icons.getIcon("question"), text);
		for (String option : options) {
			optionDialog.addOption(option);
		}
		return optionDialog.getOption();
	}
}
