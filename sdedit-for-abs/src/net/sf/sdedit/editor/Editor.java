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

package net.sf.sdedit.editor;

import java.awt.Image;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.URI;
import java.util.Collections;
import java.util.LinkedList;
import java.util.List;

import javax.swing.Action;

import net.sf.sdedit.Constants;
import net.sf.sdedit.config.Configuration;
import net.sf.sdedit.config.ConfigurationManager;
import net.sf.sdedit.config.GlobalConfiguration;
import net.sf.sdedit.diagram.Diagram;
import net.sf.sdedit.drawable.Drawable;
import net.sf.sdedit.drawable.Note;
import net.sf.sdedit.editor.apple.AppInstaller;
import net.sf.sdedit.server.RealtimeServer;
import net.sf.sdedit.text.TextHandler;
import net.sf.sdedit.ui.ImagePaintDevice;
import net.sf.sdedit.ui.PanelPaintDeviceListener;
import net.sf.sdedit.ui.UserInterface;
import net.sf.sdedit.ui.UserInterfaceListener;
import net.sf.sdedit.ui.components.configuration.Bean;
import net.sf.sdedit.ui.components.configuration.ConfigurationAction;
import net.sf.sdedit.ui.impl.UserInterfaceImpl;
import net.sf.sdedit.util.OS;
import net.sf.sdedit.util.Pair;
import net.sf.sdedit.util.DocUtil.XMLException;

/**
 * The control class of the Quick Sequence Diagram Editor.
 * 
 * @author Markus Strauch
 */
public final class Editor implements Constants, PanelPaintDeviceListener,
		UserInterfaceListener

{
	private GlobalConfiguration globalConfiguration;

	private UserInterface ui;

	private Actions actions;

	private Engine engine;

	// Reference to the real-time-server, if one is running, otherwise null
	private RealtimeServer server;

	private LinkedList<String> recentFiles;

	private LinkedList<Action> recentFileActions;

	// Flag denoting if the application has already been set up.
	private boolean setup = false;

	private static Editor instance;

	public static Editor getEditor() {
		if (instance == null) {
			instance = new Editor();
		}
		return instance;
	}

	private Editor() {
		ui = newUI();
		if (OS.TYPE == OS.Type.MAC) {
			AppInstaller.installApplication(this);
		}
		recentFiles = new LinkedList<String>();
		recentFileActions = new LinkedList<Action>();
		globalConfiguration = ConfigurationManager.getGlobalConfiguration();

		ui.addListener(this);
		setupUI();
		readRecentFiles();
		engine = new Engine(this);
		if (globalConfiguration.isAutostartServer()) {
			try {
				startRealtimeServer(globalConfiguration.getRealtimeServerPort());
				ui.message("Started real-time diagram server @localhost:"
						+ server.getPort());
			} catch (Exception e) {
				ui
						.errorMessage("The real-time diagram server could not be started due to\n"
								+ "an exception of type "
								+ e.getClass().getSimpleName()
								+ "\n"
								+ "with the message: " + e.getMessage());
			}
		}
		setup = true;
		if (OS.TYPE == OS.Type.MAC) {
			File fileToLoad = AppInstaller.getFileToLoad();
			if (fileToLoad != null) {
				try {
					loadCode(fileToLoad);
				} catch (RuntimeException e) {
					throw e;
				} catch (Exception e) {
					ui.errorMessage("Cannot load "
							+ fileToLoad.getAbsolutePath() + "\n"
							+ "due to an exception of type "
							+ e.getClass().getSimpleName() + "\n"
							+ "with the message: " + e.getMessage());
				}
			}
		}
	}

	/**
	 * @see net.sf.sdedit.ui.UserInterfaceListener#currentTabClosing()
	 */
	public void currentTabClosing() {
		actions.closeDiagramAction.actionPerformed(null);
	}

	/**
	 * @see net.sf.sdedit.ui.UserInterfaceListener#hyperlinkClicked(java.lang.String)
	 */
	public void hyperlinkClicked(String hyperlink) {
		if (hyperlink.startsWith("example:")) {
			String file = hyperlink.substring(hyperlink.indexOf(':') + 1);
			actions.getExampleAction(file, file).actionPerformed(null);
		} else if (hyperlink.startsWith("help:")) {
			int first = hyperlink.indexOf(':');
			int last = hyperlink.lastIndexOf(':');
			String title = hyperlink.substring(first + 1, last);
			String file = hyperlink.substring(last + 1);
			ui.help(title, "/net/sf/sdedit/help/" + file, false);
		}
	}

	public void error(String msg) {
		ui.errorMessage(msg);
	}

	private void readRecentFiles() {
		String sep = System.getProperty("path.separator");
		String recent = globalConfiguration.getRecentFiles();
		if (recent != null && !recent.equals("")) {
			int i = 0;
			for (String file : recent.split(sep)) {
				if (new File(file).exists()) {
					i++;
					recentFiles.add(file);
					Action act = actions.getRecentFileAction(file);
					recentFileActions.add(act);
					ui.addAction("&File.Open &recent file", act, null);
					if (i == globalConfiguration.getMaxNumOfRecentFiles()) {
						return;
					}
				}
			}
		}
	}

	public List<String> getRecentFiles() {
		return Collections.checkedList(recentFiles, String.class);
	}

	private void addToRecentFiles(String file) {
		int max = globalConfiguration.getMaxNumOfRecentFiles();
		if (max == 0) {
			return;
		}
		int i = recentFiles.indexOf(file);
		Action act;
		if (i >= 0) {
			recentFiles.remove(i);
			act = recentFileActions.get(i);
			recentFileActions.remove(i);

		} else {
			act = actions.getRecentFileAction(file);
			ui.addAction("&File.Open &recent file", act, null);
			if (recentFiles.size() == max) {
				Action last = recentFileActions.removeLast();
				ui.removeAction("&File.Open &recent file", last);
				recentFiles.removeLast();
			}
		}
		recentFiles.addFirst(file);
		recentFileActions.addFirst(act);
	}

	private void writeRecentFiles() {
		String sep = System.getProperty("path.separator");
		StringBuffer buffer = new StringBuffer();
		for (String file : recentFiles) {
			if (buffer.length() > 0) {
				buffer.append(sep);
			}
			buffer.append(file);
		}
		globalConfiguration.setRecentFiles(buffer.toString());
	}

	public int startRealtimeServer(int port) throws IOException {
		if (isServerRunning()) {
			return 0;
		}
		server = new RealtimeServer(port, this);
		server.setDaemon(true);
		server.start();
		return server.getPort();
	}

	public boolean isServerRunning() {
		return server != null;
	}

	public void shutDownServer() {
		if (isServerRunning()) {
			server.shutDown();
			server = null;
		}
	}

	private void setupUI() {
		ui.setTitle("Quick Sequence Diagram Editor");
		addActions();
		ui.showUI();
		ui.addToolbarSeparator();
		ui.addToToolbar(actions.helpAction, null);
	}

	private void addActions() {

		actions = new Actions(this);

		ui.addAction("&File", actions.newDiagramAction, null);
		ui.addAction("&File", actions.loadCodeAction, null);
		ui.addCategory("&File.Open &recent file", "open");
		ui.addAction("&File", actions.saveCodeAction,
				actions.regularTabActivator);
		ui.addAction("&File", actions.saveCodeAsAction,
				actions.regularTabActivator);

		Action exportAction = actions.getExportAction();
		if (exportAction != null) {
			ui.addAction("&File", exportAction,
					actions.nonEmptyDiagramActivator);
		} else {
			ui.addAction("&File", actions.saveImageAction,
					actions.nonEmptyDiagramActivator);
		}

		ui.addAction("&File", actions.closeDiagramAction, null);
		ui.addAction("&File", actions.closeAllAction, null);

		Action printPDFAction = actions.getPrintAction("pdf");
		if (printPDFAction != null) {
			ui.addAction("&File", printPDFAction,
					actions.noDiagramErrorActivator);
		}
		ui.addAction("&File", actions.quitAction, null);

		ConfigurationAction<Configuration> wrapAction = new ConfigurationAction<Configuration>(
				"lineWrap", "[control shift W]&Wrap lines",
				"Wrap lines whose length exceed the width of the text area",
				"wrap") {
			@Override
			public Bean<Configuration> getBean() {
				return ui.getConfiguration();
			}
		};

		ConfigurationAction<Configuration> threadedAction = new ConfigurationAction<Configuration>(
				"threaded",
				Shortcuts.getShortcut(Shortcuts.ENABLE_THREADS)
						+ "Enable &multithreading",
				"Create diagrams with arbitrarily many sequences running concurrently",
				"threads") {
			@Override
			public Bean<Configuration> getBean() {
				return ui.getConfiguration();
			}
		};

		ConfigurationAction<GlobalConfiguration> autoUpdateAction = new ConfigurationAction<GlobalConfiguration>(
				"autoUpdate", "Auto-redraw", "Update diagram as you type",
				"reload") {
			@Override
			public Bean<GlobalConfiguration> getBean() {
				return ConfigurationManager.getGlobalConfigurationBean();
			}
		};

		ConfigurationAction<GlobalConfiguration> autoScrollAction = new ConfigurationAction<GlobalConfiguration>(
				"autoScroll",
				"Auto-scrolling",
				"Scroll automatically to where the message currently being specified is visible",
				"autoscroll") {
			@Override
			public Bean<GlobalConfiguration> getBean() {
				return ConfigurationManager.getGlobalConfigurationBean();
			}
		};

		ui.addAction("&Edit", actions.undoAction, actions.regularTabActivator);
		ui.addAction("&Edit", actions.redoAction, actions.regularTabActivator);
		ui.addAction("&Edit", actions.clearAction, actions.regularTabActivator);

		ui.addConfigurationAction("&Edit", threadedAction,
				actions.regularTabActivator);

		ui.addAction("&Edit", actions.configureGloballyAction, null);
		ui.addAction("&Edit", actions.configureDiagramAction,
				actions.regularTabActivator);

		ui.addCategory("&View", null);

		ui.addConfigurationAction("&View", autoUpdateAction, null);
		ui.addConfigurationAction("&View", autoScrollAction, null);

		ui
				.addAction("&View", actions.redrawAction,
						actions.regularTabActivator);

		ui.addAction("&View", actions.widenAction,
				actions.canConfigureActivator);
		ui.addAction("&View", actions.narrowAction, actions.canNarrowActivator);
		ui.addConfigurationAction("&View", wrapAction,
				actions.regularTabActivator);
		ui.addAction("&View", actions.fullScreenAction,
				actions.nonEmptyDiagramActivator);

		ui.addAction("&View", actions.splitLeftRightAction,
				actions.horizontalSplitPossibleActivator);
		ui.addAction("&View", actions.splitTopBottomAction,
				actions.verticalSplitPossibleActivator);

		if (OS.TYPE != OS.Type.MAC) {
			ui.setQuitAction(actions.quitAction);
		}

		ui.addToToolbar(actions.newDiagramAction, null);
		ui.addToToolbar(actions.loadCodeAction, null);
		ui.addToToolbar(actions.saveCodeAction, actions.regularTabActivator);
		ui.addToToolbar(actions.saveCodeAsAction, actions.regularTabActivator);

		if (exportAction != null) {
			ui.addToToolbar(exportAction, actions.nonEmptyDiagramActivator);
		} else {
			ui.addToToolbar(actions.saveImageAction,
					actions.nonEmptyDiagramActivator);
		}

		if (printPDFAction != null) {
			ui.addToToolbar(printPDFAction, actions.noDiagramErrorActivator);
		}

		ui.addToolbarSeparator();

		ui.addToToolbar(actions.configureGloballyAction, null);
		ui.addToToolbar(actions.configureDiagramAction,
				actions.regularTabActivator);
		ui.addToToolbar(actions.redrawAction, actions.regularTabActivator);

		ui.addToolbarSeparator();

		ui.addToToolbar(actions.fullScreenAction,
				actions.nonEmptyDiagramActivator);
		ui.addToToolbar(actions.splitLeftRightAction,
				actions.horizontalSplitPossibleActivator);
		ui.addToToolbar(actions.splitTopBottomAction,
				actions.verticalSplitPossibleActivator);

		ui.addAction("E&xtras", actions.serverAction, null);
		ui.addAction("E&xtras", actions.filterAction,
				actions.regularTabActivator);
		ui.addAction("E&xtras", new ExportMapAction(this),
				actions.nonEmptyDiagramActivator);

		ui.addAction("&Help", actions.helpAction, null);
		ui.addAction("&Help", actions.helpOnMultithreadingAction, null);
		ui.addAction("&Help", actions.asyncNotesAction, null);
		if (OS.TYPE != OS.Type.MAC) {
			ui.addAction("&Help", actions.showAboutDialogAction, null);
		}

		ui.addAction("&Help.&Examples", actions.getExampleAction(
				"Ticket order", "order.sdx"), null);
		ui.addAction("&Help.&Examples", actions.getExampleAction(
				"Breadth first search", "bfs.sdx"), null);
		ui.addAction("&Help.&Examples", actions.getExampleAction(
				"Levels and mnemonics", "levels.sdx"), null);
		ui.addAction("&Help.&Examples", actions.getExampleAction(
				"SSH 2 (by courtesy of Carlos Duarte)", "ssh.sdx"), null);
		ui.addAction("&Help.&Examples", actions.getExampleAction("Webserver",
				"webserver.sdx"), null);
	}

	void loadCode() throws IOException, XMLException {
		File[] files = ui.getFiles(true, true, "Open diagram file(s)", null,
				null, "Plain diagram source (.sd)", "sd",
				"Diagram source with preferences (.sdx)", "sdx");
		if (files != null) {
			for (File file : files) {
				loadCode(file);
			}
		}
	}

	public void loadCode(File file) throws IOException, XMLException {
		InputStream stream = new FileInputStream(file);
		try {
			loadCode(stream, file.getName());
			addToRecentFiles(file.getAbsolutePath());
			ui.setCurrentFile(file);
		} finally {
			stream.close();
		}
	}

	public boolean isSetup() {
		return setup;
	}

	/**
	 * Asks for a file and then tries to load code from that file. If it has
	 * been loaded successfully, the current file is set to that file.
	 * 
	 * @throws IOException
	 *             if the file cannot be read
	 */
	void loadCode(InputStream stream, String tabName) throws IOException,
			XMLException {
		String encoding = globalConfiguration.getFileEncoding();
		Pair<String, Bean<Configuration>> result = DiagramLoader.load(stream,
				encoding);
		ui.addTab(tabName, result.getSecond());
		ui.setCode(result.getFirst());
		// happens automatically via DocumentListener
		// engine.render(result.getSecond().getDataObject(), false, true);
		ui.home();
	}

	public void quit() {
		if (closeAll()) {
			ui.exit();
			writeRecentFiles();
			try {
				ConfigurationManager.storeConfigurations();
			} catch (IOException e) {
				ui.errorMessage("Could not save the global settings file:\n"
						+ GLOBAL_CONF_FILE.getAbsolutePath() + "\n"
						+ "due to an exception of type\n"
						+ e.getClass().getSimpleName() + "\n"
						+ "with the message: " + e.getMessage());
				e.printStackTrace();
			}
			if (server != null) {
				server.shutDown();
			}
			System.exit(0);
		}
	}

	/**
	 * Returns true if ALL tabs could be closed.
	 */
	boolean closeAll() {
		boolean confirmed = false;
		do {
			if (!confirmed && !ui.isClean()) {
				String choice = ui.getOption
				   ("<html>There are unsaved changes. " +
				    "Do you want<br>to save them?",
				    "Cancel",
				    "No",
				    "::::Yes#",
				    "No to all");
				if (choice == null || choice.equals("Cancel")) {
					return false;
				}
				if (choice.equals("Yes"))  {
					try {
						if (!saveCode(false)) {
							return false;
						}
					} catch (RuntimeException re) {
						throw re;
					} catch (Exception ex) {
						ui.message("The diagram could not be stored due to an exception\n" +
								"of type " + ex.getClass().getSimpleName() + " with the message: " +
								ex.getMessage());
					}
				}
				if (choice.equals("No to all")) {
					confirmed = true;
				}
			}
		} while (ui.removeCurrentTab(false));
		return true;
	}

	/**
	 * Saves the code from the text area to the current file or to a file whose
	 * name is given by the user. If the file has been successfully written, the
	 * current file is set to that file.
	 * 
	 * @param as
	 *            flag denoting whether the user should give the file, if not,
	 *            the current file (if present) is used
	 * @throws IOException
	 *             if the file cannot be written
	 * @return flag denoting if the code has actually been saved
	 */
	public boolean saveCode(boolean as) throws IOException, XMLException {
		String code = ui.getCode().trim();
		Bean<Configuration> configuration = ui.getConfiguration();
		boolean createNew = as || ui.getCurrentFile() == null;
		File file = null;
		if (createNew) {
			String currentFile;
			File current = ui.getCurrentFile();
			if (current != null) {
				currentFile = current.getName();
				current = current.getParentFile();
			} else {
				currentFile = "untitled.sdx";
			}
			File[] files = ui.getFiles(false, false, "Save diagram file",
					currentFile, current, "Plain diagram source (.sd)", "sd",
					"Diagram source with preferences (.sdx)", "sdx");
			if (files != null) {
				file = files[0];
			}
		} else {
			file = ui.getCurrentFile();
		}
		if (file == null) {
			return false;
		}
		if (file.exists()) {
			if (createNew) {
				String option = ui.getOption("Overwrite existing file?", 
						"Cancel", "No", "Yes#");
				if (!option.equals("Yes")) {
					return false;
				}
			}
			if (globalConfiguration.isBackupFiles()) {
				File backup = new File(file.getParent(), file.getName()
						+ ".bak");
				if (backup.exists()) {
					backup.delete();
				}
				file.renameTo(backup);
			}
		}
		// set configuration to null if the diagram is to be saved as
		// plain text
		configuration = file.getName().toLowerCase().endsWith("sdx") ? configuration
				: null;
		OutputStream stream = new FileOutputStream(file);
		String encoding = globalConfiguration.getFileEncoding();
		try {
			DiagramLoader.saveDiagram(code, configuration, stream, encoding);
			addToRecentFiles(file.getAbsolutePath());
			ui.setCurrentFile(file);
			ui.setClean();
			if (createNew) {
				ui.setTabTitle(file.getName());
			}
		} finally {
			stream.close();
		}
		return true;
	}

	private boolean firstImageSaved = false;

	/**
	 * Saves the current diagram as a PNG image file whose name is chosen by the
	 * user. Asks for confirmation, if a file would be overwritten.
	 * 
	 * @throws IOException
	 *             if the image file cannot be written due to an i/o error
	 */
	void saveImage() throws IOException {
		String code = getUI().getCode().trim();
		if (code.equals("")) {
			return;
		}
		ImagePaintDevice ipd = new ImagePaintDevice();
		TextHandler handler = new TextHandler(code);
		Configuration conf = getUI().getConfiguration().getDataObject();
		try {
			new Diagram(conf, handler, ipd).generate();
		} catch (Exception ex) {
			ui.errorMessage("The diagram source text has errors.");
			return;
		}
		Image image = ipd.getImage();
		if (image != null) {
			File current = null;
			if (!firstImageSaved) {
				current = ui.getCurrentFile();
				if (current != null) {
					current = current.getParentFile();
				}
				firstImageSaved = true;
			}
			String currentFile = null;
			if (ui.getCurrentFile() != null) {
				currentFile = ui.getCurrentFile().getName();
				int dot = currentFile.lastIndexOf('.');
				if (dot >= 0) {
					currentFile = currentFile.substring(0, dot + 1) + "png";
				}

			}
			File[] files = ui.getFiles(false, false, "save as PNG",
					currentFile, current, "PNG image", "png");
			File imageFile = files != null ? files[0] : null;
			if (imageFile != null
					&& (!imageFile.exists() || 1 == ui
							.confirmOrCancel("Overwrite existing file "
									+ imageFile.getName() + "?"))) {
				ipd.saveImage(imageFile);
				ui.message("Exported image as\n" + imageFile.getAbsolutePath());
			}
		}
	}

	/**
	 * Starts a new thread that generates a diagram from the source text found
	 * in the text area of the currently selected tab.
	 * 
	 * @param syntaxCheckOnly
	 *            flag denoting if only syntax is checked and no diagram is
	 *            generated yet
	 */
	public void codeChanged(boolean syntaxCheckOnly) {
		ui.setErrorStatus(false, "", -1, -1);
		String code = ui.getCode();
		if (code != null && !code.trim().equals("")) {
			engine.render(ui.getConfiguration().getDataObject(),
					syntaxCheckOnly, false);
		} else {
			ui.clearDisplay();
		}
	}

	/**
	 * Returns the user interface.
	 * 
	 * @return the user interface
	 */
	public UserInterface getUI() {
		return ui;
	}

	private UserInterface newUI() {
		return new UserInterfaceImpl();
	}

	// TODO move this to the GUI side (use diagram reference of paint device)

	/**
	 * Moves the cursor to the position in the text area where the object or
	 * message corresponding to the drawable instance is declared.
	 * 
	 * @param drawable
	 *            a drawable instance to show the corresponding declaration for
	 */
	public void mouseClickedDrawable(Drawable drawable) {
		Diagram diag = ui.getDiagram();
		if (diag != null) {
			Integer pos = (Integer) diag.getStateForDrawable(drawable);
			if (pos != null) {
				ui.moveCursorToPosition(pos - 1);
			}
		}
		if (drawable instanceof Note) {
			Note note = (Note) drawable;
			URI link = note.getLink();
			if (link != null) {
				File current = ui.getCurrentFile();
				File linked;
				if (current != null) {
					linked = new File(current.toURI().resolve(link));
				} else {
					linked = new File(link);
				}
				if (!ui.selectTabWith(linked)) {
					try {
						loadCode(linked);
					} catch (RuntimeException e) {
						throw e;
					} catch (Exception e) {
						ui
								.errorMessage(linked.getAbsolutePath()
										+ "\n"
										+ "could not be loaded due to an exception of\n"
										+ "type "
										+ e.getClass().getSimpleName()
										+ " with the message\n"
										+ e.getMessage());
					}
				}

			}
		}

	}

	/**
	 * Returns true if and only if the given drawable instance is associated by
	 * the most recently used diagram with a DiagramDataProvider state, id est a
	 * position in the text area.
	 * 
	 * @param drawable
	 *            a drawable instance such that the mouse has just entered it
	 * 
	 * @return true if and only if the given drawable instance is associated by
	 *         the most recently used diagram with a position in the text area
	 */
	public boolean mouseEnteredDrawable(Drawable drawable) {
		Diagram diag = ui.getDiagram();
		if (diag != null && diag.getStateForDrawable(drawable) != null) {
			return true;
		}
		return false;
	}

	/**
	 * @see net.sf.sdedit.ui.PanelPaintDeviceListener#mouseExitedDrawable(net.sf.sdedit.drawable.Drawable)
	 */
	public void mouseExitedDrawable(Drawable drawable) {
		/* empty */
	}

	public PanelPaintDeviceListener getPanelPaintDeviceListener() {
		return this;
	}
}
