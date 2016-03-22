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

import java.awt.event.ActionEvent;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.net.URL;

import javax.swing.AbstractAction;
import javax.swing.Action;

import net.sf.sdedit.Constants;
import net.sf.sdedit.config.Configuration;
import net.sf.sdedit.config.ConfigurationManager;
import net.sf.sdedit.multipage.MultipageExporter;
import net.sf.sdedit.server.Exporter;
import net.sf.sdedit.ui.components.buttons.Activator;
import net.sf.sdedit.ui.components.configuration.Bean;
import net.sf.sdedit.util.OS;

import static net.sf.sdedit.editor.Shortcuts.*;

@SuppressWarnings("serial")
public final class Actions implements Constants {

	public static final String ICON_NAME = "icon-name";

	private Editor editor;

	Actions(Editor editor) {
		this.editor = editor;
	}

	public final Activator canConfigureActivator = new Activator() {
		public boolean isEnabled() {
			return editor.getUI().getConfiguration() != null;
		}
	};

	public final Activator nonEmptyDiagramActivator = new Activator() {
		public boolean isEnabled() {
			return !editor.getUI().isDiagramBlank();
		}
	};
	
	public final Activator noDiagramErrorActivator = new Activator() {
		public boolean isEnabled() {
			return !editor.getUI().isDiagramBlank() &&
			   editor.getUI().getDiagramError() == null;
		}
	}; 

	public final Activator canNarrowActivator = new Activator() {
		public boolean isEnabled() {
			Bean<Configuration> conf = editor.getUI().getConfiguration();
			if (conf == null) {
				return false;
			}
			return conf.getDataObject().getGlue() > 0;
		}
	};

	public final Activator regularTabActivator = new Activator() {
		public boolean isEnabled() {
			return editor.getUI().isDiagramTabSelected();
		}
	};

	public final Activator verticalSplitPossibleActivator = new Activator() {
		public boolean isEnabled() {
			Bean<Configuration> conf = editor.getUI().getConfiguration();
			return conf != null && !conf.getDataObject().isVerticallySplit();

		}
	};

	public final Activator horizontalSplitPossibleActivator = new Activator() {
		public boolean isEnabled() {
			Bean<Configuration> conf = editor.getUI().getConfiguration();
			return conf != null && conf.getDataObject().isVerticallySplit();
		}
	};

	final Action clearAction = new AbstractAction() {
		{
			putValue(ICON_NAME, "eraser");
			putValue(Action.SHORT_DESCRIPTION, "Erase the source code");
			putValue(Action.NAME, getShortcut(CLEAR) + "&Erase code");
		}

		public void actionPerformed(ActionEvent e) {
			editor.getUI().setCode("");
			editor.codeChanged(false);
		}
	};

	final Action configureGloballyAction = new AbstractAction() {
		{
			putValue(ICON_NAME, "globalsettings");
			putValue(Action.SHORT_DESCRIPTION, "Edit global preferences");
			putValue(Action.NAME, getShortcut(GLOBAL_CONFIGURATION)
					+ "&Global preferences...");
		}

		public void actionPerformed(ActionEvent e) {
			editor.getUI().configure(false);
		}
	};

	final Action configureDiagramAction = new AbstractAction() {
		{
			putValue(ICON_NAME, "configure");
			putValue(Action.SHORT_DESCRIPTION, "Edit diagram preferences");
			putValue(Action.NAME, getShortcut(DIAGRAM_CONFIGURATION)
					+ "&Diagram preferences...");
		}

		public void actionPerformed(ActionEvent e) {
			editor.getUI().configure(true);
		}
	};

	final Action helpAction = new AbstractAction() {
		{
			putValue(ICON_NAME, "help");
			putValue(Action.SHORT_DESCRIPTION,
					"Display a comprehensive help page");
			putValue(Action.NAME, getShortcut(HELP) + "&Help");
		}

		public void actionPerformed(ActionEvent e) {
			editor.getUI().help("Help", "/net/sf/sdedit/help/help.html", true);
		}
	};

	final Action helpOnMultithreadingAction = new AbstractAction() {
		{
			putValue(ICON_NAME, "help");
			putValue(Action.SHORT_DESCRIPTION,
					"Show a help page dedicated to multithreading");
			putValue(Action.NAME, "&Multithreading help");
		}

		public void actionPerformed(ActionEvent e) {
			editor.getUI().help("Multithreading help",
					"/net/sf/sdedit/help/multithreading_help.html", false);
		}
	};

	final Action asyncNotesAction = new AbstractAction() {
		{
			putValue(ICON_NAME, "help");
			putValue(Action.SHORT_DESCRIPTION,
					"Show a help page containing notes on asynchronous messages");
			putValue(Action.NAME, "&Notes on asynchronous messages");
		}

		public void actionPerformed(ActionEvent e) {
			editor.getUI().help("Notes on asynchronous messages",
					"/net/sf/sdedit/help/async.html", false);
		}
	};

	final Action loadCodeAction = new AbstractAction() {
		{
			putValue(ICON_NAME, "open");
			putValue(Action.NAME, getShortcut(OPEN) + "&Open...");
			putValue(Action.SHORT_DESCRIPTION, "Load diagram");
		}

		public void actionPerformed(ActionEvent e) {
			try {
				editor.loadCode();
			} catch (Exception ex) {
				ex.printStackTrace();
				editor.error("Could not open the file due to an\n"
						+ "exception of type: " + ex.getClass().getSimpleName()
						+ "\n" + "with the message: " + ex.getMessage());
			}
		}
	};

	final Action newDiagramAction = new AbstractAction() {
		{
			putValue(ICON_NAME, "new");
			putValue(Action.NAME, getShortcut(NEW) + "&New diagram");
			putValue(Action.SHORT_DESCRIPTION, "Add a tab for a new diagram");
		}

		public void actionPerformed(ActionEvent e) {
			Bean<Configuration> conf = ConfigurationManager
					.createNewDefaultConfiguration();
			editor.getUI().addTab("untitled", conf);
		}

	};

	final Action closeAllAction = new AbstractAction() {
		{
			putValue(ICON_NAME, "close");
			putValue(Action.NAME, getShortcut(CLOSE_ALL) + "Close All");
			putValue(Action.SHORT_DESCRIPTION, "Close all tabs");
		}

		public void actionPerformed(ActionEvent e) {
			if (editor.closeAll()) {
				editor.getUI().addTab("untitled",
						ConfigurationManager.createNewDefaultConfiguration());
			}
		}

	};

	public final Action closeDiagramAction = new AbstractAction() {
		{
			putValue(ICON_NAME, "close");
			putValue(Action.NAME, getShortcut(CLOSE) + "&Close");
			putValue(Action.SHORT_DESCRIPTION, "Close the current tab");
		}

		public void actionPerformed(ActionEvent e) {
			if (editor.getUI().getNumberOfTabs() > 1) {
				if (editor.getUI().isClean()) {
					editor.getUI().removeCurrentTab(false);
				} else {
					String option = editor.getUI().getOption(
							"<html>There are unsaved changes.<br>"
									+ "Do you want to save them?", "Cancel",
							"No", "Yes#");
					if (option.equals("Yes")) {
						try {
							if (editor.saveCode(false)) {
								editor.getUI().removeCurrentTab(true);
							}
						} catch (RuntimeException re) {
							throw re;
						}

						catch (Exception ioe) {
							editor.error(ioe.getMessage());
						}
					} else if (option.equals("No")) {
						editor.getUI().removeCurrentTab(true);
					}
				}
			}
		}
	};

	final Action quitAction = new AbstractAction() {
		{
			putValue(ICON_NAME, "exit");
			putValue(Action.SHORT_DESCRIPTION, "Quit the application");
			putValue(Action.NAME, getShortcut(QUIT) + "&Quit");
		}

		public void actionPerformed(ActionEvent e) {
			editor.quit();
		}
	};

	final Action saveCodeAction = new AbstractAction() {
		{
			putValue(ICON_NAME, "save");
			putValue(NAME, getShortcut(SAVE) + "&Save");
			putValue(SHORT_DESCRIPTION, "Save the diagram source code");
		}

		public void actionPerformed(ActionEvent e) {
			try {
				editor.saveCode(false);
			} catch (RuntimeException re) {
				throw re;
			} catch (Exception ioe) {
				editor.error(ioe.getMessage());
			}
		}
	};

	final Action saveCodeAsAction = new AbstractAction() {
		{
			putValue(ICON_NAME, "saveas");
			putValue(Action.NAME, getShortcut(SAVE_AS) + "S&ave as...");
			putValue(Action.SHORT_DESCRIPTION,
					"Save the diagram source code as a new file");
		}

		public void actionPerformed(ActionEvent e) {
			try {
				editor.saveCode(true);
			} catch (RuntimeException re) {
				throw re;
			}

			catch (Exception ex) {
				editor.error("Could not open the file due to an\n"
						+ "exception of type: " + ex.getClass().getSimpleName()
						+ "\n" + "with the message: " + ex.getMessage());
			}
		}
	};

	final Action saveImageAction = new AbstractAction() {
		{
			putValue(ICON_NAME, "image");
			putValue(Action.NAME, getShortcut(EXPORT_IMAGE)
					+ "&Export as PNG...");
			putValue(Action.SHORT_DESCRIPTION,
					"Export the diagram as a PNG image");

		}

		public void actionPerformed(ActionEvent e) {
			try {
				editor.saveImage();
			} catch (IOException ioe) {
				editor.error(ioe.getMessage());
			}
		}
	};

	final Action redrawAction = new AbstractAction() {
		{
			putValue(ICON_NAME, "reload");
			putValue(Action.NAME, getShortcut(REDRAW) + "Re&draw");
			putValue(Action.SHORT_DESCRIPTION, "Redraw the diagram");
		}

		public void actionPerformed(ActionEvent e) {
			editor.codeChanged(false);
		}
	};

	final Action widenAction = new AbstractAction() {
		{
			putValue(ICON_NAME, "widen");
			putValue(Action.SHORT_DESCRIPTION, "Widen the diagram");
			putValue(Action.NAME, getShortcut(WIDEN) + "&Widen");
		}

		public void actionPerformed(ActionEvent e) {
			Configuration conf = editor.getUI().getConfiguration()
					.getDataObject();
			if (conf != null) {
				conf.setGlue(conf.getGlue()
						+ ConfigurationManager.getGlobalConfiguration()
								.getGlueChangeAmount());
				editor.codeChanged(false);
			}
		}
	};

	final Action narrowAction = new AbstractAction() {
		{
			putValue(ICON_NAME, "narrow");
			putValue(Action.SHORT_DESCRIPTION, "Narrow the diagram");
			putValue(Action.NAME, getShortcut(NARROW) + "&Narrow");
		}

		public void actionPerformed(ActionEvent e) {
			Configuration conf = editor.getUI().getConfiguration()
					.getDataObject();
			if (conf != null) {
				int glue = Math.max(0, conf.getGlue()
						- ConfigurationManager.getGlobalConfiguration()
								.getGlueChangeAmount());
				conf.setGlue(glue);
				editor.codeChanged(false);
			}
		}
	};

	final Action getExampleAction(final String name, final String file) {

		return new AbstractAction() {
			{
				putValue(Action.NAME, name);
			}

			public void actionPerformed(ActionEvent e) {
				InputStream stream = null;
				try {
					URL url = getClass().getResource(
							"/net/sf/sdedit/examples/" + file);
					stream = url.openStream();
					editor.loadCode(stream, file);
				} catch (RuntimeException re) {
					throw re;
				}

				catch (Exception ex) {
					editor.getUI().errorMessage(
							"Loading example from classpath failed\n"
									+ "due to an exception of type "
									+ ex.getClass().getSimpleName() + "\n"
									+ "with the message: " + ex.getMessage());
				} finally {
					if (stream != null) {
						try {
							stream.close();
						} catch (IOException ex) {
							ex.printStackTrace();
						}
					}
				}
			}
		};
	}

	final Action redoAction = new AbstractAction("Redo") {
		{
			putValue(Action.NAME, getShortcut(REDO) + "&Redo");
			putValue(Action.SHORT_DESCRIPTION,
					"Redo the typing that has most recently been undone");

			putValue(ICON_NAME, "redo");
		}

		public void actionPerformed(ActionEvent evt) {
			editor.getUI().redo();
		}
	};

	final Action undoAction = new AbstractAction("Undo") {
		{
			putValue(Action.NAME, getShortcut(UNDO) + "&Undo");
			putValue(Action.SHORT_DESCRIPTION,
					"Undo the typing that has most recently been done");

			putValue(ICON_NAME, "undo");
		}

		public void actionPerformed(ActionEvent evt) {
			editor.getUI().undo();
		}
	};
	//
	// final Action nextTabAction = new AbstractAction("Next tab") {
	// {
	// putValue(Action.NAME, "[" + ctrl() + " shift N]&Next tab");
	// putValue(Action.SHORT_DESCRIPTION, "Switch to the text tab");
	//
	// putValue(ICON_NAME, "next");
	// }
	//
	// public void actionPerformed(ActionEvent evt) {
	// editor.getUI().nextTab();
	// }
	// };
	//
	// final Action previousTabAction = new AbstractAction("Previous tab") {
	// {
	// putValue(Action.NAME, "[" + ctrl() + " shift B]&Previous tab");
	// putValue(Action.SHORT_DESCRIPTION, "Switch to the previous tab");
	//
	// putValue(ICON_NAME, "previous");
	// }
	//
	// public void actionPerformed(ActionEvent evt) {
	// editor.getUI().previousTab();
	// }
	// };

	final Action showAboutDialogAction = new AbstractAction() {
		{
			putValue(Action.NAME, "About");
			putValue(ICON_NAME, "help");
		}

		public void actionPerformed(ActionEvent evt) {
			editor.getUI().showAboutDialog(
					Editor.class.getResource("/net/sf/sdedit/about.html"));
		}
	};

	final Action fullScreenAction = new AbstractAction() {
		{
			putValue(Action.NAME, getShortcut(FULL_SCREEN) + "&Full screen");
			putValue(ICON_NAME, "fullscreen");
			putValue(Action.SHORT_DESCRIPTION,
					"Display the diagram in full-screen mode");
		}

		public void actionPerformed(ActionEvent evt) {
			editor.getUI().fullScreen();
		}
	};

	final Action filterAction = new AbstractAction() {
		{
			putValue(Action.NAME, getShortcut(FILTER) + "&Filter...");
			putValue(ICON_NAME, "filter");
			putValue(Action.SHORT_DESCRIPTION,
					"Filter the (selected) text through a command");
		}

		public void actionPerformed(ActionEvent evt) {
			editor.getUI().toggleFilterMode();
		}
	};

	final Action serverAction = new AbstractAction() {
		{
			putValue(Action.NAME, "Start/stop &RT server...");
			putValue(ICON_NAME, "server");
			putValue(Action.SHORT_DESCRIPTION,
					"Start or stop a server that receives diagram specifications through sockets");
		}

		public void actionPerformed(ActionEvent evt) {
			if (editor.isServerRunning()) {
				if (editor.getUI().confirmOrCancel(
						"Stop real-time diagram server?") == 1) {
					editor.shutDownServer();
				}
				return;
			}
			String port = String.valueOf(ConfigurationManager
					.getGlobalConfiguration().getRealtimeServerPort());
			port = editor
					.getUI()
					.getString(
							"Enter the port where"
									+ " the real-time diagram\nserver should listen (0 for any free port):",
							port);
			if (port == null || port.equals("")) {
				return;
			}
			try {
				int p = Integer.parseInt(port);
				int actualPort = editor.startRealtimeServer(p);
				ConfigurationManager.getGlobalConfiguration()
						.setRealtimeServerPort(p);
				editor.getUI().message(
						"Started real-time diagram server@localhost:"
								+ actualPort);
			} catch (Exception e) {
				editor.getUI().errorMessage(
						"The real-time diagram server could not be started\n"
								+ "due to an exception of type "
								+ e.getClass().getSimpleName()
								+ " with the message:" + e.getMessage());
			}
		}
	};

	final Action getRecentFileAction(final String fileName) {
		return new AbstractAction() {
			{
				putValue(Action.NAME, new File(fileName).getName());
				putValue(Action.SHORT_DESCRIPTION, new File(fileName)
						.getAbsolutePath());

			}

			public void actionPerformed(ActionEvent e) {
				try {
					editor.loadCode(new File(fileName));
				} catch (RuntimeException re) {
					throw re;
				} catch (Exception ex) {
					editor.error("Could not open the file due to an\n"
							+ "exception of type: "
							+ ex.getClass().getSimpleName() + "\n"
							+ "with the message: " + ex.getMessage());
				}
			}
		};
	}

	final Action splitLeftRightAction = new AbstractAction() {
		{
			putValue(Action.NAME, getShortcut(SPLIT_LEFT_RIGHT)
					+ "Split view left/right");
			putValue(ICON_NAME, "view_left_right");
			putValue(Action.SHORT_DESCRIPTION, "Split view left/right");
		}

		public void actionPerformed(ActionEvent evt) {
			editor.getUI().layout(0);
		}
	};

	final Action splitTopBottomAction = new AbstractAction() {
		{
			putValue(Action.NAME, getShortcut(SPLIT_TOP_BOTTOM)
					+ "Split view top/bottom");
			putValue(ICON_NAME, "view_top_bottom");
			putValue(Action.SHORT_DESCRIPTION, "Split view top/bottom");
		}

		public void actionPerformed(ActionEvent evt) {
			editor.getUI().layout(1);
		}
	};

	final Action getExportAction() {
		return Exporter.isAvailable() ? new ExportAction(editor) : null;
	}

	final Action getPrintAction(final String filetype) {
		if (!MultipageExporter.isAvailable()) {
			return null;
		}
		return new AbstractAction() {
			{
				putValue(ICON_NAME, filetype);
				putValue(Action.SHORT_DESCRIPTION,
						"Prints or exports the diagram in multi-page "
								+ filetype.toUpperCase() + " format");
				putValue(Action.NAME, getShortcut(PRINT)
						+ "&Print/export multi-page " + filetype.toUpperCase()
						+ "...");
			}

			public void actionPerformed(ActionEvent e) {
				String code = editor.getUI().getCode();
				if (code != null && code.trim().length() > 0) {
					editor.getUI().showPrintDialog(filetype);
				}
			}
		};
	}
}
