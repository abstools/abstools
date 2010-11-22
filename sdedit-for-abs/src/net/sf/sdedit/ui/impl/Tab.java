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

import static java.lang.System.currentTimeMillis;
import static javax.swing.SwingUtilities.invokeLater;
import static javax.swing.SwingUtilities.isEventDispatchThread;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Component;
import java.awt.Cursor;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.Point;
import java.awt.event.ActionEvent;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.io.File;
import java.util.LinkedList;
import java.util.List;

import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JPopupMenu;
import javax.swing.JScrollPane;
import javax.swing.JSplitPane;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;

import net.sf.sdedit.Constants;
import net.sf.sdedit.config.Configuration;
import net.sf.sdedit.config.ConfigurationManager;
import net.sf.sdedit.diagram.Diagram;
import net.sf.sdedit.diagram.Lifeline;
import net.sf.sdedit.drawable.Arrow;
import net.sf.sdedit.drawable.Drawable;
import net.sf.sdedit.error.DiagramError;
import net.sf.sdedit.error.FatalError;
import net.sf.sdedit.text.TextHandler;
import net.sf.sdedit.ui.PanelPaintDevice;
import net.sf.sdedit.ui.PanelPaintDeviceListener;
import net.sf.sdedit.ui.components.AutoCompletion;
import net.sf.sdedit.ui.components.Stainable;
import net.sf.sdedit.ui.components.StainedListener;
import net.sf.sdedit.ui.components.TextArea;
import net.sf.sdedit.ui.components.ZoomPane;
import net.sf.sdedit.ui.components.AutoCompletion.SuggestionProvider;
import net.sf.sdedit.ui.components.configuration.Bean;

/**
 * A single tab in the user interface, consisting of a diagram view, a text pane
 * and a status bar that can be exchanged by a text field for entering a filter
 * command applied to the text in the pane. All methods that depend on or change
 * the state of GUI components on the screen use the event dispatch thread
 * internally.
 * 
 * @author Markus Strauch
 * 
 */
public class Tab extends JPanel implements Stainable, DocumentListener,
		SuggestionProvider, PropertyChangeListener {

	private static final long serialVersionUID = -4105088603920744983L;

	private LinkedList<Diagram> diagramStack;

	private DiagramError error;

	final private RedrawThread redrawThread;

	final private UserInterfaceImpl ui;

	final private JLabel errorLabel;

	final private JLabel statusLabel;

	final private JPanel bottomPanel;

	final private ZoomPane zoomPane;

	final private TextArea textArea;

	final private FilterCommandField filterField;

	final private JPanel bottom;

	/**
	 * The file that is associated to the content in the text-area.
	 */
	private File file;

	/**
	 * Flag indicating if the text and the contents of the file are consistent,
	 * or if there is no file associated and no text has yet been entered.
	 */
	private boolean stained;

	/**
	 * A list of listeners that are informed when the clean flag gets false.
	 */
	final private List<StainedListener> stainedListeners;

	/**
	 * This string is set to the contents of the text area when it is to be
	 * declared to be consistent via <tt>setClean(true)</tt> or when a file is
	 * loaded.
	 */
	private String code;

	/**
	 * The index of the character in the text-area where an erroreous line
	 * starts, or -1. See {@linkplain #setError(boolean, String, int, int)}.
	 */
	private int errorCharIndex;

	/**
	 * The time in milliseconds since 1970, when a key has been typed for the
	 * last time. On {@linkplain #redraw()}, auto-scrolling will only be done
	 * if at most half a second has passed since that time.
	 */
	private long timeOfLastKeyChange;

	private JPopupMenu menu;

	private JSplitPane splitter;

	private JScrollPane textScroller;

	private boolean filterMode;

	private Bean<Configuration> configuration;

	private Bean<Configuration> oldConfiguration;

	private List<PanelPaintDeviceListener> ppdListeners;

	Tab(UserInterfaceImpl ui, RedrawThread redrawThread, Font codeFont,
			Bean<Configuration> configuration

	) {
		this.ui = ui;
		this.redrawThread = redrawThread;
		diagramStack = new LinkedList<Diagram>();
		textArea = new TextArea();
		textArea.setFont(codeFont);
		textArea.getDocument().addDocumentListener(this);
		textArea.setMinimumSize(new Dimension(100, 100));
		ppdListeners = new LinkedList<PanelPaintDeviceListener>();
		new AutoCompletion(textArea, this, '=', ':', '>');
		menu = createPopupMenu();
		zoomPane = new ZoomPane();
		zoomPane.addMouseListener(new MouseAdapter() {
			@Override
			public void mousePressed(MouseEvent e) {
				if (getDiagram() != null && e.getButton() == MouseEvent.BUTTON3) {
					menu.show((Component) e.getSource(), e.getX(), e.getY());
					e.consume();
					return;
				}
			}
		});
		filterField = new FilterCommandField(ui);
		filterMode = false;
		textScroller = new JScrollPane();
		textScroller.getVerticalScrollBar().setUnitIncrement(30);
		setLineWrap(configuration.getDataObject().isLineWrap());
		splitter = new JSplitPane(JSplitPane.VERTICAL_SPLIT, zoomPane,
				textScroller);
		splitter.setOneTouchExpandable(true);
		splitter.setResizeWeight(0.8);
		setLayout(new BorderLayout());
		stainedListeners = new LinkedList<StainedListener>();
		add(splitter, BorderLayout.CENTER);
		errorCharIndex = -1;
		code = "";
		errorLabel = new JLabel("");
		statusLabel = new JLabel("");
		errorLabel.addMouseListener(new MouseAdapter() {
			@Override
			public void mouseEntered(MouseEvent e) {
				if (errorCharIndex > -1) {
					errorLabel.setCursor(Constants.HAND_CURSOR);
				}
			}

			@Override
			public void mouseExited(MouseEvent e) {
				errorLabel.setCursor(Cursor.getDefaultCursor());
			}

			@Override
			public void mouseClicked(MouseEvent e) {
				if (errorCharIndex > -1) {
					moveCursorToPosition(errorCharIndex);
				}
			}
		});
		bottom = new JPanel();
		bottom.setLayout(new BorderLayout());
		add(bottom, BorderLayout.SOUTH);
		bottomPanel = new JPanel();
		bottomPanel.setLayout(new BorderLayout());

		bottomPanel.add(errorLabel, BorderLayout.CENTER);
		bottomPanel.add(statusLabel, BorderLayout.EAST);
		bottomPanel.setPreferredSize(new Dimension(Integer.MAX_VALUE, 20));

		bottom.add(bottomPanel, BorderLayout.CENTER);

		LookAndFeelManager.instance().registerOrphan(this);
		LookAndFeelManager.instance().registerOrphan(filterField);
		this.configuration = configuration;
		configuration.addPropertyChangeListener(this);
		setClean();
	}

	void addPanelPaintDeviceListener(PanelPaintDeviceListener ppdl) {
		ppdListeners.add(ppdl);
	}

	private void setLineWrap(boolean on) {
		if (on) {
			textScroller.setViewportView(textArea);
		} else {
			JPanel noWrapPanel = new JPanel(new BorderLayout());
			noWrapPanel.add(textArea);
			textScroller.setViewportView(noWrapPanel);
		}
	}

	private void somethingChanged() {
		redrawThread.indicateChange();
		timeOfLastKeyChange = currentTimeMillis();
		invokeLater(new Runnable() {
			public void run() {
				boolean isStained = textArea.getText().length() != code
						.length()
						|| !textArea.getText().equals(code)
						|| !oldConfiguration.equals(configuration);
				if (stained != isStained) {
					fireStainedStatusChanged(isStained);
					stained = isStained;
				}
				ui.enableComponents();
			}
		});
	}

	synchronized DiagramError getDiagramError() {
		return error;
	}

	void renderDiagram() {
		PanelPaintDevice paintDevice = new PanelPaintDevice(true);
		for (PanelPaintDeviceListener listener : ppdListeners) {
			paintDevice.addListener(listener);
		}
		TextHandler textHandler = new TextHandler(getCode());
		Diagram diagram = new Diagram(configuration.getDataObject(),
				textHandler, paintDevice);
		DiagramError newError = null;
		try {
			diagram.generate();
		} catch (RuntimeException e) {
			newError = new FatalError(textHandler, e); 
		} catch (DiagramError e) {
			newError = e;
		}
		synchronized (diagramStack) {
			diagramStack.addLast(diagram);
			synchronized (this) {
				error = newError;
			}
		}
	}

	Diagram getDiagram() {
		synchronized (diagramStack) {
			switch (diagramStack.size()) {
			case 0:
				return null;
			case 1:
				return diagramStack.getLast();
			default:
				Diagram diagram = diagramStack.getLast();
				diagramStack.clear();
				diagramStack.addLast(diagram);
				return diagram;
			}
		}
	}

	public void addStainedListener(StainedListener listener) {
		stainedListeners.add(listener);
	}

	/**
	 * 
	 * @param layout
	 *            0 for a split along the x-axis, 1 for the y-axis
	 */
	void layout(int layout) {
		remove(splitter);
		switch (layout) {

		case 0:
			splitter = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT,
					textScroller, zoomPane);
			splitter.setResizeWeight(0.2);
			break;
		case 1:
			splitter = new JSplitPane(JSplitPane.VERTICAL_SPLIT, zoomPane,
					textScroller);
			splitter.setOneTouchExpandable(true);
			splitter.setResizeWeight(0.8);
			break;
		default:
			throw new IllegalArgumentException("layout " + layout
					+ " not supported");
		}
		splitter.setOneTouchExpandable(true);
		add(splitter, BorderLayout.CENTER);
		revalidate();
		configuration.getDataObject().setVerticallySplit(layout == 1);
		ui.enableComponents();
	}

	TextArea getTextArea() {
		return textArea;
	}

	boolean isClean() {
		return !stained;
	}

	void setFile(File file) {
		this.file = file;
	}

	File getFile() {
		return file;
	}

	ZoomPane getZoomPane() {
		return zoomPane;
	}

	void setClean() {
		boolean fire = stained;
		code = textArea.getText();
		stained = false;
		oldConfiguration = configuration.copy();
		if (fire) {
			fireStainedStatusChanged(false);
		}
	}

	private void fireStainedStatusChanged(boolean status) {
		for (StainedListener listener : stainedListeners) {
			listener.stainedStatusChanged(status);
		}
	}

	/**
	 * Clears the diagram view.
	 */
	void clear() {
		synchronized (diagramStack) {
			diagramStack.clear();
		}
		redraw();
	}

	/**
	 * Scrolls the diagram view to the top-left corner.
	 */
	void home() {
		invokeLater(new Runnable() {
			public void run() {
				zoomPane.home();
			}
		});
	}

	/**
	 * Returns the code that is currently begin displayed by the text-area.
	 * 
	 * @return the code that is currently being displayed by the text-area.
	 */
	String getCode() {
		return textArea.getText();
	}

	/**
	 * Changes the code displayed by the text-area. After that,
	 * {@linkplain #isClean()} will return true.
	 * 
	 * @param code
	 *            the code to be displayed by the text-area
	 */
	void setCode(final String code) {
		if (isEventDispatchThread()) {
			textArea.setText(code);
			setClean();
			return;
		}
		invokeLater(new Runnable() {
			public void run() {
				textArea.setText(code);
				setClean();
			}
		});
	}

	void redraw() {
		invokeLater(new Runnable() {
			public void run() {
				Diagram diagram = getDiagram();
				if (diagram != null) {
					zoomPane.setViewportView(((PanelPaintDevice) diagram
							.getPaintDevice()).getPanel());
					if (ConfigurationManager.getGlobalConfiguration()
							.isAutoScroll()
							&& currentTimeMillis() - timeOfLastKeyChange <= 500) {
						scrollToCurrentDrawable();
					}
					
				} else {
					zoomPane.setViewportView(null);
				}
				ui.enableComponents();
			}
		});
	}

	void moveCursorToPosition(int position) {
		textArea.requestFocusInWindow();
		textArea.setCaretPosition(position);
	}

	void undo() {
		textArea.undo();
	}

	void redo() {
		textArea.redo();
	}

	void enterFilterMode() {
		if (filterMode) {
			return;
		}
		filterMode = true;
		invokeLater(new Runnable() {
			public void run() {
				filterField.reset();
				bottom.remove(bottomPanel);
				bottom.add(filterField, BorderLayout.CENTER);
				bottom.revalidate();
				filterField.requestFocus();
			}
		});
	}

	void toggleFilterMode() {
		if (filterMode) {
			leaveFilterMode();
		} else {
			enterFilterMode();
		}
	}

	void leaveFilterMode() {
		if (!filterMode) {
			return;
		}
		filterMode = false;
		invokeLater(new Runnable() {
			public void run() {
				filterField.reset();
				bottom.remove(filterField);
				bottom.add(bottomPanel, BorderLayout.CENTER);
				bottom.revalidate();
			}
		});
	}

	private void scrollToCurrentDrawable() {
		int begin = textArea.getCurrentLineBegin();
		Diagram diagram = getDiagram();
		if (diagram != null) {
			PanelPaintDevice ppd = (PanelPaintDevice) diagram.getPaintDevice();
			Drawable drawable = diagram.getDrawableForState(begin);
			if (drawable instanceof Arrow) {
				Arrow arrow = (Arrow) drawable;
				Point textPosition = arrow.getTextPosition();
				int x = textPosition != null ? textPosition.x : arrow.getLeft();
				float xratio = 1F * x / ppd.getWidth();
				int y = drawable.getTop();
				float yratio = 1F * y / ppd.getHeight();
				zoomPane.scrollToPosition(xratio, yratio);
			} else {
				if (drawable != null) {
					int x = drawable.getLeft();
					float xratio = 1F * x / ppd.getWidth();
					int y = drawable.getTop();
					float yratio = 1F * y / ppd.getHeight();
					zoomPane.scrollToPosition(xratio, yratio);
				} else {
					int caret = textArea.getCaretPosition();
					if (textArea.getText().substring(caret).trim().length() == 0) {
						zoomPane.scrollToBottom();
					}
				}
			}
		}
	}

	void append(final String text) {
		if (isEventDispatchThread()) {
			textArea.setText(textArea.getText() + text);
			// happens automatically via DocumentListener
			// redrawThread.indicateChange();
		} else {
			invokeLater(new Runnable() {
				public void run() {
					textArea.setText(textArea.getText() + text);
					// redrawThread.indicateChange();
				}
			});
		}
	}

	void setStatus(final String status) {
		invokeLater(new Runnable() {
			public void run() {
				statusLabel.setText(status + "    ");
			}
		});
	}

	void setError(final boolean warning, final String error, final int begin,
			final int end) {
		invokeLater(new Runnable() {
			public void run() {
				if (warning) {
					errorLabel.setForeground(Color.ORANGE);
				} else {
					errorLabel.setForeground(Color.RED);
				}
				errorLabel.setText(error);
				errorCharIndex = begin;
				textArea.markError(begin, end);
			}
		});
	}

	@SuppressWarnings("serial")
	private JPopupMenu createPopupMenu() {
		JPopupMenu popup = new JPopupMenu();
		Action fitSize = new AbstractAction() {
			{
				putValue(Action.NAME, "Fit size");
			}

			public void actionPerformed(ActionEvent e) {
				zoomPane.fitSize();
			}
		};

		Action originalZoom = new AbstractAction() {
			{
				putValue(Action.NAME, "100 %");
			}

			public void actionPerformed(ActionEvent e) {
				zoomPane.setScale(1);
			}
		};
		popup.add(fitSize);
		popup.add(originalZoom);
		return popup;
	}

	/**
	 * @see net.sf.sdedit.ui.components.AutoCompletion.SuggestionProvider#getSuggestions(java.lang.String)
	 */
	public List<String> getSuggestions(String prefix) {
		List<String> suggestions = new LinkedList<String>();
		Diagram diag = getDiagram();
		if (diag != null) {
			for (Lifeline lifeline : diag.getAllLifelines()) {
				String name = lifeline.getName();
				if (name.startsWith(prefix)) {
					suggestions.add(name);
				}
			}
		}
		return suggestions;
	}

	Bean<Configuration> getConfiguration() {
		return configuration;
	}

	void setConfiguration(Bean<Configuration> configuration) {
		if (configuration != null) {
			this.configuration.removePropertyChangeListener(this);
		}
		this.configuration = configuration;
		this.configuration.addPropertyChangeListener(this);
		oldConfiguration = configuration;
	}

	/**
	 * Called on configuration changes.
	 */
	public void propertyChange(PropertyChangeEvent evt) {
		somethingChanged();
		if (evt.getPropertyName().toLowerCase().equals("linewrap")) {
			boolean wrap = (Boolean) evt.getNewValue();
			setLineWrap(wrap);
		}
	}

	/**
	 * Called on source text changes.
	 */
	public void changedUpdate(DocumentEvent e) {
		somethingChanged();
	}

	/**
	 * Called on source text changes.
	 */
	public void insertUpdate(DocumentEvent e) {
		somethingChanged();
	}

	/**
	 * Called on source text changes.
	 */
	public void removeUpdate(DocumentEvent e) {
		somethingChanged();
	}
}
