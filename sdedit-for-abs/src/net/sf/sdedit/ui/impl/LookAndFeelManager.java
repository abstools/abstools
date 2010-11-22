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

import java.awt.Font;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.io.File;
import java.net.MalformedURLException;
import java.net.URL;
import java.net.URLClassLoader;
import java.util.Map;
import java.util.WeakHashMap;

import javax.swing.JComponent;
import javax.swing.RootPaneContainer;
import javax.swing.SwingUtilities;
import javax.swing.UIManager;
import javax.swing.UnsupportedLookAndFeelException;
import javax.swing.UIManager.LookAndFeelInfo;

/**
 * For dynamically changing the look and feel of whole frames and dialogs.
 * 
 * @author Markus Strauch
 */
public/* singleton */class LookAndFeelManager extends URLClassLoader {

	/*
	 * These weak hash maps serve as sets
	 */
	private Map<JComponent, Object> rootComponents = new WeakHashMap<JComponent, Object>();

	private Map<RootPaneContainer, Object> containers = new WeakHashMap<RootPaneContainer, Object>();

	private Map<JComponent, Object> orphans = new WeakHashMap<JComponent, Object>();

	/*
	 * The dummy value for the weak hash maps
	 */
	private Object dummy = new Object();

	private static LookAndFeelManager instance;

	private String currentLAF;

	private static UIManager.LookAndFeelInfo[] available;

	static {
		instance = new LookAndFeelManager();
		available = new UIManager.LookAndFeelInfo[UIManager
				.getInstalledLookAndFeels().length];
		int i = 0;
		for (LookAndFeelInfo info : UIManager.getInstalledLookAndFeels()) {
			available[i] = info;
			i++;
		}

	}

	public void setFont(Font font) {
		UIManager.put("Button.font", font);
		UIManager.put("ToggleButton.font", font);
		UIManager.put("RadioButton.font", font);
		UIManager.put("CheckBox.font", font);
		UIManager.put("ColorChooser.font", font);
		UIManager.put("ComboBox.font", font);
		UIManager.put("Label.font", font);
		UIManager.put("List.font", font);
		UIManager.put("MenuBar.font", font);
		UIManager.put("MenuItem.font", font);
		UIManager.put("RadioButtonMenuItem.font", font);
		UIManager.put("CheckBoxMenuItem.font", font);
		UIManager.put("Menu.font", font);
		UIManager.put("PopupMenu.font", font);
		UIManager.put("OptionPane.font", font);
		UIManager.put("Panel.font", font);
		UIManager.put("ProgressBar.font", font);
		UIManager.put("ScrollPane.font", font);
		UIManager.put("Viewport.font", font);
		UIManager.put("TabbedPane.font", font);
		UIManager.put("Table.font", font);
		UIManager.put("TableHeader.font", font);
		UIManager.put("TextField.font", font);
		UIManager.put("PasswordField.font", font);
		UIManager.put("TextArea.font", font);
		UIManager.put("TextPane.font", font);
		UIManager.put("EditorPane.font", font);
		UIManager.put("TitledBorder.font", font);
		UIManager.put("ToolBar.font", font);
		UIManager.put("ToolTip.font", font);
		UIManager.put("Tree.font", font);
	}

	private LookAndFeelManager() {
		super(new URL[0], Thread.currentThread().getContextClassLoader());
	}

	@SuppressWarnings("unchecked")
	public static UIManager.LookAndFeelInfo[] getAvailableLookAndFeels() {
		return available;
	}

	/**
	 * Returns the singleton <tt>LookAndFeelManager</tt>.
	 * 
	 * @return the singleton <tt>LookAndFeelManager</tt>
	 */
	public static LookAndFeelManager instance() {
		return instance;
	}

	public void useExternalLAF(File jarFile, String cls)
			throws MalformedURLException, ClassNotFoundException,
			InstantiationException, IllegalAccessException,
			UnsupportedLookAndFeelException {
		addURL(jarFile.toURI().toURL());
		Class<?> clazz = loadClass(cls, true);
		ClassLoader contextCL = Thread.currentThread().getContextClassLoader();
		Thread.currentThread().setContextClassLoader(this);
		UIManager.installLookAndFeel(clazz.getSimpleName(), cls);
		UIManager.setLookAndFeel(cls);
		//Thread.currentThread().setContextClassLoader(contextCL);
	}

	/**
	 * Registers a JDialog so its subcomponents will get another look and feel
	 * when {@linkplain #updateUserInterface()} is called.
	 * 
	 * @param dialog
	 *            a JDialog that is to be managed by this
	 *            <tt>LookAndFeelManager</tt>, so the JDialog's subcomponents
	 *            look and feel can be changed by it
	 */
	public void register(RootPaneContainer container) {
		containers.put(container, container);
	}

	private void collectFrom(RootPaneContainer container) {
		rootComponents.put((JComponent) container.getGlassPane(), dummy);
		rootComponents.put(container.getRootPane(), dummy);
		rootComponents.put(container.getLayeredPane(), dummy);
		rootComponents.put((JComponent) container.getContentPane(), dummy);
		rootComponents.put(container.getRootPane().getJMenuBar(), dummy);
		if (container.getRootPane().getJMenuBar() != null) {
			rootComponents.put(container.getRootPane().getJMenuBar(), dummy);
		}
	}

	/**
	 * Call <tt>updateUI()</tt> on all components inside the registered frames
	 * and dialogs.
	 */
	public void updateUserInterface() {
		rootComponents.clear();
		for (RootPaneContainer container : containers.keySet()) {
			collectFrom(container);
		}
		for (JComponent comp : rootComponents.keySet()) {
			if (comp != null) {
				SwingUtilities.updateComponentTreeUI(comp);
			}
		}
		for (JComponent comp : orphans.keySet()) {
			SwingUtilities.updateComponentTreeUI(comp);
		}
	}

	public void registerOrphan(JComponent orphan) {
		orphans.put(orphan, dummy);
	}

	public boolean changeTo(String lookAndFeelName) {
		currentLAF = lookAndFeelName;
		for (LookAndFeelInfo info : available) {
			if (info.getName().equals(lookAndFeelName)) {
				try {
					UIManager.setLookAndFeel(info.getClassName());
					updateUserInterface();
					return true;
				} catch (RuntimeException re) {
					throw re;
				} catch (Throwable t) {
					t.printStackTrace();
					return false;
				}

			}
		}
		return false;
	}

	public void propertyChange(PropertyChangeEvent evt) {
		if (evt.getPropertyName().equals("lookAndFeel")) {
			String newLAF = (String) evt.getNewValue();
			if (currentLAF == null || !newLAF.equals(currentLAF)) {
				changeTo(newLAF);
			}
		}
	}
}
