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

package net.sf.sdedit.ui;

import java.io.File;
import java.net.URL;

import javax.swing.Action;

import net.sf.sdedit.config.Configuration;
import net.sf.sdedit.diagram.Diagram;
import net.sf.sdedit.error.DiagramError;
import net.sf.sdedit.ui.components.buttons.Activator;
import net.sf.sdedit.ui.components.configuration.Bean;
import net.sf.sdedit.ui.components.configuration.ConfigurationAction;

/**
 * Specifies the methods required of a (multi-tabbed) user interface for the
 * Quick Sequence Diagram Editor.
 * 
 * @author Markus Strauch
 * 
 */
public interface UserInterface {
	/**
	 * Adds a listener to this UserInterface.
	 * 
	 * @param listener
	 *            a listener for the UserInterface
	 */
	public void addListener(UserInterfaceListener listener);

	/**
	 * Adds a choosable component to the user interface such that on choosing it
	 * the given action is performed.
	 * 
	 * @param category
	 *            a string denoting the category of the action
	 * @param action
	 *            a performable action
	 * @param activator
	 *            an {@linkplain Activator} that decides whether the action
	 *            resp. its associated button is to be enabled
	 */
	public void addAction(String category, Action action, Activator activator);

	public void addCategory(String category, String icon);

	public void removeAction(String category, Action action);

	public void nextTab();

	public void previousTab();

	public void addConfigurationAction(String category,
			ConfigurationAction<?> action, Activator activator);

	/**
	 * If a regular tab is open, creates a diagram from the code currently being
	 * displayed and returns it. Otherwise returns <tt>null</tt>.
	 * 
	 * @return the newly created diagram
	 */
	public Diagram renderDiagram();

	/**
	 * Returns the diagram rendered most recently inside the current tab, or
	 * null, if there is not a regular tab visible.
	 * 
	 * @return the diagram rendered most recently
	 */
	public Diagram getDiagram();

	/**
	 * Returns the first error that has occurred during the most recent
	 * rendering of a diagram, if any.
	 * 
	 * @return the first error that has occurred during the most recent
	 *         rendering of a diagram, if any
	 */
	public DiagramError getDiagramError();

	/**
	 * Sets the action that is to be performed when the user quits.
	 * 
	 * @param action
	 *            the action to be performed when the user quits
	 */
	public void setQuitAction(Action action);

	/**
	 * Sets a piece of code in the current tab to generate a diagram from.
	 * 
	 * @param text
	 *            a piece of code to generate a diagram from
	 */
	public void setCode(String text);

	/**
	 * Appends text in the text area of the tab identified by the first
	 * argument.
	 * 
	 * @param tab
	 *            the name of the tab where the text is to be appended
	 * @param text
	 *            the text to be appended
	 */
	public void appendText(String tab, String text);

	/**
	 * Returns the code from which a diagram is to be generated or an empty
	 * string if there is no code.
	 * 
	 * @return the code from which a diagram is to be generated or an empty
	 *         string if there is no code
	 */
	public String getCode();

	/**
	 * Shows the diagram generated from the code in the current tab.
	 */
	public void redraw();

	/**
	 * Sets a status line.
	 * 
	 * @param status
	 *            a status line
	 */
	public void setStatus(String status);

	/**
	 * Asks the user to confirm something or to cancel the process that lead to
	 * the point where something must be confirmed.
	 * 
	 * @param message
	 *            a message describing what is to be confirmed
	 * @return 1 if the user confirms, 0 if the user disagrees, -1 for cancel
	 */
	public int confirmOrCancel(String message);

	/**
	 * Asks the user for confirmation.
	 * 
	 * @param message
	 *            a message describing what is to be confirmed
	 * @return true iff the user confirms
	 */
	public boolean confirm(String message);

	/**
	 * Lets the user choose one or more files and returns them.
	 * 
	 * @param open
	 *            true if the files are to be opened
	 * @param multiple
	 *            true if multiple files can be selected
	 * @param message
	 *            the message describing the purpose of the file to be chosen
	 * @param directory
	 *            the directory where to start choosing, or <tt>null</tt>
	 * @param filter
	 *            can be empty, in which case no file filter is used, otherwise
	 *            the first string is a description of the filter and the
	 *            remaining strings are extensions of files to be shown
	 * 
	 * @return an array of chosen files or <tt>null</tt> if no file has been
	 *         chosen
	 */
	public File[] getFiles(boolean open, boolean multiple, String message,
			String file, File directory, String... filter);

	/**
	 * Asks the user to type some string into an input dialog.
	 * 
	 * @param question
	 *            the question to which the string to be typed is an answer
	 * @param initialValue
	 *            the initial string that is suggested as an answer
	 * @return the string typed in by the user
	 */
	public String getString(String question, String initialValue);

	/**
	 * Shows a window where the preferences can be set
	 * 
	 * @param local
	 *            flag denoting if diagram preferences are to be configured
	 */
	public void configure(boolean local);

	/**
	 * Displays a message to the user.
	 * 
	 * @param msg
	 *            a message
	 */
	public void message(String msg);

	/**
	 * Makes the user interface visible.
	 */
	public void showUI();

	/**
	 * Displays an error message to the user.
	 * 
	 * @param msg
	 *            an error message
	 */
	public void errorMessage(String msg);

	/**
	 * Sets the title of the user interface.
	 * 
	 * @param title
	 *            the title of the user interface.
	 */
	public void setTitle(String title);

	/**
	 * Adds an action that can be quickly performed (by just a single click, for
	 * instance).
	 * 
	 * @param quickAction
	 *            an action that can be quickly performed
	 */
	public void addToToolbar(Action action, Activator activator);

	public void addToolbarSeparator();

	/**
	 * Moves the cursor to the given position in the text area.
	 * 
	 * @param pos
	 *            the position to where the cursor is to be moved
	 */
	public void moveCursorToPosition(int pos);

	/**
	 * Displays text that informs about what was entered wrongly.
	 * 
	 * @param warning
	 *            flag indicating if the user should only be warned
	 * @param errorStatus
	 *            text that informs about what was entered wrongly
	 * @param begin
	 *            the index where the string causing the error begins
	 * @param end
	 *            the index where the string causing the error ends
	 */
	public void setErrorStatus(boolean warning, String errorStatus, int begin,
			int end);

	/**
	 * Makes the left top corner of the diagram visible, which may imply that
	 * the part that is currently visible is scrolled out of view.
	 */
	public void home();

	/**
	 * Adds a new tab to the user interface which becomes the tab that is
	 * currently selected.
	 * 
	 * @param title
	 *            the title of the tab
	 * @param configuration
	 *            the configuration to be used for the diagram that is displayed
	 *            by the tab (typically a default configuration for empty tabs
	 *            or a loaded configuration for tabs that show diagrams loaded
	 *            from files)
	 * @return the actual unique title of the newly added tab (may differ from
	 *         the original title)
	 */
	public String addTab(String title, Bean<Configuration> configuration);

	/**
	 * Sets the title of the current tab.
	 * 
	 * @param title
	 *            the title of the current tab
	 */
	public void setTabTitle(String title);

	/**
	 * Removes the current tab. If the parameter <tt>check</tt> is
	 * <tt>true</tt> and there is no other tab open, the tab cannot be
	 * removed.
	 * 
	 * @param check
	 *            flag denoting whether the existence of another tab is a
	 *            pre-condition for removing the current tab
	 * @return a flag denoting if the current tab has in fact been removed
	 */
	public boolean removeCurrentTab(boolean check);

	/**
	 * Returns the file that is associated to the text in the current tab, may
	 * be <tt>null</tt> if no such file exists.
	 * 
	 * @return the file that is associated to the text in the current tab
	 */
	public File getCurrentFile();

	/**
	 * Associates the given file to the text in the current tab.
	 * 
	 * @param file
	 *            the file to be associated to the text in the current tab
	 */
	public void setCurrentFile(File file);

	/**
	 * A flag denoting if the user has not changed the text currently displayed
	 * since last time the current tab had been added or
	 * {@linkplain #setClean()} had been called.
	 * 
	 * @return flag denoting if the user has changed the text currently
	 *         displayed
	 */
	public boolean isClean();

	/**
	 * Specifies that the text currently displayed is &quot;clean&quot;, this
	 * means there are no changes made by the user.
	 * 
	 */
	public void setClean();

	/**
	 * Displays a help page.
	 * 
	 * @param title
	 *            the title of the tab where the help page is shown
	 * @param path
	 *            the path to the help document (for example /foo/bar/help.html
	 *            is the path to help.html in the package foo.bar
	 * @param navigation
	 *            flag denoting if a navigation tree is to be built from the
	 *            anchor names inside the help page
	 * 
	 */
	public void help(String title, String path, boolean navigation);

	/**
	 * Undoes the last change made to the text area.
	 * 
	 */
	public void undo();

	/**
	 * Redoes the last change that had been made to the text area and that had
	 * been made undone.
	 * 
	 */
	public void redo();

	/**
	 * Clears the area where the diagram is displayed.
	 * 
	 */
	public void clearDisplay();

	/**
	 * Returns the number of tabs currently open.
	 * 
	 * @return the number of tabs that are currently open
	 */
	public int getNumberOfTabs();

	/**
	 * Shows an about-dialog with a content found at the URL given.
	 * 
	 * @param aboutURL
	 *            a URL with the content of the about dialog
	 */
	public void showAboutDialog(URL aboutURL);

	/**
	 * Shows the diagram most recently generated in a full screen view.
	 */
	public void fullScreen();

	/**
	 * Enters the filter mode where the user can enter a command for filtering
	 * the contents of the text area.
	 */
	public void enterFilterMode();

	/**
	 * Leaves the filter mode, no command can be entered any more.
	 */
	public void leaveFilterMode();

	/**
	 * Enters the filter mode, if we are not already in it, otherwise leaves it.
	 */
	public void toggleFilterMode();

	public void showPrintDialog(String filetype);

	/**
	 * Selects the first tab that shows a diagram associated to the given file
	 * or does nothing, if no such tab exists.
	 * 
	 * @param file
	 *            a diagram file
	 * @return flag denoting if an appropriate tab has been found
	 */
	public boolean selectTabWith(File file);

	/**
	 * This method is called when the application is exited. It is <i>not</i>
	 * supposed to call <tt>System.exit()</tt>.
	 */
	public void exit();

	public void layout(int layout);

	/**
	 * Returns a flag denoting if there are currently any diagram elements
	 * displayed in the current tab
	 * 
	 * @return a flag denoting if there are currently any diagram elements
	 *         displayed in the current tab
	 */
	public boolean isDiagramBlank();

	/**
	 * Returns a flag denoting if a tab with a diagram is currently selected.
	 * 
	 * @return a flag denoting if a tab with a diagram is currently selected
	 */
	public boolean isDiagramTabSelected();

	/**
	 * Returns the configuration belonging to the active tab or null if the
	 * active tab does show a help page.
	 * 
	 * @return the configuration belonging to the active tab
	 */
	public Bean<Configuration> getConfiguration();
	
	public String getOption (String text, String... options);
}
