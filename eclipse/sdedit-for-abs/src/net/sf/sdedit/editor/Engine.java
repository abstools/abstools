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

import java.io.BufferedReader;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStreamWriter;
import java.io.PrintWriter;
import java.io.StringReader;
import java.util.LinkedList;

import net.sf.sdedit.config.Configuration;
import net.sf.sdedit.config.ConfigurationManager;
import net.sf.sdedit.diagram.Diagram;
import net.sf.sdedit.error.DiagramError;
import net.sf.sdedit.error.FatalError;
import net.sf.sdedit.error.SemanticError;
import net.sf.sdedit.text.TextHandler;

/**
 * Utility class for drawing diagrams on a separate thread.
 * 
 * @author Markus Strauch
 */
final class Engine extends Thread {

	private static final String getFatalErrorDescription(Throwable ex) {
		return "A FATAL ERROR has occured: " + ex.getClass().getSimpleName();
	}

	Engine(Editor editor) {
		this.editor = editor;
		setName("Diagram-Engine");
		setDaemon(true);
		stack = new LinkedList<Boolean>();
		start();
	}

	private final Editor editor;

	private final LinkedList<Boolean> stack;

	/**
	 * (A)synchronously initiates a new drawing process and returns a reference
	 * to the diagram. When the processing is asynchronous and the diagram is
	 * finished, a notification to the calling editor will be sent via
	 * editor.getUI().setPaintDevice(...) with the paint device belonging to the
	 * diagram as a parameter.
	 * 
	 * @param configuration
	 *            the configuration of the diagram to be drawn
	 * @param syntaxCheckOnly
	 * @param synchronous
	 *            flag denoting if the diagram generation is to take place
	 *            (synchronously) on the current thread (true) or on a
	 *            background thread (false)
	 * 
	 * @return a reference to the (not yet finished) diagram
	 */
	final void render(final Configuration configuration,
			final boolean syntaxCheckOnly, final boolean synchronous) {
		if (synchronous) {
			_render(syntaxCheckOnly);
		} else {
			synchronized (stack) {
				stack.addLast(syntaxCheckOnly);
				stack.notify();
			}
		}
	}

	private synchronized void _render(final boolean syntaxCheckOnly) {
		editor.getUI().leaveFilterMode();
		Diagram diagram = editor.getUI().renderDiagram();
		// TODO
		// This reference can be null, at least when this
		// method is not synchronized. Why?
		if (diagram == null) {
			return;
		}
		DiagramError error = editor.getUI().getDiagramError();
		if (error == null) {
			editor.getUI().setErrorStatus(false, "", -1, -1);
			if (diagram.getFragmentManager().openFragmentsExist()) {
				editor
						.getUI()
						.setErrorStatus(
								true,
								"Warning: There are open comments. Use [c:<type> <text>]...[/c]",
								-1, -1);
			}

			int noteNumber = diagram.getNextFreeNoteNumber();
			if (noteNumber == 0) {
				editor.getUI().setStatus("");
			} else {
				editor.getUI().setStatus(
						"Next note number: " + diagram.getNextFreeNoteNumber());
			}
		} else {
			editor.getUI().setStatus("");
			if (error instanceof FatalError) {
				FatalError fatal = (FatalError) error;
				System.err
						.println("********************************************************");
				System.err
						.println("*                                                      *");
				System.err
						.println("*            A FATAL ERROR HAS OCCURED.                *");
				System.err
						.println("*                                                      *");
				System.err
						.println("********************************************************");
				error.getCause().printStackTrace();
				// cautiously embedding this call into a try/catch-block
				try {
					handle(diagram, fatal.getCause());
				} catch (Throwable t) {
					t.printStackTrace();
				}
			} else {
				TextHandler handler = (TextHandler) error.getProvider();
				String prefix = "";

				if (error instanceof SemanticError) {
					prefix = diagram.isThreaded()
							&& diagram.getCallerThread() != -1 ? "Thread "
							+ diagram.getCallerThread() + ": " : "";
				}
				editor.getUI().setErrorStatus(false,
						prefix + error.getMessage(),
						handler.getLineBegin() - 1, handler.getLineEnd());
			}
		}
		if (!syntaxCheckOnly) {
			editor.getUI().redraw();
		}
	}

	private void saveLog(File logFile, Throwable exception,
			TextHandler textHandler) throws IOException {

		FileOutputStream stream = new FileOutputStream(logFile);
		try {
			PrintWriter printWriter = new PrintWriter(new OutputStreamWriter(
					stream, ConfigurationManager.getGlobalConfiguration()
							.getFileEncoding()));
			BufferedReader bufferedReader = new BufferedReader(
					new StringReader(textHandler.getText()));
			int error = textHandler.getLineNumber();
			printWriter.println(exception.getClass().getSimpleName()
					+ " has occurred in line " + error + "\n");
			int i = 0;
			for (;;) {
				String line = bufferedReader.readLine();
				if (line == null) {
					bufferedReader.close();
					break;
				}
				line = line.trim();
				if (i == error - 1) {
					line = ">>>>>>>>>>>>>> " + line;
				}
				printWriter.println(line);
				i++;
			}
			printWriter.println("\n\n:::::::::::::::::::::::::::::\n\n");
			exception.printStackTrace(printWriter);
			printWriter.flush();
			printWriter.close();
			editor
					.getUI()
					.errorMessage(
							getFatalErrorDescription(exception)
									+ "\n\nAn error log file has been saved under \n"
									+ logFile.getAbsolutePath()
									+ "\n\n"
									+ "Please send an e-mail with this file as an attachment to:\n"
									+ "sdedit@users.sourceforge.net");
		} finally {
			stream.close();
		}
	}

	private void handle(Diagram diagram, RuntimeException ex) {

		String name = "sdedit-errorlog-" + System.currentTimeMillis();

		File errorLogFile = new File(name);
		try {
			errorLogFile.createNewFile();
		} catch (IOException e0) {
			try {
				errorLogFile = new File(System.getProperty("user.home"), name);
				errorLogFile.createNewFile();
			} catch (IOException e1) {
				errorLogFile = new File(System.getProperty("java.io.tmpdir",
						name));
			}
		}

		try {
			saveLog(errorLogFile, ex, (TextHandler) diagram.getDataProvider());
		} catch (IOException e) {
			editor.getUI().errorMessage(
					getFatalErrorDescription(ex)
							+ "\n\nAn error log file could not be saved.");
		}

	}

	/**
	 * Draws diagrams submitted by calls to render(). When the diagram is
	 * finished, it notifies the user interface.
	 */
	@Override
	public void run() {
		while (true) {
			boolean syntaxCheckOnly;
			synchronized (stack) {
				while (stack.isEmpty()) {
					try {
						stack.wait();
					} catch (InterruptedException ie) {
						Thread.currentThread().interrupt();
					}
				}
				syntaxCheckOnly = stack.removeLast();
				stack.clear();
			}
			_render(syntaxCheckOnly);
		}
	}
}
