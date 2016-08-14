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

package net.sf.sdedit.ui.components;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Component;
import java.awt.Cursor;
import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Rectangle;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.awt.event.MouseMotionListener;
import java.util.Arrays;
import java.util.LinkedList;
import java.util.List;
import java.util.Observable;

import javax.swing.JComponent;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JViewport;

public class ZoomPane extends JPanel implements Scalable, MouseListener,
		MouseMotionListener {

	private final static long serialVersionUID = 0xAB343927;

	private double scale;

	private Zoomable<? extends JComponent> viewPort;

	private JScrollPane scrollPane;

	private List<MouseListener> mouseListeners;

	private List<MouseMotionListener> mouseMotionListeners;

	private MyObservable observable = new MyObservable();

	private JComponent root;

	public ZoomPane() {
		this(true);
	}

	public ZoomPane(boolean enableScrolling) {
		super();
		mouseListeners = new LinkedList<MouseListener>();
		mouseMotionListeners = new LinkedList<MouseMotionListener>();
		scale = 1;
		inheritListeners();
		setLayout(new BorderLayout());
		internal.addMouseListener(this);
		internal.addMouseMotionListener(this);
		JViewport port = new GrabbableViewport();
		port.setView(internal);

		if (enableScrolling) {
			scrollPane = new JScrollPane();
			scrollPane.setViewport(port);
			scrollPane.setDoubleBuffered(true);
			scrollPane.getVerticalScrollBar().setUnitIncrement(30);
			add(scrollPane, BorderLayout.CENTER);
		} else {
			add(internal, BorderLayout.CENTER);
		}
		root = this;
	}
	
	/*
	 * Note: this method has been inserted because otherwise in full-screen
	 * mode the shape of the cursor would not change. FullScreen sets
	 * the root to its "glass pane", this is enough to work a way around
	 * the problem.
	 */

	/**
	 * Sets the <tt>JComponent</tt> whose cursor is changed when the
	 * <tt>setCursor</tt> method is called on the internal <tt>JPanel</tt>
	 * containing the zoomable content.
	 * 
	 * @param root
	 *            the <tt>JComponent</tt> whose cursor is changed when the
	 *            <tt>setCursor</tt> method is called on the internal
	 *            <tt>JPanel</tt> containing the zoomable content
	 */
	public void setRoot(JComponent root) {
		this.root = root;
	}

	public void addMouseListener(MouseListener mouseListener) {
		internal.addMouseListener(mouseListener);
	}

	public void addMouseMotionListener(MouseMotionListener mouseMotionListener) {
		internal.addMouseMotionListener(mouseMotionListener);
	}

	public JPanel getPanel() {
		return internal;
	}

	public void scrollToBottom() {
		scrollPane.getVerticalScrollBar().setValue(
				scrollPane.getVerticalScrollBar().getMaximum());
	}

	public void scrollToPosition(float xratio, float yratio) {
		final int hx = internal.getVisibleRect().width / 2;
		final int hy = internal.getVisibleRect().height / 2;

		final int ymax = scrollPane.getVerticalScrollBar().getMaximum();

		int y = (int) (yratio * ymax);
		scrollPane.getVerticalScrollBar().setValue(y - hy);

		final int xmax = scrollPane.getHorizontalScrollBar().getMaximum();
		int x = (int) (xratio * xmax);

		scrollPane.getHorizontalScrollBar().setValue(x - hx);
	}

	public void home() {
		scrollPane.getHorizontalScrollBar().setValue(0);
		scrollPane.getVerticalScrollBar().setValue(0);
	}

	private void inheritListeners() {

		mouseListeners.clear();
		mouseMotionListeners.clear();
		if (viewPort != null) {
			mouseListeners.addAll(Arrays.asList(((Component) viewPort)
					.getMouseListeners()));
			mouseMotionListeners.addAll(Arrays.asList(((Component) viewPort)
					.getMouseMotionListeners()));
		}
	}

	public void fitWidth() {
		if (viewPort == null) {
			return;
		}
		setScale(1D * getWidth() / viewPort.getAbsoluteWidth());
	}

	public void fitHeight() {
		if (viewPort == null) {
			return;
		}
		setScale(1D * getHeight() / viewPort.getAbsoluteHeight());
	}

	public void fitSize() {
		if (viewPort == null) {
			return;
		}
		double w = 1D * getWidth() / viewPort.getAbsoluteWidth();
		double h = 1D * getHeight() / viewPort.getAbsoluteHeight();
		setScale(Math.min(w, h));
	}

	public void setScale(double scale) {
		Rectangle visible = internal.getVisibleRect();
		double factor = scale / this.scale;
		this.scale = Math.min(4, scale);
		redraw();
		visible = new Rectangle((int) (visible.x * factor),
				(int) (visible.y * factor), (int) (visible.width * factor),
				(int) (visible.height * factor));
		internal.scrollRectToVisible(visible);
		observable.setChanged();
		observable.notifyObservers(this);
	}

	public void redraw() {
		if (scrollPane != null) {
			// scrollingEnabled
			scrollPane.setViewportView(internal);
		}
	}

	public JScrollPane getScrollPane() {
		return scrollPane;
	}

	public void setViewportView(Zoomable viewPort) {
		this.viewPort = viewPort;
		if (viewPort != null) {
			viewPort.setZoomPane(this);
		}
		inheritListeners();
		redraw();
	}

	public double getScale() {
		return scale;
	}

	private JPanel internal = new JPanel() {

		public Dimension getPreferredSize() {
			if (viewPort == null) {
				return super.getPreferredSize();
			}
			return new Dimension((int) (viewPort.getAbsoluteWidth() * scale),
					(int) (viewPort.getAbsoluteHeight() * scale));
		}

		@Override
		public void setCursor(Cursor cursor) {
			root.setCursor(cursor);
		}

		@Override
		public String getToolTipText(MouseEvent e) {
			if (viewPort == null) {
				return "";
			}
			return viewPort.asJComponent().getToolTipText(translate(e));
		}

		@Override
		public void paintComponent(Graphics g) {
			Graphics2D g2d = (Graphics2D) g.create();
			g2d.scale(scale, scale);
			if (viewPort == null) {
				Rectangle rectangle = g2d.getClipBounds();
				g2d.setColor(Color.WHITE);
				g2d.fill(rectangle);
			} else {
				viewPort.paintComponent(g2d);
			}
			g2d.dispose();
		}
	};

	private MouseEvent translate(MouseEvent e) {
		int x = (int) (e.getX() / scale);
		int y = (int) (e.getY() / scale);

		return new MouseEvent((Component) e.getSource(), e.getID(),
				e.getWhen(), e.getModifiers(), x, y, e.getClickCount(), e
						.isPopupTrigger(), e.getButton());
	}

	public void mouseEntered(MouseEvent e) {
		for (MouseListener ml : mouseListeners) {
			ml.mouseEntered(translate(e));
		}
	}

	public void mouseExited(MouseEvent e) {
		for (MouseListener ml : mouseListeners) {
			ml.mouseExited(translate(e));
		}
	}

	public void mouseClicked(MouseEvent e) {

		for (MouseListener ml : mouseListeners) {
			ml.mouseClicked(translate(e));
		}
	}

	public void mousePressed(MouseEvent e) {

		for (MouseListener ml : mouseListeners) {
			ml.mousePressed(translate(e));
		}
	}

	public void mouseReleased(MouseEvent e) {
		for (MouseListener ml : mouseListeners) {
			ml.mouseReleased(translate(e));
		}
	}

	public void mouseDragged(MouseEvent e) {
		for (MouseMotionListener ml : mouseMotionListeners) {
			ml.mouseDragged(translate(e));
		}
	}

	public void mouseMoved(MouseEvent e) {
		for (MouseMotionListener ml : mouseMotionListeners) {
			ml.mouseMoved(translate(e));
		}
	}

	// Multiple inheritance, Java style:

	public Observable asObservable() {
		return observable;
	}

	private static class MyObservable extends Observable {
		public synchronized void setChanged() {
			super.setChanged();
		}
	}
}
