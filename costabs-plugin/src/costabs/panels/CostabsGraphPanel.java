package costabs.panels;

import java.awt.Color;
import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.beans.Transient;

import javax.swing.JPanel;

import org.eclipse.core.resources.IFile;

import costabs.console.ConsoleHandler;
import costabs.dialogs.DialogPrinter;
import costabs.exceptions.CostabsException;
import costabs.trackers.OutputManager;

public class CostabsGraphPanel extends JPanel implements MouseListener {

	private static final long serialVersionUID = 1L;

	private IFile file;
	
	private int lastWidth = 0;
	private int lastHeight = 0;
	
	public CostabsGraphPanel(IFile file) {
		super();
		addMouseListener(this);
		this.file = file;
	}
	
	public void setFile(IFile file) {
		this.file = file;
		if (OutputManager.getInstance().getOutputTracker(file) != null && 
				OutputManager.getInstance().getOutputTracker(file).getGraph() != null) {
			if (OutputManager.getInstance().getOutputTracker(file).getGraph().getWidth() > lastWidth) {
				lastWidth = OutputManager.getInstance().getOutputTracker(file).getGraph().getWidth();
			}
			if (OutputManager.getInstance().getOutputTracker(file).getGraph().getHeight() > lastHeight){
				lastHeight = OutputManager.getInstance().getOutputTracker(file).getGraph().getHeight();
			}
		}
	}

	@Override
	public int getWidth() {
//		if (OutputManager.getInstance().getOutputTracker(file) != null && 
//			OutputManager.getInstance().getOutputTracker(file).getGraph() != null) {
//			return OutputManager.getInstance().getOutputTracker(file).getGraph().getWidth();
//		}
		return lastWidth;
	}
	
	@Override
	public int getHeight() {
//		if (OutputManager.getInstance().getOutputTracker(file) != null &&
//			OutputManager.getInstance().getOutputTracker(file).getGraph() != null) {
//			return OutputManager.getInstance().getOutputTracker(file).getGraph().getHeight();
//		}
		return lastHeight;
	}

	@Override
	@Transient
	public Color getBackground() {
		if (super.getBackground() != null) {
			return super.getBackground();
		}
		return Color.WHITE;
	}
	
	@Override
	public void paint(Graphics g) {
		g.setColor(getBackground());
		g.fillRect(0, 0, getWidth(), getHeight());

		if (OutputManager.getInstance().getOutputTracker(file) != null &&
				OutputManager.getInstance().getOutputTracker(file).getGraph() != null) {
			OutputManager.getInstance().getOutputTracker(file).getGraph().paint(g, this);
			if (OutputManager.getInstance().getOutputTracker(file).getGraph().getWidth() > lastWidth) {
				lastWidth = OutputManager.getInstance().getOutputTracker(file).getGraph().getWidth();
			}
			if (OutputManager.getInstance().getOutputTracker(file).getGraph().getHeight() > lastHeight){
				lastHeight = OutputManager.getInstance().getOutputTracker(file).getGraph().getHeight();
			}
		}
		
	}
	
	@Override
	public void mouseClicked(MouseEvent e) {
		try {
			if (OutputManager.getInstance().getOutputTracker(file) != null && 
					OutputManager.getInstance().getOutputTracker(file).getGraph() != null) {
				OutputManager.getInstance().getOutputTracker(file).getGraph().handleClick(e.getX(), e.getY());
			}
		} catch (CostabsException e1) {
			ConsoleHandler.write("Error handling mouseClicked event in the graph: " + e1.getMessage());
			DialogPrinter.logError(new CostabsException("Error handling mouseClicked event in the graph", e1));
		} catch (Exception e1) {
			ConsoleHandler.write("Error handling mouseClicked event in the graph: " + e1.getMessage());
			DialogPrinter.logError(new CostabsException("Error handling mouseClicked event in the graph", e1));
		}
		repaint();
	}

	@Override
	public void mouseEntered(MouseEvent e) {
	}

	@Override
	public void mouseExited(MouseEvent e) {
	}

	@Override
	public void mousePressed(MouseEvent e) {
	}

	@Override
	public void mouseReleased(MouseEvent e) {
	}
	
}
