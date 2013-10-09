package costabs.panels;

import java.awt.Graphics;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;

import javax.swing.JPanel;

import org.eclipse.core.resources.IFile;

import costabs.console.ConsoleHandler;
import costabs.dialogs.DialogPrinter;
import costabs.exceptions.CostabsException;
import costabs.trackers.OutputManager;

public class CostabsGraphPanel extends JPanel implements MouseListener {

	private static final long serialVersionUID = 1L;

	private IFile file;
	
	public CostabsGraphPanel(IFile file) {
		super();
		addMouseListener(this);
		this.file = file;
	}
	
	public void setFile(IFile file) {
		this.file = file;
	}

	@Override
	public int getWidth() {
		if (OutputManager.getInstance().getOutputTracker(file) != null && 
			OutputManager.getInstance().getOutputTracker(file).getGraph() != null) {
			return OutputManager.getInstance().getOutputTracker(file).getGraph().getWidth();
		}
		return 0;
	}
	
	@Override
	public int getHeight() {
		if (OutputManager.getInstance().getOutputTracker(file) != null &&
			OutputManager.getInstance().getOutputTracker(file).getGraph() != null) {
			return OutputManager.getInstance().getOutputTracker(file).getGraph().getHeight();
		}
		return 0;
	}

	@Override
	public void paint(Graphics g) {
		if (OutputManager.getInstance().getOutputTracker(file) != null) {
			OutputManager.getInstance().getOutputTracker(file).getGraph().paint(g, this);
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
