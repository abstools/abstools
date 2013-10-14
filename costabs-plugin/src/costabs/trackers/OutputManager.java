package costabs.trackers;

import java.io.File;
import java.util.HashMap;

import javax.swing.JScrollPane;

import org.eclipse.core.resources.IFile;

import costabs.awt.EmbeddedSwingComposite;
import costabs.beans.CostabsOutput;
import costabs.exceptions.CostabsException;
import costabs.panels.CostabsGraphPanel;
import costabs.panels.GraphPanelHandler;
import costabs.structures.CostabsConstants;
import costabs.structures.CostabsXMLFrontend;

public class OutputManager {

	private HashMap<IFile, OutputTracker> trackers;

	private static OutputManager manager;

	private GraphPanelHandler graphPanel;
	
	private CostabsGraphPanel panel;
	
	public OutputManager() {
		trackers = new HashMap<IFile, OutputTracker>();
	}
	
	public static OutputManager getInstance() {
		if (OutputManager.manager == null) {
			OutputManager.manager = new OutputManager();
		}
		return OutputManager.manager;
	}

	public void loadResults(IFile file) throws CostabsException {
		if (trackers.containsKey(file)) {
			OutputTracker tracker = trackers.get(file);
			if (tracker != null) {
				tracker.cleanTrack();
			}
		}
		File f = new File(CostabsConstants.OUTPUT_XML);
		CostabsOutput output = CostabsXMLFrontend.readOutput(f);
		trackers.put(file, new OutputTracker(file, output));
	}

	public void cleanAll (IFile file) throws CostabsException{
		OutputTracker tracker = trackers.get(file);
		if (tracker == null) {
			return;
		}
		tracker.cleanAllInfo();
		trackers.remove(file);
		updateView(file);
	}
	
	public OutputTracker getOutputTracker(IFile file) {
		return trackers.get(file);
	}

	public void setGraphPanel(GraphPanelHandler graphPanel) {
		this.graphPanel = graphPanel;
	}
	
	public void setPanel(CostabsGraphPanel panel) {
		this.panel = panel;
	}
	

	public void updateView(IFile file) {
		if (graphPanel != null) {
			graphPanel.showPanel(file);
			graphPanel.repaint();
		}
		if (panel != null) {
			panel.setFile(file);
			panel.repaint();
		}
	}

}
