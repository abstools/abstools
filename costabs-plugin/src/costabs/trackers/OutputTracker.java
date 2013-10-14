package costabs.trackers;

import java.awt.Graphics;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.TreeSet;

import javax.swing.JPanel;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;

import costabs.beans.Command;
import costabs.beans.CostabsOutput;
import costabs.beans.Interaction;
import costabs.beans.Interactions;
import costabs.beans.Line;
import costabs.beans.Node;
import costabs.console.ConsoleHandler;
import costabs.exceptions.CostabsException;
import costabs.structures.CostabsGraph;
import costabs.structures.CostabsSVGGraph;
import costabs.utils.SourceUtils;

public class OutputTracker {

	private CostabsOutput output;

	private IFile file;

	private TreeSet<Integer> linesSet;

	private ArrayList<CommandTracker> trackers = new ArrayList<CommandTracker>();
	
	private CostabsGraph graph;

	public OutputTracker(IFile file, CostabsOutput output) {
		super();
		this.output = output;
		this.file = file;
		setLines ();
	}

	public void setFile (IFile file) {
		this.file = file;

	}
	public void cleanTrack () {
		if (trackers == null) {
			file = null;
			return;
		}
		for(CommandTracker tracker: trackers) {
			tracker.clean();
		}
		trackers = null; 
		file = null; 
	}

	public void trackResults () {
		trackers = new ArrayList<CommandTracker>();
		List<Command> commands = output.getCommands().getCommands();

		if (commands == null) {
			return;	
		}

		for(Command command: commands) {
			CommandTracker tracker = CommandFactory.getTracker(command);
			try {
				tracker.setiFile(file);
				tracker.track();
				trackers.add(tracker);
			}
			catch (CostabsException e1) {
				if (tracker.getFile() != null) {
					ConsoleHandler.write(ConsoleHandler.ERROR, "ERROR: while reading the file " + tracker.getFile());
				}
				else {
					ConsoleHandler.write(ConsoleHandler.ERROR, "ERROR: while printing the marker with text " + tracker.getText() + " and level " + tracker.getLevel());
				}
			}
		}
	}

	public void cleanInteractions () throws CostabsException {
		try {
			for(String s: InterMarkerTracker.getMarkers()) {
				file.deleteMarkers(s, false, IResource.DEPTH_INFINITE);
			}
			for(String s: InterHighlightTracker.getMarkers()) {
				file.deleteMarkers(s, false, IResource.DEPTH_INFINITE);
			}
			if (graph != null) {
				graph.cleanMarked();
			}
		}
		catch (CoreException e) {
			throw new CostabsException ("An error has ocurred while cleaning the information: " + e.getMessage());
		}
	}

	public void cleanMarkers () throws CostabsException {
		try {
			IFile file = SourceUtils.getActiveFile();
			Collection<String> markers = CommandFactory.getMarkerIds();
			for (String s: markers) {
				file.deleteMarkers(s, false, IResource.DEPTH_INFINITE);
			}
		}
		catch (CoreException e) {
			throw new CostabsException ("An error has ocurred while cleaning the information: " + e.getMessage());
		}
	}

	public void cleanAllInfo () throws CostabsException {
		cleanMarkers();
		cleanInteractions();
		linesSet = new TreeSet<Integer>();
		if (graph != null) {
			graph.cleanGraph();
		}
	}

	public CostabsOutput getOutput () {
		return output;
	}

	public IFile getFile () {
		return file;
	}

	public TreeSet<Integer> getLinesSet() {
		return linesSet;
	}

	private void setLines () {
		linesSet = new TreeSet<Integer>();
		Interactions interactions = output.getInteractions();
		if (interactions == null || interactions.getInteractions() == null){
			return;
		}
		for(Interaction inter: interactions.getInteractions()){
			if (!CommandFactory.TYPE_SOURCE.equals(inter.getType())) {
				continue;
			}
			List<Line> lines = inter.getLines().getLines();

			for(Line l: lines) {
				linesSet.add(Integer.parseInt(l.getL()));
			}
		}
	}
	
	
	public CostabsGraph getGraph () {
		return graph;
	}
	
	public void createGraph (Command command) throws CostabsException{
		graph = new CostabsSVGGraph();
		graph.loadGraph(file,command);
	}

}
