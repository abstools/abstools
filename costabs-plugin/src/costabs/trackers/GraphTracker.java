package costabs.trackers;

import java.io.File;
import java.util.ArrayList;
import java.util.Collection;

import costabs.beans.Command;
import costabs.console.ConsoleHandler;
import costabs.exceptions.CostabsException;
import costabs.structures.CostabsSVGGraph;

public class GraphTracker extends CommandTracker{

	private Command command;
	
	public GraphTracker(Command command) {
		super(command);
		this.command = command;
	}

	@Override
	public void track() throws CostabsException {
		File f = new File (getFile());
		if (!f.exists()) {
			ConsoleHandler.write("warning","WARN: File " + getFile() + " does not exist");
			return;
		}
		
		OutputManager.getInstance().getOutputTracker(getiFile()).createGraph(command);
	}

	@Override
	public void clean() {
	}

	public static Collection<String> getMarkers() {
		return new ArrayList<String>();
	}

}
