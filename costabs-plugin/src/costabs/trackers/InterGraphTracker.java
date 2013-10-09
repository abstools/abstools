package costabs.trackers;

import java.util.ArrayList;
import java.util.Collection;

import com.kitfox.svg.SVGElementException;

import costabs.beans.Command;
import costabs.exceptions.CostabsException;
import costabs.structures.CostabsGraph;

public class InterGraphTracker extends CommandTracker{

	public InterGraphTracker(Command command) {
		super(command);
	}

	@Override
	public void track() throws CostabsException {
		OutputManager.getInstance().getOutputTracker(getiFile()).getGraph().interMarkNode(getNodes().getNodes(), CostabsGraph.COLOR_SOURCE_MARKED);
	}

	@Override
	public void clean() {
	}

	public static Collection<String> getMarkers() {
		return new ArrayList<String>();
	}

}
