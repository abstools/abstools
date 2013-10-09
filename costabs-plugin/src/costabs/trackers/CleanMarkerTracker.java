package costabs.trackers;

import java.util.ArrayList;
import java.util.Collection;

import costabs.beans.Command;
import costabs.exceptions.CostabsException;

public class CleanMarkerTracker extends CommandTracker{

	public CleanMarkerTracker(Command command) {
		super(command);
	}

	@Override
	public void track() throws CostabsException {
		OutputManager.getInstance().getOutputTracker(getiFile()).cleanMarkers();
	}

	@Override
	public void clean() {
	}

	public static Collection<String> getMarkers() {
		return new ArrayList<String>();
	}

}
