package costabs.trackers;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;

import costabs.beans.Command;

public class InterMarkerTracker extends MarkerTracker {

	public static final String MARKER_HIGHLIGHT = "CostabsPlugin.costabs.marker.inter.info";
	public static final String MARKER_HIGHLIGHT_WARN = "CostabsPlugin.costabs.marker.inter.warn";
	
	public static HashMap<String,String> MARKER_IDS = new HashMap<String, String>();;
	
	{
		InterMarkerTracker.MARKER_IDS.put("info", MARKER_HIGHLIGHT);
		InterMarkerTracker.MARKER_IDS.put("warning", MARKER_HIGHLIGHT_WARN);
	}
	
	
	public InterMarkerTracker(Command command) {
		super(command);
	}
	
	public static Collection<String> getMarkers() {
		if (InterMarkerTracker.MARKER_IDS == null) {
			return new ArrayList<String>();
		}
		return InterMarkerTracker.MARKER_IDS.values();
	}

	@Override
	protected String getMarker (String level) {
		return InterMarkerTracker.MARKER_IDS.get(level);
	}


}
