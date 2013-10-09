package costabs.trackers;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;

import org.eclipse.core.resources.IMarker;

import costabs.beans.Command;

public class InterHighlightTracker extends HighlightTracker {

	public static final String MARKER_INTER_HIGHLIGHT = "CostabsPlugin.costabs.marker.inter.highlight";
	public static final String MARKER_INTER_SELECTED = "CostabsPlugin.costabs.marker.selected";
	public static final String MARKER_INTER_HIGHLIGHT_WARN = "CostabsPlugin.costabs.marker.inter.highlightwarn";
	
	public static HashMap<String, String> MARKER_IDS = new HashMap<String, String>();

	{
		InterHighlightTracker.MARKER_IDS.put("info", MARKER_INTER_HIGHLIGHT);
		InterHighlightTracker.MARKER_IDS.put("warning", MARKER_INTER_HIGHLIGHT_WARN);
		InterHighlightTracker.MARKER_IDS.put("selected", MARKER_INTER_SELECTED);
	}
	
		
	public InterHighlightTracker(Command command) {
		super(command);
	}
	
	public static Collection<String> getMarkers() {
		if (InterHighlightTracker.MARKER_IDS == null) {
			return new ArrayList<String>();
		}
		return InterHighlightTracker.MARKER_IDS.values();
	}

	@Override
	protected String getMarker (String level) {
		return InterHighlightTracker.MARKER_IDS.get(level);
	}
	
}
