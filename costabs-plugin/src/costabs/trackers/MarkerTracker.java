package costabs.trackers;

import java.io.File;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IMarker;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;

import costabs.beans.Command;
import costabs.exceptions.CostabsException;
import costabs.structures.CostabsConstants;

public class MarkerTracker extends CommandTracker{


	public static final String MARKER_UB = "CostabsPlugin.costabs.marker.info";
	public static final String MARKER_UB_WARN = "CostabsPlugin.costabs.marker.warn";

	protected static HashMap<String, String> MARKER_IDS;

	{	
		MARKER_IDS = new HashMap<String, String>();
		MARKER_IDS.put("info", MARKER_UB);
		MARKER_IDS.put("warning", MARKER_UB_WARN);
	}

	
	public MarkerTracker(Command command) {
		super(command);
	}

	@Override
	public void track() throws CostabsException {
		for(int i = 0; i < getLinesInt().length; i ++) {
			try {
				IMarker marker = this.getiFile().createMarker(getMarker(getLevel()));
				marker.setAttribute(IMarker.LINE_NUMBER, getLinesInt()[i]);
				//marker.setAttribute(IMarker.SEVERITY, getSeverity(getLevel()));
				//marker.setAttribute(IMarker.PRIORITY, IMarker.PRIORITY_HIGH);
				marker.setAttribute(IMarker.MESSAGE, getText());
			} catch (CoreException e) {
				throw new CostabsException(e.getMessage(),e);
			}
		}

	}

	protected String getMarker (String level) {
		return MarkerTracker.MARKER_IDS.get(level);
	}

	private int getSeverity (String level) {
		if (CostabsConstants.LEVEL_WARN.equals(level)) {
			return IMarker.SEVERITY_WARNING;
		}
		else {
			return IMarker.SEVERITY_INFO;
		}
	}

	@Override
	public void clean() {
		try {
			this.getiFile().deleteMarkers(MARKER_UB, false, IResource.DEPTH_INFINITE);
			this.getiFile().deleteMarkers(MARKER_UB_WARN, false, IResource.DEPTH_INFINITE);
		} catch (CoreException e) {
			e.printStackTrace();
		}
	}

	public static Collection<String> getMarkers() {
		if (MarkerTracker.MARKER_IDS == null) {
			return new ArrayList<String>();
		}
		return MarkerTracker.MARKER_IDS.values();
	}


	
	
}
