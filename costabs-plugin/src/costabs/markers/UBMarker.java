package costabs.markers;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IMarker;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;

public class UBMarker {
	
	public static final String MARKER_UB = "CostabsPlugin.costabs.marker";

	/**
	 * Highlight a line in a source file, adding a marker.
	 * @param berf Source file.
	 * @param message Message to show in the marker.
	 * @param numLine Line number to highlight.
	 */
	public void markLine(IFile berf, String message, int numLine) {

		try {

			IMarker marker = berf.createMarker(MARKER_UB);

			
			marker.setAttribute(IMarker.LINE_NUMBER, numLine);
			marker.setAttribute(IMarker.SEVERITY, IMarker.SEVERITY_INFO);
			marker.setAttribute(IMarker.PRIORITY, IMarker.PRIORITY_HIGH);

			marker.setAttribute(IMarker.MESSAGE, message);

		} catch (CoreException e) {
			e.printStackTrace();
		}

	}
	
	/**
	 * Delete all trace markers in a file source. 
	 * @param berf The file source.
	 */
	public void removeAllMarkers(IFile berf) {
		try {
			berf.deleteMarkers(MARKER_UB, false, IResource.DEPTH_INFINITE);
		} catch (CoreException e) {
			e.printStackTrace();
		}
	}

}


