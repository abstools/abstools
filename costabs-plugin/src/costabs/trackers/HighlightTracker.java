package costabs.trackers;

import java.io.BufferedReader;
import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.StringReader;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;

import org.eclipse.core.resources.IMarker;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;

import costabs.beans.Command;
import costabs.exceptions.CostabsException;

public class HighlightTracker extends CommandTracker{


	public static final String MARKER_HIGHLIGHT = "CostabsPlugin.costabs.marker.highlight";
	public static final String MARKER_HIGHLIGHT_WARN = "CostabsPlugin.costabs.marker.highlightwarn";

	protected static HashMap<String, String> MARKER_IDS;

	{
		MARKER_IDS = new HashMap<String, String>();
		MARKER_IDS.put("info", MARKER_HIGHLIGHT);
		MARKER_IDS.put("warning", MARKER_HIGHLIGHT_WARN);
	}

	public HighlightTracker(Command command) {
		super(command);
	}

	@Override
	public void track() throws CostabsException {
		String content;
		try {
			InputStream input = getiFile().getContents();
			char [] contentArr = new char[input.available()]; 
			BufferedReader br = new BufferedReader(new InputStreamReader(input));
			br.read(contentArr);
			content = new String (contentArr);
			br.close();
//			String [] lines = content.split("\n");
//			for(int i = 0; i < lines.length; i++) {
//				System.out.println(i + ":" + lines[i]);
//			}

		}
		catch (Exception e) {
			throw new CostabsException("Cannot read the source file " + getiFile().getFullPath().toOSString(), e);
		}

		for(int i = 0; i < getLinesInt().length; i ++) {
			try {

				IMarker marker = this.getiFile().createMarker(getMarker(getLevel()));
				marker.setAttribute(IMarker.LINE_NUMBER, getLinesInt()[i]);
				setChars(marker, content, getLinesInt()[i]);
				//		marker.setAttribute(IMarker.MARKER, IMarker.);
				//		marker.setAttribute(IMarker.SEVERITY, IMarker.SEVERITY_WARNING);
				//		marker.setAttribute(IMarker.PRIORITY, IMarker.PRIORITY_HIGH);
				marker.setAttribute("colorPreferenceValue", "126,12,32");
				marker.setAttribute(IMarker.PRIORITY, IMarker.PRIORITY_HIGH);
				marker.setAttribute(IMarker.MESSAGE, getText());

			} catch (CoreException e) {
				throw new CostabsException (e.getMessage(), e);
				//			}	 catch (IOException e) {
				//				throw new CostabsException (e.getMessage(), e);
			}
		}
	}

	@Override
	public void clean() {
		try {
			this.getiFile().deleteMarkers(MARKER_HIGHLIGHT, false, IResource.DEPTH_INFINITE);
			this.getiFile().deleteMarkers(MARKER_HIGHLIGHT_WARN, false, IResource.DEPTH_INFINITE);
		} catch (CoreException e) {

		}

	}

	protected String getMarker (String level) {
		return HighlightTracker.MARKER_IDS.get(level);
	}

	/**
	 * Set the start and end number char in the file as the marker limits.
	 * @param m Marker in which to add the limits.
	 * @param br File source.
	 * @param numLine Line number to highlight.
	 */
	private void setChars(IMarker m, String content, int numLine) throws CostabsException {

		try {

			String [] lines = content.split("\n");
			int start = 0;
			int end = -1;
			int i = 0;
			
			do {
				start = end + 1;
				end = end + 1 + lines[i].length();
				i++;
			} while ((i < numLine) && (i < lines.length));

			m.setAttribute(IMarker.CHAR_START, start);
			m.setAttribute(IMarker.CHAR_END, end);
			//		} catch (IOException e) {
			//			e.printStackTrace();
		} catch (CoreException e) {

		}		
	}

	/**
	 * Get number of chars in the indentation of the line.
	 * @param line Line to parse.
	 * @return The number of chars in the indentation.
	 */
	private int indentation(String line) {

		if (line == null || line.equals("")) return 0;

		int i = 0;
		char c = line.charAt(i);

		while ((i < line.length()) && ((c == ' ') || (c == '\t'))) {
			i++;
			if (i < line.length())
				c = line.charAt(i);
		}

		// Check if a line-comment starts
		if ((c == '/') && (i + 1 < line.length())) {
			c = line.charAt(i + 1);
			if (c == '/') 
				return line.length();
		}

		return i;
	}

	public static Collection<String> getMarkers() {
		if (HighlightTracker.MARKER_IDS == null) {
			return new ArrayList<String>();
		}
		return HighlightTracker.MARKER_IDS.values();
	}

}
