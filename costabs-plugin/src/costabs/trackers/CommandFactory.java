package costabs.trackers;

import java.util.ArrayList;
import java.util.Collection;

import costabs.beans.Command;

public class CommandFactory {

	public static String CONSOLE = "console";
	public static String HIGHLIGHT = "highlight";
	public static String MARKER = "marker";
	public static String GRAPH = "graph";
	public static String MARK_NODE = "mark_node";
	public static String MESSAGE = "message";
	public static String SELECTED = "selected";
	public static String CLEAN_MARKERS = "clean_markers";
	
	public static String TYPE_GRAPH = "graph";
	public static String TYPE_SOURCE = "source";
	
	
	public static CommandTracker getTracker (Command command) {
		String type = command.getType();
		if (CONSOLE.equals(type)) {
			return new ConsoleTracker (command); 
		}
		else if (HIGHLIGHT.equals(type)) {
			return new HighlightTracker (command); 
		}
		else if (MARKER.equals(type)) {
			return new MarkerTracker (command); 
		}
		else if (MESSAGE.equals(type)) {
			return new MessageTracker (command); 
		}
		else if (GRAPH.equals(type)) {
			return new GraphTracker (command); 
		}
		else {
			// TODO Lanzar un mensaje de error o una exception o algo... 
			return null;
		}
	}

	public static CommandTracker getInterTracker (Command command) {
		String type = command.getType();
		if (CONSOLE.equals(type)) {
			return new ConsoleTracker (command); 
		}
		else if (MESSAGE.equals(type)) {
			return new MessageTracker (command); 
		}
		else if (HIGHLIGHT.equals(type)) {
			return new InterHighlightTracker (command); 
		}
		else if (MARKER.equals(type)) {
			return new InterMarkerTracker (command); 
		}
		else if (MARK_NODE.equals(type)) {
			return new InterGraphTracker(command); 
		}
		else if (SELECTED.equals(type)) {
			return new InterHighlightTracker (command); 
		}
		else if (CLEAN_MARKERS.equals(type)) {
			return new CleanMarkerTracker(command); 
		}
		else {
			// TODO Lanzar un mensaje de error o una exception o algo... 
			return null;
		}
	}

	public static Collection<String> getMarkerIds () {
		ArrayList<String> ids = new ArrayList<String>();
		for (String s: HighlightTracker.getMarkers()) {
			ids.add(s);
		}
		for (String s: MarkerTracker.getMarkers()) {
			ids.add(s);
		}
		for (String s: InterMarkerTracker.getMarkers()) {
			ids.add(s);
		}
		for (String s: InterHighlightTracker.getMarkers()) {
			ids.add(s);
		}
		return ids;
	}
}
