package costabs.trackers;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;

import org.eclipse.core.resources.IFile;

import costabs.beans.Command;
import costabs.beans.Lines;
import costabs.beans.Line;
import costabs.exceptions.CostabsException;

public abstract class CommandTracker extends Command{
	
	private int [] linesInt; 
	
	private IFile iFile;
	
//	public static final String MARKER_HIGHLIGHT = "Costabs.plugin.highlight";
//	public static final String MARKER_HIGHLIGHT_WARN = "Costabs.plugin.highlightwarn";
//	public static final String MARKER_UB = "CostabsPlugin.costabs.marker";
//	public static final String MARKER_WARN = "CostabsPlugin.costabs.markerwarn";
//
//	protected static HashMap<String, String> MARKER_IDS;
//
//	{
//		MARKER_IDS = new HashMap<String, String>();
//		MARKER_IDS.put("info_marker", MARKER_UB);
//		MARKER_IDS.put("info_highlight", MARKER_HIGHLIGHT);
//
//		MARKER_IDS.put("warning_marker", MARKER_WARN);
//		MARKER_IDS.put("warning_highlight", MARKER_HIGHLIGHT_WARN);
//	}
	
	public CommandTracker(Command command) {
		this.setColor(command.getColor());
		this.setFile(command.getFile());
		this.setLevel(command.getLevel());
		this.setLines(command.getLines());
		this.setText(command.getText());
		this.setType(command.getType());
		this.setNodes(command.getNodes());
		
		fillLines(command.getLines());	
	}
	
	public int[] getLinesInt() {
		return linesInt;
	}

	public IFile getiFile() {
		return iFile;
	}

	public void setiFile(IFile iFile) {
		this.iFile = iFile;
	}
	
	private void fillLines (Lines lines) {
		if (lines == null) {
			return;
		}
		
//		String [] al = lines.split(",");
//		linesInt = new int [al.length];
//		
//		for(int i = 0; i < al.length; i ++) {
//			linesInt[i] = Integer.parseInt(al[i]);
//		}
		
		linesInt = new int [lines.getLines().size()];
		for(int i = 0; i < lines.getLines().size(); i++) {
			linesInt[i] = Integer.parseInt(lines.getLines().get(i).getL());
		}
		
	}

	public abstract void track () throws CostabsException ;

	public abstract void clean ();

	
}
