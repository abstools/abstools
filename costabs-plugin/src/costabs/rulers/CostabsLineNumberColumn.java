package costabs.rulers;


import java.util.ArrayList;
import java.util.List;
import java.util.TreeSet;

import org.eclipse.core.resources.IFile;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.text.source.CompositeRuler;
import org.eclipse.jface.text.source.LineNumberRulerColumn;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.events.MouseListener;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Display;
import org.eclipse.ui.plugin.AbstractUIPlugin;

import costabs.beans.Command;
import costabs.beans.Interaction;
import costabs.beans.Line;
import costabs.beans.Lines;
import costabs.console.ConsoleHandler;
import costabs.dialogs.DialogPrinter;
import costabs.exceptions.CostabsException;
import costabs.structures.CostabsConstants;
import costabs.structures.CostabsSVGGraph;
import costabs.trackers.CommandFactory;
import costabs.trackers.CommandTracker;
import costabs.trackers.OutputManager;
import costabs.trackers.OutputTracker;
import costabs.utils.SourceUtils;


public class CostabsLineNumberColumn extends LineNumberRulerColumn {

	private static String ICON_FILE = "icons/costabs_rule_marker.png";

//	private boolean visible = false;

//	private List<Interaction> interactions;

//	private TreeSet<Integer> linesSet;
	
	private LineNumberListener listener;
	
	public static Image COLUMN_IMAGE; 

	{
		try {
			ImageDescriptor image = AbstractUIPlugin.imageDescriptorFromPlugin(CostabsConstants.PLUGIN_ID, CostabsLineNumberColumn.ICON_FILE);
			COLUMN_IMAGE = image.createImage();
		} catch (Exception e){
			DialogPrinter.logError(new Exception(CostabsLineNumberColumn.ICON_FILE + " cannot be found: " + e.getMessage(), e));
		}
	}

	public CostabsLineNumberColumn() {
	}

	@Override
	protected String createDisplayString(int line) {
		return "";
	}

	@Override
	protected int computeNumberOfDigits() {
		//return super.computeNumberOfDigits() + 1;
		return 2;
	}

	/*
	 * @see org.eclipse.jface.text.source.IVerticalRulerInfo#getLineOfLastMouseButtonActivity()
	 */
	public int getLineOfLastMouseButtonActivity() {
		return getParentRuler().getLineOfLastMouseButtonActivity();
	}

//	@Override
//	protected void paintLine(int line, int y, int lineheight, GC gc,
//			Display display) {
//		super.paintLine(line, y, lineheight, gc, display);
//	}

	@Override
	protected void paintLine(int line, int y, int lineheight, GC gc, Display display) {
		int l = line + 1;
		IFile activeFile = SourceUtils.getActiveFile();
		
		OutputTracker tracker = OutputManager.getInstance().getOutputTracker(activeFile);
		listener.setFile(activeFile);
		if (tracker != null) {
			TreeSet<Integer> linesSet = tracker.getLinesSet();
			
			if (linesSet.contains(l)) {
				gc.drawImage(CostabsLineNumberColumn.COLUMN_IMAGE, 0, y);
			}
			
		}
		
		super.paintLine(line, y, lineheight, gc, display);
	}

	@Override
	public Control createControl(CompositeRuler parentRuler,
			Composite parentControl) {
		Control c = super.createControl(parentRuler, parentControl);
		listener = new LineNumberListener(this);
		c.addMouseListener(listener);
		return c;

	}

}

class LineNumberListener implements MouseListener {

	private CostabsLineNumberColumn column;
	
	private IFile file;

	LineNumberListener (CostabsLineNumberColumn column) {
		this.column = column;
	}

	public void setFile(IFile file) {
		this.file = file;
	}

	@Override
	public void mouseUp(MouseEvent e) {}

	@Override
	public void mouseDown(MouseEvent e) {}

	@Override
	public void mouseDoubleClick(MouseEvent e) {
		int line = column.getLineOfLastMouseButtonActivity() + 1;
		
		IFile activeFile = SourceUtils.getActiveFile();
		OutputTracker tracker = OutputManager.getInstance().getOutputTracker(activeFile);
		
		if (tracker == null) {
			return;
		}
		
		TreeSet<Integer> linesSet = tracker.getLinesSet();
		

		if (!linesSet.contains(line)) {
			return;
		}

		for (Interaction inter: tracker.getOutput().getInteractions().getInteractions()) {
			
			if (inter.getLines() == null) {
				continue;
			}

			boolean found = false;
			for(Line ll: inter.getLines().getLines()) {
				if (Integer.parseInt(ll.getL()) == line) {
					found = true;
				}
			}

			if (!found) {
				continue;
			}
			
			if (!Interaction.NO_CLEAN_INTERACTION_VALUE.equals(inter.getClean())) {
				try {
					tracker.cleanInteractions();
				} catch (CostabsException e1) {
					ConsoleHandler.write("warning", "ERROR: There was a problem trying to clean previous markers");
				}
			}

			List<Command> commands = inter.getCommands().getCommands();
			
			// Command to highlight the line selected  
			Command c = new Command ();
			c.setType(CommandFactory.SELECTED);
			ArrayList<Line> ls = new ArrayList<Line>();
			Line l = new Line ();
			l.setL(String.valueOf(line));
			ls.add(l);
			Lines lss = new Lines();
			lss.setLines(ls);
			c.setLines(lss);
			c.setLevel(CommandFactory.SELECTED);
			commands.add(c);
			
			for(Command command: commands) {
				CommandTracker ctracker = CommandFactory.getInterTracker(command);
				try {
					ctracker.setiFile(activeFile);
					ctracker.track();
				}
				catch (CostabsException e1) {
					ConsoleHandler.write("warning", "ERROR: Marker with text '" + ctracker.getText() + "' of type " + ctracker.getType() + "cannot be printed: " + e1.getMessage());
				}
			}
		}
		
		OutputManager.getInstance().updateView(file);

	}
}
