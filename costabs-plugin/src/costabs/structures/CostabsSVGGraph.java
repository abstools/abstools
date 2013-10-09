package costabs.structures;

import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Point;
import java.awt.RenderingHints;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.URI;
import java.net.URL;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map.Entry;
import java.util.TreeSet;

import javax.swing.JPanel;

import org.eclipse.core.resources.IFile;

import com.kitfox.svg.SVGDiagram;
import com.kitfox.svg.SVGElement;
import com.kitfox.svg.SVGElementException;
import com.kitfox.svg.SVGException;
import com.kitfox.svg.SVGUniverse;
import com.kitfox.svg.Title;
import com.kitfox.svg.animation.AnimationElement;
import com.kitfox.svg.app.beans.SVGIcon;

import costabs.beans.Command;
import costabs.beans.CostabsOutput;
import costabs.beans.Interaction;
import costabs.beans.Node;
import costabs.console.ConsoleHandler;
import costabs.exceptions.CostabsException;
import costabs.trackers.CommandFactory;
import costabs.trackers.CommandTracker;
import costabs.trackers.OutputManager;

public class CostabsSVGGraph extends CostabsGraph {

	private SVGIcon icon;

	private SVGDiagram diagram;

	private HashMap<String,String> idsTable;

	private Command command;

	private TreeSet<String> lastClicked = new TreeSet<String>();

	private IFile file = null;

	public void loadGraph (IFile file, Command command) throws CostabsException {
		this.command = command;
		this.file = file;
		//		FileReader reader;
		icon = new SVGIcon();
		icon.setScaleToFit(true);
		try {
			// Arghh!! It is needed to create a temporary file to to read all graphs from different files. 
			// Work-around to fix the problem of SVGSalamander while loading multiple graphs
			// using the same file name.
			File tmpfile = copy(this.command.getFile());

			SVGUniverse universe = new SVGUniverse();
			URI uri = universe.loadSVG(new URL("file://" + tmpfile.getAbsolutePath()));
			diagram = universe.getDiagram(uri);
			icon.setSvgURI(uri);

			idsTable = new HashMap<String, String> ();

			fillIdsTable(diagram.getRoot());
		} catch (Exception  e) {
			throw new CostabsException("Graph file cannot be found " + command.getFile());
		}
	}

	public int getWidth () {
		if (diagram != null) {
			return (int)diagram.getWidth();
		}
		return 0;
	}

	public int getHeight () {
		if (diagram != null) {
			return (int)diagram.getHeight();
		}
		return 0;
	}

//	public void paint(Graphics g, JPanel panel) {
//		System.out.println("Llamando de nuevo a pintar... ");
//		if (icon != null) {
////			g.setColor(panel.getBackground());
////			g.fillRect(0, 0, getWidth(), getHeight());
//			icon.paintIcon(panel, g, 0, 0);
//		}
//		//panel.setPreferredSize(new Dimension(getWidth(),getHeight()));
//	}
	
	
	public void paint(Graphics gg, JPanel panel) {

		Graphics2D g = (Graphics2D)gg;

		if (panel.getBackground() != null)
		{
			Dimension dim = panel.getSize();
			g.setColor(panel.getBackground());
			g.fillRect(0, 0, dim.width, dim.height);
		}
		g.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
		g.setRenderingHint(RenderingHints.KEY_RENDERING, RenderingHints.VALUE_RENDER_QUALITY);
		if (diagram != null) 
		{
			try
			{
				diagram.render(g);
			}
			catch (SVGException e)
			{
				e.printStackTrace();
			}
		}	
	}


	private void fillIdsTable (SVGElement element) {


		for (int i=0; i < element.getNumChildren(); i ++) {
			try {
				if (element.getChild(i) instanceof com.kitfox.svg.Title) {
					if (!element.hasAttribute("class", AnimationElement.AT_XML) || 
							"node".equals(element.getPresAbsolute("class").getStringValue()) ||
							"edge".equals(element.getPresAbsolute("class").getStringValue())){
						Title title = (Title)element.getChild(i);
						idsTable.put(title.getText(),element.getId());
					}
					//ids.add(title.getText());
				}

				SVGElement child = element.getChild(i);
				fillIdsTable(child);
			} catch (SVGElementException e) {
				e.printStackTrace();
			}
		}
	}

	public void cleanGraph () {
		cleanMarked();
		diagram = null;
		icon = null;
	}

	public void handleClick (int x, int y) throws CostabsException {
		List pickedElements;	
		try {
			pickedElements = diagram.pick(new Point(x,y),null);

			TreeSet<String> ids = new TreeSet<String>();

			// Clean markers...
			cleanMarked();
			// InteractionsTracker.cleanInterMarkers(OutputManager.getInstance().getFile());
			OutputManager.getInstance().getOutputTracker(file).cleanInteractions();

			// Getting elements clicked
			for (int i = 0; i < pickedElements.size(); i++) {
				ArrayList<SVGElement> elements = (ArrayList<SVGElement>)pickedElements.get(i);
				for (SVGElement element: elements) {
					if (element.getId() != null) {
						ids.add(element.getId());
					}
				}
			}

			// Mark nodes in the graph
			TreeSet<String> allNodes = new TreeSet<String>();
			for(String id: ids) {
				TreeSet<String> nodes = getNodesToMark (id);
				markNode(id, CostabsGraph.COLOR_SELECTED);
				for (String node: nodes) {
					markNode(node, CostabsGraph.COLOR_MARKED);
				}
				nodes.add(id);
				allNodes.addAll(nodes);

			}
			lastClicked = allNodes;
			System.out.println("All nodes " + allNodes);

			// Other commands like highlight's or markers

			for(String id: ids) {
				List<Command> commands = getCommandsToApply(id);
				for(Command command: commands) {
					if (CommandFactory.TYPE_GRAPH.equals(command.getType())) {
						continue;
					}
					CommandTracker tracker = CommandFactory.getInterTracker(command);
					tracker.setiFile(file);
					tracker.track();
				}
			}
		} catch (SVGException e) {
			throw new CostabsException("SVG file cannot be handled properly: " + e.getMessage(), e);
		}
	}

	public void cleanMarked () {
		for(String node: lastClicked) {
			try {
				markNode(node, CostabsGraph.COLOR_NO_SELECTED);
			} catch (SVGElementException e) {
				ConsoleHandler.write("Marked node " + node + " cannot be cleaned");
			}
			lastClicked = new TreeSet<String>();
		}
	}


	public void interMarkNode (List<Node> nodes, String color) throws CostabsException {

		for(Node n: nodes) {
			lastClicked.add(getSVGId(n.getId()));
			try {
				markNode(getSVGId(n.getId()), color);
				System.out.println("Marcando nodo " + n.getId());
			} catch (SVGElementException e) {
				e.printStackTrace();
			}
		}
	}

	private void markNode (String id, String color) throws SVGElementException {
		SVGElement element = diagram.getElement(id);
		System.out.println("Element vale " + element);
		if (element == null) {
			return;
		}
		if (!element.hasAttribute("class", AnimationElement.AT_XML) || 
			!"node".equals(element.getPresAbsolute("class").getStringValue())){
			return;
		}
		System.out.println("Al lio, tiene class " + element.getPresAbsolute("class").getStringValue());
		markNodeAux(element, color);
	}

	private void markNodeAux (SVGElement element, String color) throws SVGElementException {
		for(int i=0; i < element.getNumChildren(); i ++) {
			SVGElement child = element.getChild(i);
			if (child.hasAttribute("fill", AnimationElement.AT_XML)) {
				System.out.println("Cambiando color " + color + " " + child.getTagName() + " " + child.getPresAbsolute("stroke").getStringValue());
				child.setAttribute("fill", AnimationElement.AT_XML, color);
			}
			markNodeAux(child, color);
		}
	}


	private TreeSet<String> getNodesToMark (String id) throws SVGException {
		TreeSet<String> nodes = new TreeSet<String>();

		CostabsOutput output = OutputManager.getInstance().getOutputTracker(file).getOutput();

		SVGElement element = diagram.getElement(id);

		if (!element.hasAttribute("class", AnimationElement.AT_XML) || 
			(!"node".equals(element.getPresAbsolute("class").getStringValue()) && 
			 !"edge".equals(element.getPresAbsolute("class").getStringValue()))){
			return nodes;
		}

		List<Interaction> interactions = output.getInteractions().getInteractions();

		Node n = new Node();
		String idxml = getCostaId(id);
		n.setId(idxml);

		if (interactions == null || idxml == null) {
			return nodes;
		}
		for (Interaction interaction: interactions) {

			if (CommandFactory.TYPE_GRAPH.equals(interaction.getType()) && 
					interaction.getNodes() != null && 
					interaction.getNodes().getNodes().contains(n)) {
				for(Command command: interaction.getCommands().getCommands()) {
					if (CommandFactory.MARK_NODE.equals(command.getType())) {

						for(Node node: command.getNodes().getNodes()) {
							nodes.add(getSVGId(node.getId()));
							//nodes.add(node.getId());
						}

					}
				}
			}
		}

		return nodes; 

	}

	private ArrayList<Command> getCommandsToApply (String id) {
		ArrayList<Command> commands = new ArrayList<Command>();

		CostabsOutput output = OutputManager.getInstance().getOutputTracker(file).getOutput();

		List<Interaction> interactions = output.getInteractions().getInteractions();
		Node n = new Node();
		n.setId(getCostaId(id));

		if(interactions == null) {
			return commands;
		}
		for (Interaction interaction: interactions) {
			if (CommandFactory.TYPE_GRAPH.equals(interaction.getType()) && 
					interaction.getNodes() != null &&
					interaction.getNodes().getNodes().contains(n)) {
				commands.addAll(interaction.getCommands().getCommands());
			}	
		}

		return commands; 

	}


	public String getSVGId (String title){
		return idsTable.get(title);
	}

	private String getCostaId (String svgid){
		for(Entry<String, String> entry: idsTable.entrySet()) {
			if (svgid.equals(entry.getValue())) {
				return entry.getKey();
			}
		}
		return null;
	}

	/**
	 * Creates a temporary file with the content of the file <code>fo</code> 
	 * @param fo The original file name
	 * @return returns a temporary file with the content of <code>fo</code>
	 * @throws CostabsException If something goes wrong
	 */
	public static File copy (String fo) throws CostabsException {

		File f1 = new File(fo);
		File f2;
		try {
			f2 = File.createTempFile("pt_graph_fo", null, new File(CostabsConstants.TMP_DIR_SACO));
			InputStream in = new FileInputStream(f1);
			OutputStream out = new FileOutputStream(f2);

			byte[] buf = new byte[1024];
			int len;
			while ((len = in.read(buf)) > 0){
				out.write(buf, 0, len);
			}
			in.close();
			out.close();
			return f2;
		} catch (IOException e) {
			throw new CostabsException("Cannot copy the file " + fo + " to a temporary directory");
		}
	}
	
	@Override
	public String toString() {
		return "SVG Graph: " + file.getFullPath().toOSString();
	}

}
