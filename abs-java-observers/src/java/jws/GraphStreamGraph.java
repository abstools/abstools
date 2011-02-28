package jws;

import java.io.File;
import java.io.IOException;
import java.util.HashMap;
import java.util.Map;
import java.util.Random;
import java.util.concurrent.atomic.AtomicInteger;

import org.graphstream.graph.Edge;
import org.graphstream.graph.Graph;
import org.graphstream.graph.Node;
import org.graphstream.graph.implementations.MultiGraph;
import org.graphstream.stream.file.FileSink;
import org.graphstream.stream.file.FileSinkDGS;
import org.graphstream.ui.layout.Layout;
import org.graphstream.ui.layout.Layouts;
import org.graphstream.ui.swingViewer.GraphRenderer;
import org.graphstream.ui.swingViewer.Viewer;
import org.graphstream.ui.swingViewer.Viewer.CloseFramePolicy;
import org.graphstream.ui.swingViewer.basicRenderer.SwingBasicGraphRenderer;

import abs.backend.java.observing.COGView;
import abs.backend.java.observing.ObjectView;

public class GraphStreamGraph extends AbstractObjectGraph {
   final Graph graph = new MultiGraph("Tutorial 1",true,true);
   {
   	graph.addAttribute("ui.antialias");
   	graph.addAttribute("ui.quality");
   	graph.setAttribute("layout.force", 0.5f);
   	graph.setAttribute("layout.quality", 3);
   	graph.addAttribute("ui.stylesheet", 
   			"graph {" +
   			"   padding:50;" +
   			"   fill-color:white;" +
   			"}" +
   			"" +
   			"node { " +
   			"	fill-color: darkgreen; " +
   			"	stroke-color:blue; " +
   			"	stroke-width:2; " +
   			"	stroke-mode:plain;" +
   			"  padding:20;" +
   			"  size-mode: dyn-size;" +
   			"  shadow-mode:gradient-radial;" +
   			"  shadow-color:darkgray,gray;" +
   			"  shadow-width:105%;" +
   			"  shadow-offset:5;" +
   			"  text-style:bold;" +
   			"  text-color:black;" +
   			"}" +
   			"" +
   			"node.cog {" +
   			"	fill-color: darkred;" +
   			"" +
   			"}" +
   			"" +
   			"edge {" +
   			"	fill-color:black;" +
   			"	size:1;" +
   			"  arrow-shape:arrow;" +
   			"}" +
   			"" +
   			"edge.cog {" +
   			"	fill-color:darkgray;" +
   			"	size:8;" +
   			"  arrow-shape:none;" +
   			"}");

   }
   
	@Override
   public void addObject(final ObjectView o, final boolean COG) {
		super.addObject(o, COG);
		
		Node n = graph.addNode(getID(o));
		n.addAttribute("ui.label", getID(o));
		n.addAttribute("Label", getID(o));
		n.addAttribute("abs.cog", getID(getCOGOwner(o)));
		n.addAttribute("abs.class", o.getClassName());
		if (COG) {
			n.addAttribute("layout.weight", 0.5f);
			n.addAttribute("ui.class", "cog");
			n.addAttribute("ui.size", "25");
			n.addAttribute("Size", 20);
			n.addAttribute("Color", "[0,255,0]");
		} else {
			n.addAttribute("layout.weight", 0.5f);
			n.addAttribute("ui.size", "20");
			n.addAttribute("Size", 10);
			n.addAttribute("Color", "[255,0,0]");
		}
		
		if (o.equals("Main 1")) {
			n.addAttribute("ui.hide");
			System.out.println("YES");
		}
		
		if (!COG) {
			addEdge(o,getCOGOwner(o), true);
		}
		
	}

	
	final AtomicInteger edgeCounter = new AtomicInteger();
	

	@Override
   public void addEdge(ObjectView source, ObjectView target, boolean cog) {
		Edge e = graph.addEdge("edge-"+edgeCounter.incrementAndGet(),getID(source),getID(target),true);
		if (cog) {
			e.addAttribute("ui.class", "cog");
			e.addAttribute("layout.weight", 0.1f);
			e.addAttribute("Weight", 5);
			
		} else {
			e.addAttribute("layout.weight", 0.5f);
			e.addAttribute("Weight", 5);
			
		}
	}
	
	public Viewer display() {
		Viewer viewer = new Viewer(graph,
				Viewer.ThreadingModel.GRAPH_IN_ANOTHER_THREAD);
				GraphRenderer renderer = new SwingBasicGraphRenderer();

				viewer.addView(
				String.format("defaultView_%d", (long) (Math.random() * 10000)),
				renderer);

				Layout layout = Layouts.newLayoutAlgorithm();
				viewer.enableAutoLayout(layout);
				viewer.setCloseFramePolicy(CloseFramePolicy.EXIT);
				


				return viewer;
	}

	private String fileName;
	private FileSink fileSink;
	

	@Override
	public void begin() {
		fileSink = new FileSinkDGS();
		graph.addSink(fileSink);
		Random r = new Random();
      fileName = "graph"+r.nextInt(Integer.MAX_VALUE)+ ".dgs";
      try {
	      fileSink.begin(fileName);
      } catch (IOException e) {
	      // TODO Auto-generated catch block
	      e.printStackTrace();
      }
   }

	@Override
   public void end() {
		try {
	      fileSink.end();
	      File f = new File(fileName);
	      System.out.println("Graph written to "+f.toURI().toURL().toExternalForm());
      } catch (IOException e) {
	      // TODO Auto-generated catch block
	      e.printStackTrace();
      }
   }
}


