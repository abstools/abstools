package jws;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.Graphics2D;
import java.awt.RenderingHints;
import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.atomic.AtomicInteger;

import javax.swing.JFrame;
import javax.swing.JScrollPane;

import org.graphstream.graph.Edge;
import org.graphstream.graph.Graph;
import org.graphstream.graph.Node;
import org.graphstream.graph.implementations.*;
import org.graphstream.ui.layout.Layout;
import org.graphstream.ui.layout.Layouts;
import org.graphstream.ui.swingViewer.GraphRenderer;
import org.graphstream.ui.swingViewer.Viewer;
import org.graphstream.ui.swingViewer.Viewer.CloseFramePolicy;
import org.graphstream.ui.swingViewer.basicRenderer.SwingBasicGraphRenderer;

import abs.backend.java.observing.COGView;
import abs.backend.java.observing.ObjectView;

class MyObjectGraph {
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
   
	private final Map<COGView, ObjectView> initialObjects = new HashMap<COGView, ObjectView>();
	final AtomicInteger edgeCounter = new AtomicInteger();
   
	
	public void addObject(final ObjectView o, final boolean COG) {

		if (COG) {
			initialObjects.put(o.getCOG(),o);
		} 
		
		Node n = graph.addNode(getID(o));
		n.addAttribute("ui.label", getID(o));
		n.addAttribute("Label", getID(o));
		n.addAttribute("abs.cog", getID(initialObjects.get(o.getCOG())));
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
			addEdge("cogEdge"+edgeCounter.incrementAndGet(), o,initialObjects.get(o.getCOG()), true);
		}
		
		step();
	}
	
	private void step() {
//		try {
//	      Thread.sleep(1000);
//      } catch (InterruptedException e) {
//	      // TODO Auto-generated catch block
//	      e.printStackTrace();
//      }
	}

	private String getID(ObjectView o) {
	   return o.getClassName()+" "+o.getID();
   }
	
	public void addObject(ObjectView o) {
		addObject(o,false);
	}
	
	public void addEdge(String s, ObjectView source, ObjectView target) {
		addEdge(s,source,target,false);
	}

	public void addEdge(String s, ObjectView source, ObjectView target, boolean cog) {
		Edge e = graph.addEdge(s,getID(source),getID(target),true);
		if (cog) {
			e.addAttribute("ui.class", "cog");
			e.addAttribute("layout.weight", 0.1f);
			e.addAttribute("Weight", 5);
			
		} else {
			e.addAttribute("layout.weight", 0.5f);
			e.addAttribute("Weight", 5);
			
		}
		step();

	}
	
	public Viewer display() {
		Viewer viewer = new Viewer(graph,
				Viewer.ThreadingModel.GRAPH_IN_ANOTHER_THREAD);
				GraphRenderer renderer = new MyRenderer();

				viewer.addView(
				String.format("defaultView_%d", (long) (Math.random() * 10000)),
				renderer);

				Layout layout = Layouts.newLayoutAlgorithm();
				viewer.enableAutoLayout(layout);

				return viewer;
	}
}


class MyRenderer extends SwingBasicGraphRenderer {
	protected void renderGraph(Graphics2D g) {
		super.renderGraph(g);

   }
}

