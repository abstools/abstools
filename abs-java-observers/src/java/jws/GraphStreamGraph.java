package jws;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.io.PrintWriter;
import java.util.HashMap;
import java.util.Map;
import java.util.Random;
import java.util.concurrent.atomic.AtomicInteger;

import org.graphstream.graph.Edge;
import org.graphstream.graph.Element;
import org.graphstream.graph.Graph;
import org.graphstream.graph.Node;
import org.graphstream.graph.implementations.MultiGraph;
import org.graphstream.stream.file.FileSink;
import org.graphstream.stream.file.FileSinkDGS;
import org.graphstream.stream.file.FileSinkGML;
import org.graphstream.ui.j2dviewer.J2DGraphRenderer;
import org.graphstream.ui.layout.Layout;
import org.graphstream.ui.layout.LayoutListener;
import org.graphstream.ui.layout.Layouts;
import org.graphstream.ui.swingViewer.GraphRenderer;
import org.graphstream.ui.swingViewer.Viewer;
import org.graphstream.ui.swingViewer.Viewer.CloseFramePolicy;
import org.graphstream.ui.swingViewer.ViewerPipe;
import org.graphstream.ui.swingViewer.basicRenderer.SwingBasicGraphRenderer;

import static org.graphstream.algorithm.Toolkit.*;

import abs.backend.java.observing.COGView;
import abs.backend.java.observing.ObjectView;

public class GraphStreamGraph extends AbstractObjectGraph {
    static {
       System.setProperty("gs.ui.renderer", "org.graphstream.ui.j2dviewer.J2DGraphRenderer");
    }
    
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
   			"}\n" +
   			"" +
   			"node { " +
   		            "       shape:rounded-box;" +
   		            "       size-mode:fit;" +
   	                     "      text-alignment:center;" +
   			"  padding:5;" +
//                        "  shadow-mode:gradient-radial;" +
//                        "  shadow-color:gray;" +
//                        "  shadow-width:105%;" +
//                        "  shadow-offset:2, 2;" +
                        "       stroke-color: gray;" +
                        "  stroke-width:2; " +
                        "  stroke-mode:plain;" +
//   		            "fill-mode: plain; " +
   			"  text-color:black;" +
   			"}\n" +
 	                "node.obj {" +
                        "       stroke-color: darkgray;" +
                        "  stroke-width:1; " +
                          "   fill-color: lightgreen; " +
 	                "}\n"+
   			"" +
   			"node.netnode {" +
   			"  size-mode:dyn-size;"+
   			"  size:100;" +
   		        "  z-index:0;" +
   			"  shape:box;" +
   			"  stroke-color:black;" +
   			"  fill-color:#eeeeee;" +
   			"}\n" +
   			""+
   			"node.cog {" +
   		            "  padding:8;" +
   			"	fill-color: lightblue;" +
   		            "  text-style:bold;" +
   			"" +
   			"}\n" +
   			"" +
   			"edge {" +
   			"	fill-color:black;" +
   			"	size:1;" +
   			"  arrow-shape:arrow;" +
   			"}\n" +
   		            "edge.usage {" +
   		            "  fill-color:darkred;" +
   		            "  size:3;" +
   		            "  arrow-shape:arrow;" +
   		            "}\n" +
   		            "" +
   	                     "edge.nodeown {" +
   	                     "  fill-color:#aa0000;" +
   	                     "  size:4;" +
   	                     "}\n" +
   	                     "" +
   			"" +
   			"edge.cog {" +
   			"	fill-color:lightgray;" +
   			"	size:8;" +
   			"  arrow-shape:none;" +
   			"}\n"
   			);

   }
   
   
   
	@Override
   public void addObject(final ObjectView o, final boolean COG) {
		super.addObject(o, COG);
		
		Node n = graph.addNode(getID(o));
		n.addAttribute("ui.label", getID(o));
		n.addAttribute("label", getLabel(o));
		n.addAttribute("cog", getID(getCOGOwner(o)));
		n.addAttribute("class", o.getClassName());
		if (COG) {
			n.addAttribute("layout.weight", 0.5f);
			n.addAttribute("ui.class", "cog");
			//n.addAttribute("ui.size", "25");
			n.addAttribute("Size", 20);
			n.addAttribute("Color", "[0,255,0]");
	      n.addAttribute("kind", "cog");
		} else {
			n.addAttribute("layout.weight", 0.5f);
		        n.addAttribute("ui.class", "obj");
			n.addAttribute("ui.size", "20");
			n.addAttribute("Size", 10);
			n.addAttribute("Color", "[255,0,0]");
         n.addAttribute("kind", "object");
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
   private Layout layout;
   private Viewer viewer;
	

	@Override
   public synchronized void addEdge(ObjectView source, ObjectView target, boolean cog) {
	   String edgeName = getID(source)+"-"+getID(target);
	   Edge e = graph.getEdge(edgeName);

	   
	   
	   if (e == null || 
	       (!cog && e.getAttribute("kind").equals("cog"))) {
	      e = graph.addEdge(edgeName,getID(source),getID(target),true);
         e.addAttribute("count", 1);
	      if (cog) {
	            e.addAttribute("ui.class", "cog");
	         e.addAttribute("kind", "cog");
	         e.addAttribute("weight", 5);
	      } else {
                 e.addAttribute("ui.class", "usage");
	         e.addAttribute("kind", "usage");
	         e.addAttribute("weight", 1);
	      }
	   } else {
	      int c = (Integer) e.getAttribute("count");
	      e.setAttribute("count", c+1);
	      System.out.println("Count = "+(c+1));
	   }
	}
	
	public Viewer display() {
		viewer = new Viewer(graph,
				Viewer.ThreadingModel.GRAPH_IN_ANOTHER_THREAD);
				GraphRenderer renderer = new J2DGraphRenderer();

				viewer.addView(
				String.format("defaultView_%d", (long) (Math.random() * 10000)),
				renderer);

				viewer.enableAutoLayout(layout);
				viewer.setCloseFramePolicy(CloseFramePolicy.EXIT);




				return viewer;
	}

	private String dgsFileName;
	private String filePrefix;
	private FileSink fileSink;
   private Node netnode;
	

      class MyLayoutListener implements LayoutListener {

         @Override
         public void edgeChanged(String arg0, double[] arg1) {
            // TODO Auto-generated method stub
            
         }

         @Override
         public void edgesChanged(Map<String, double[]> arg0) {
            // TODO Auto-generated method stub
            
         }

         @Override
         public void nodeInfos(String arg0, double arg1, double arg2, double arg3) {
             System.out.println("Node infos");
         }

         @Override
         public void nodeMoved(String node, double arg1, double arg2, double arg3) {
             Node n = graph.getNode(node);
             double[] pos = nodePosition(n);
             double[] netpos = nodePosition(netnode);
             double size = netnode.getNumber("ui.size");
             netnode.setAttribute("ui.size", ""+(size+10));
             System.out.println("Node moved");
         }

         @Override
         public void nodesMoved(Map<String, double[]> arg0) {
            // TODO Auto-generated method stub
            
         }

         @Override
         public void stepCompletion(double arg0) {
            // TODO Auto-generated method stub
            
         }
          
      }
   
	@Override
	public void begin() {
           layout = Layouts.newLayoutAlgorithm();
	   viewer = graph.display();
	   viewer.enableAutoLayout(layout);
	   netnode = graph.addNode("NetNode1");
           netnode.addAttribute("ui.class", "netnode");
           netnode.addAttribute("ui.label", "Node 1");
           netnode.addAttribute("ui.size", "100");
           layout.addListener(new MyLayoutListener());
           layout.freezeNode("NetNode1", true);
           
           ViewerPipe fromViewer = viewer.newViewerPipe();
//           fromViewer.addViewerListener(this);
           fromViewer.addSink(graph);
		fileSink = new FileSinkDGS();
		graph.addSink(fileSink);
		Random r = new Random();
		filePrefix = "graph"+r.nextInt(Integer.MAX_VALUE);
      dgsFileName = filePrefix+ ".dgs";
      try {
	      fileSink.begin(dgsFileName);
      } catch (IOException e) {
	      // TODO Auto-generated catch block
	      e.printStackTrace();
      }
   }

	@Override
   public void end() {
		try {
	      fileSink.end();
	      File f = new File(dgsFileName);
	      System.out.println("DGS Graph written to "+f.toURI().toURL().toExternalForm());
	      MyGMLWriter mw = new MyGMLWriter();
         File file = new File(filePrefix+".gml");
	      mw.writeAll(graph, file);
         System.out.println("GML Graph written to "+file.toURI().toURL().toExternalForm());
      } catch (IOException e) {
	      e.printStackTrace();
      }
   }
}


class MyGMLWriter {
   private PrintWriter pw;

   public synchronized void writeAll(Graph g, File file) throws FileNotFoundException {
      pw = new PrintWriter(new FileOutputStream(file));
      
      pw.println("Creator "+escape("ABS Graph Writer"));
      pw.println("graph [");
      
      for (Node n : g.getNodeSet()) {
         writeNode(n);
      }
      
      for (Edge e : g.getEdgeSet()) {
         writeEdge(e);
      }
      
      pw.println("]");
      
      pw.close();
      pw = null;
   }
   
   private void writeEdge(Edge e) {
      pw.println("\t edge [");
      pw.println("\t\t source "+e.getSourceNode().getId());
      pw.println("\t\t target "+e.getTargetNode().getId());
      pw.println("\t\t weight "+e.getAttribute("weight"));
      pw.println("\t\t count "+e.getAttribute("count"));
      pw.println("\t\t kind "+escape((String)e.getAttribute("kind")));
      pw.println("\t ]");
      
   }

   private void writeNode(Node n) {
      pw.println("\t node [");
      pw.println("\t\t id "+n.getId());
      pw.println("\t\t label "+escape((String)n.getAttribute("label")));
      pw.println("\t\t kind "+escape((String)n.getAttribute("kind")));
      pw.println("\t\t class "+escape((String)n.getAttribute("class")));
      pw.println("\t\t cog "+n.getAttribute("cog"));
      pw.println("\t ]");
   }

   public static String escape(String s) {
      return "\"" + s + "\"";
   }
}

