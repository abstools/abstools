package jws;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Dimension;
import java.lang.reflect.InvocationTargetException;

import javax.swing.BorderFactory;
import javax.swing.JFrame;
import javax.swing.JPanel;
import javax.swing.SwingUtilities;

import abs.backend.java.observing.ObjectView;
import edu.uci.ics.jung.algorithms.layout.AbstractLayout;
import edu.uci.ics.jung.algorithms.layout.FRLayout;
import edu.uci.ics.jung.algorithms.layout.FRLayout2;
import edu.uci.ics.jung.algorithms.layout.ISOMLayout;
import edu.uci.ics.jung.algorithms.layout.KKLayout;
import edu.uci.ics.jung.algorithms.layout.Layout;
import edu.uci.ics.jung.algorithms.layout.StaticLayout;
import edu.uci.ics.jung.algorithms.layout.util.Relaxer;
import edu.uci.ics.jung.algorithms.layout.util.VisRunner;
import edu.uci.ics.jung.algorithms.util.IterativeContext;
import edu.uci.ics.jung.graph.DirectedGraph;
import edu.uci.ics.jung.graph.DirectedSparseGraph;
import edu.uci.ics.jung.graph.DirectedSparseMultigraph;
import edu.uci.ics.jung.visualization.BasicVisualizationServer;
import edu.uci.ics.jung.visualization.VisualizationViewer;
import edu.uci.ics.jung.visualization.decorators.ToStringLabeller;

import edu.uci.ics.jung.visualization.layout.LayoutTransition;
import edu.uci.ics.jung.visualization.renderers.Renderer;
import edu.uci.ics.jung.visualization.renderers.VertexLabelAsShapeRenderer;
import edu.uci.ics.jung.visualization.util.Animator;


public class JungObjectGraph extends AbstractObjectGraph implements
      ObjectGraph {

	@Override
	public void addEdge(ObjectView source, ObjectView target, boolean cog) {
		graph.addEdge(edgeCounter.incrementAndGet(), getID(source), getID(target));
		vv.repaint();
	}
	
	@Override
	public void addObject(ObjectView o, boolean COG) {
	   super.addObject(o, COG);
	   
	   layout.initialize();
	   startAnimator();
	   vv.repaint();
	}

	private void startAnimator() {
	   Relaxer relaxer = new VisRunner((IterativeContext)layout);
      relaxer.stop();
      relaxer.prerelax();
      StaticLayout<String,Integer> staticLayout =
         new StaticLayout<String,Integer>(graph, layout);
         LayoutTransition<String,Integer> lt =
                 new LayoutTransition<String,Integer>(vv, vv.getGraphLayout(),
                                 staticLayout);
         Animator animator = new Animator(lt);
         animator.start();
   }

	private DirectedGraph<String, Integer> graph;
	private VisualizationViewer<String, Integer> vv;
	private AbstractLayout<String, Integer> layout;
	
	@Override
	public void begin() {
		graph = new DirectedSparseGraph<String, Integer>();

		layout = new FRLayout<String,Integer>(graph);
		layout.setSize(new Dimension(600,600)); 
		vv = new VisualizationViewer<String,Integer>(layout, new Dimension(600,600));
		vv.setBackground(Color.white);

		startAnimator();
		
		VertexLabelAsShapeRenderer<String,Integer> vlasr = new VertexLabelAsShapeRenderer<String,Integer>(vv.getRenderContext());

//		vv.getRenderContext().setVertexShapeTransformer(arg0)
		vv.getRenderer().getVertexLabelRenderer().setPosition(Renderer.VertexLabel.Position.CNTR);


		vv.getRenderContext().setVertexLabelTransformer(new ToStringLabeller());
  	   vv.getRenderContext().setVertexShapeTransformer(vlasr);

		//vv.setPreferredSize(new Dimension(450,450)); //Sets the viewing area size
		
		try {
	      SwingUtilities.invokeAndWait(new Runnable() {
	      	@Override
	      	public void run() {
	      		JFrame frame = new JFrame("Simple Graph View");
	      		frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
	      		JPanel pane = new JPanel();
	      		pane.setLayout(new BorderLayout());
	      		pane.add(vv, BorderLayout.CENTER);
	      		frame.getContentPane().add(pane);
	      		pane.setBorder(BorderFactory.createEmptyBorder(50,50,50,50));
	      		
	      		frame.pack();
	      		frame.setVisible(true);
	      		
	      	}
	      });
      } catch (InterruptedException e) {
	      e.printStackTrace();
      } catch (InvocationTargetException e) {
	      e.printStackTrace();
      }
		

	}

	@Override
	public void end() {
	}

	
}
