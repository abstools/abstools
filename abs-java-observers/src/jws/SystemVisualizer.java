package jws;

import java.io.File;
import java.io.IOException;
import java.util.HashMap;
import java.util.Map;
import java.util.Random;
import java.util.concurrent.atomic.AtomicInteger;

import org.graphstream.stream.file.FileSink;
import org.graphstream.stream.file.FileSinkDGS;
import org.graphstream.stream.file.FileSinkDOT;
import org.graphstream.stream.file.FileSinkGML;
import org.graphstream.stream.file.FileSinkImages;
import org.graphstream.stream.file.FileSinkImages.OutputPolicy;
import org.graphstream.stream.file.FileSinkImages.OutputType;
import org.graphstream.stream.file.FileSinkImages.Resolution;
import org.graphstream.stream.file.FileSinkImages.Resolutions;
import org.graphstream.ui.swingViewer.Viewer;
import org.graphstream.ui.swingViewer.Viewer.CloseFramePolicy;

import abs.backend.java.lib.types.ABSValue;
import abs.backend.java.observing.COGView;
import abs.backend.java.observing.GuardView;
import abs.backend.java.observing.ObjectView;
import abs.backend.java.observing.RegistratingObserver;
import abs.backend.java.observing.TaskView;


public class SystemVisualizer extends RegistratingObserver {

	final MyObjectGraph graph = new MyObjectGraph();
	Viewer viewer = graph.display();
	
	{
		viewer.setCloseFramePolicy(CloseFramePolicy.EXIT);
		

	}
	
	final AtomicInteger edgeCounter = new AtomicInteger();
	
	@Override
	public synchronized void taskCreated(TaskView task) {
		if (task.getSource() != null) {
			graph.addEdge(edgeCounter.incrementAndGet()+"edge", task.getSource(), task.getTarget());
		}
	}
	
	@Override
	public synchronized void newCOGCreated(COGView cog, ObjectView initialObject) {
		cog.registerObjectCreationListener(this);
      cog.getScheduler().registerTaskSchedulerObserver(this);
      
	   graph.addObject(initialObject, true);
	}
	
	private String fileName;
	private FileSink fileSink;
	
	
	
	@Override
	public synchronized void objectCreated(ObjectView o) {
//	   super.objectCreated(o);

	   graph.addObject(o, false);
	   //graph.addEdge(edgeCounter.incrementAndGet()+"edge", initialObjects.get(o.getCOG()), o, true);
	}

	@Override
	public void systemStarted() {
		fileSink = new FileSinkDGS();
		graph.graph.addSink(fileSink);
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
	public synchronized void systemFinished() {
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
