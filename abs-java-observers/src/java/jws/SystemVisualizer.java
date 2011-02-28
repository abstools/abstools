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

	final ObjectGraph graph;
	
	public SystemVisualizer() {
		 graph = new GraphStreamGraph();
	}
	
	protected SystemVisualizer(ObjectGraph graph) {
		this.graph = graph;
	}
	
	@Override
	public synchronized void taskCreated(TaskView task) {
		if (task.getSource() != null) {
			graph.addEdge(task.getSource(), task.getTarget());
		}
	}
	
	@Override
	public synchronized void newCOGCreated(COGView cog, ObjectView initialObject) {
		cog.registerObjectCreationListener(this);
      cog.getScheduler().registerTaskSchedulerObserver(this);
      
	   graph.addObject(initialObject, true);
	}
	
	@Override
	public synchronized void objectCreated(ObjectView o) {
	   graph.addObject(o, false);
	   //graph.addEdge(edgeCounter.incrementAndGet()+"edge", initialObjects.get(o.getCOG()), o, true);
	}

	@Override
	public synchronized void systemStarted() {
		graph.begin();
	}
	
	@Override
	public synchronized void systemFinished() {
		graph.end();
	}
	
}
