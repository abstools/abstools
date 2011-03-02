package jws;

import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.atomic.AtomicInteger;

import abs.backend.java.observing.COGView;
import abs.backend.java.observing.ObjectView;

public abstract class AbstractObjectGraph implements ObjectGraph {

	final Map<COGView, ObjectView> initialObjects = new HashMap<COGView, ObjectView>();
	final Map<ObjectView, Integer> objectIDs = new HashMap<ObjectView, Integer>();
	final AtomicInteger edgeCounter = new AtomicInteger();
   final AtomicInteger nodeCounter = new AtomicInteger();
   
	
	
	public AbstractObjectGraph() {
		super();
	}

	protected String getID(ObjectView o) {
	   Integer id = objectIDs.get(o);
	   if (id == null) {
	      id = nodeCounter.incrementAndGet();
	      objectIDs.put(o,id);
	   }
      return String.valueOf(id);
   }

   protected String getLabel(ObjectView o) {
      return o.getClassName()+" "+o.getID();
   }
	
	@Override
   public void addObject(ObjectView o) {
   	addObject(o,false);

	}

	@Override
   public void addObject(ObjectView o, boolean COG) {
		if (COG) {
			initialObjects.put(o.getCOG(),o);
		} 
	}

	
	@Override
   public void addEdge(ObjectView source, ObjectView target) {
   	addEdge(source,target,false);
   }

	protected ObjectView getCOGOwner(final ObjectView o) {
      return initialObjects.get(o.getCOG());
   }

}