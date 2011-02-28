package jws;

import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.atomic.AtomicInteger;

import abs.backend.java.observing.COGView;
import abs.backend.java.observing.ObjectView;

public abstract class AbstractObjectGraph implements ObjectGraph {

	final Map<COGView, ObjectView> initialObjects = new HashMap<COGView, ObjectView>();
	final AtomicInteger edgeCounter = new AtomicInteger();
   
	
	
	public AbstractObjectGraph() {
		super();
	}

	protected String getID(ObjectView o) {
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