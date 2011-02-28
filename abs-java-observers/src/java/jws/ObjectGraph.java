package jws;

import abs.backend.java.observing.ObjectView;

public interface ObjectGraph {

	/* (non-Javadoc)
	 * @see jws.ObjectGraph2#addObject(abs.backend.java.observing.ObjectView, boolean)
	 */
	public abstract void addObject(final ObjectView o, final boolean COG);

	/* (non-Javadoc)
	 * @see jws.ObjectGraph2#addObject(abs.backend.java.observing.ObjectView)
	 */
	public abstract void addObject(ObjectView o);

	/* (non-Javadoc)
	 * @see jws.ObjectGraph2#addEdge(java.lang.String, abs.backend.java.observing.ObjectView, abs.backend.java.observing.ObjectView)
	 */
	public abstract void addEdge(ObjectView source, ObjectView target);

	/* (non-Javadoc)
	 * @see jws.ObjectGraph2#addEdge(java.lang.String, abs.backend.java.observing.ObjectView, abs.backend.java.observing.ObjectView, boolean)
	 */
	public abstract void addEdge(ObjectView source,
	      ObjectView target, boolean cog);

	public abstract void begin();

	public abstract void end();

}