package abs.api;

import java.net.URI;
import java.util.IdentityHashMap;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;

/**
 * An in-memory implementation of Notary. Note that this implementation
 * uses {@link java.util.IdentityHashMap} to implement
 * {@link #get(Reference)}. As part of the life cycle, the local
 * registry is wiped when this instance is stopped.
 *
 * @see Notary
 * @author Behrooz Nobakht
 * @since 1.0
 */
public class LocalNotary implements Notary {

	private final ConcurrentMap<URI, Object> refs = new ConcurrentHashMap<>(8192);
	private final IdentityHashMap<Object, Reference> ids = new IdentityHashMap<>(8192);

	/** {@inheritDoc} */
	@Override
	public void add(Reference reference, Object target) throws RuntimeException {
		Object oldValue = refs.putIfAbsent(key(reference), target);
		if (oldValue != null) {
			throw new IllegalArgumentException("Duplicate reference: " + reference);
		}
		ids.put(target, reference);
	}

	/** {@inheritDoc} */
	@Override
	public boolean remove(Reference reference) {
		Object oldValue = refs.remove(key(reference));
		return oldValue != null;
	}

	/** {@inheritDoc} */
	@Override
	public Object get(Reference reference) {
		return refs.get(key(reference));
	}

	/** {@inheritDoc} */
	@Override
	public Reference get(Object object) {
		return ids.get(object);
	}

	@Override
	public Reference identify(Reference reference) {
		URI key = key(reference);
		if (refs.containsKey(key)) {
			return ids.get(refs.get(key));
		}
		return null;
	}

	/**
	 * <p>
	 * key.
	 * </p>
	 *
	 * @param ref
	 *            a {@link abs.api.Reference} object.
	 * @return a {@link java.net.URI} object.
	 */
	protected URI key(Reference ref) {
		return ref.name();
	}

	/** {@inheritDoc} */
	@Override
	public void stop() throws Exception {
		refs.clear();
		ids.clear();
	}

	/** {@inheritDoc} */
	@Override
	public long size() {
		return refs.size();
	}

}
