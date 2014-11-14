package abs.api;

import java.lang.reflect.Method;
import java.net.URI;
import java.util.Comparator;

/**
 * A method reference encapsulates a method invocation as a general
 * interface. A method invocation should have the following behavior:
 * <ul>
 * <li>the name of the method that is exposed by {@link #name()} of
 * this method reference.
 * <li>the owner object of the method is a reference exposed by
 * {@link #owner()}.
 * <li>the actual parameters of the method
 * </ul>
 *
 * @see Reference
 * 
 * @author Behrooz Nobakht
 * @since 1.0
 */
public interface MethodReference extends Reference {

	/**
	 * Exposes a reference to the object that has a method as
	 * specified by this method reference.
	 *
	 * @return the reference as the object that has a method with the
	 *         name of this method reference.
	 */
	Reference owner();

	/**
	 * The arguments with which the method should be invoked. This
	 * allows type checking of the actual parameters of the method.
	 *
	 * @return the actual parameters of this method invocation
	 */
	Object[] args();

	/**
	 * A default comparator for an instance of {@link MethodReference}
	 * that identifies two method reference as equal based on their
	 * {@link Method} signature.
	 */
	static Comparator<MethodReference> COMPARATOR = (m1, m2) -> {
		if (m1.owner().compareTo(m2.owner()) != 0) {
			return 0;
		}
		return m1.name().compareTo(m2.name());
	};

	/**
	 * A helper method to create an instance of method reference. The
	 * creation does not perform any checks or validations on the
	 * owner or the arguments.
	 *
	 * @param owner
	 *            the reference to the object that has the method.
	 * @param name
	 *            the name of the method in the owner
	 * @param args
	 *            the actual parameters of the method invocation
	 * @return an instance of {@link abs.api.MethodReference}
	 */
	static MethodReference of(Reference owner, String name, Object... args) {
		return new SimpleMethodReference(name, owner, args);
	}

	/**
	 * A default simple implementation of {@link MethodReference}.
	 */
	static class SimpleMethodReference implements MethodReference {

		private static final long serialVersionUID = -4362935717861176059L;

		private final URI name;
		private final Reference owner;
		private final Object[] args;

		public SimpleMethodReference(String name, Reference owner, Object... args) {
			this.name = URI.create(name);
			this.owner = owner;
			this.args = args;
		}

		@Override
		public URI name() {
			return name;
		}

		@Override
		public Reference owner() {
			return owner;
		}

		@Override
		public Object[] args() {
			return args;
		}

	}

}
