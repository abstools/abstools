package abs.api;

/**
 * @author Behrooz Nobakht
 * @since 1.0
 */
public interface ReferenceFactory {

	/**
	 * The name space of references.
	 */
	String NS = Actor.NS;

	/**
	 * The default {@link ReferenceFactory}.
	 */
	ReferenceFactory DEFAULT = new ReferenceFactory() {
		@Override
		public Reference create(String name) {
			final String uri = name.startsWith(NS) ? name : NS + name;
			return Reference.from(uri);
		}
	};

	/**
	 * The factory method.
	 * 
	 * @param name
	 *            the name with which the reference should be created
	 * @return the created reference
	 */
	Reference create(String name);

}
