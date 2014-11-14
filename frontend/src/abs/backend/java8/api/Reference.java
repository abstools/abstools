package abs.api;

import java.io.Serializable;
import java.io.UnsupportedEncodingException;
import java.net.URI;
import java.net.URLDecoder;
import java.net.URLEncoder;

/**
 * A reference encapsulate a serializable and comparable
 * {@linkplain URI uri} with a name. Since {@link URI} is a direct
 * dependency in this interface, all restrictions and requirements are
 * applied to any implementation.
 * 
 * @author Behrooz Nobakht
 * @since 1.0
 */
public interface Reference extends Serializable, Comparable<Reference> {

	/**
	 * The name of this reference.
	 * 
	 * @return the name of this reference
	 */
	URI name();

	/**
	 * Compares this reference to another reference using
	 * {@link #name()} of the reference.
	 * 
	 * @return the result of the comparison of {@code this} with the
	 *         provided reference. The same semantics as default
	 *         {@link Comparable} holds here.
	 */
	@Override
	default int compareTo(Reference o) {
		return name().compareTo(o.name());
	}

	/**
	 * Encode an instance of a reference using {@link URLEncoder}.
	 * 
	 * @param ref
	 *            the reference to be encoded.
	 * @return the encoded reference
	 * @throws UnsupportedEncodingException
	 *             see {@link URLEncoder#encode(String, String)}
	 */
	public static String encode(Reference ref) throws UnsupportedEncodingException {
		return URLEncoder.encode(ref.name().toASCIIString(), "UTF-8");
	}

	/**
	 * Decode an instance of reference with provided text using
	 * {@link URLDecoder}.
	 * 
	 * @param text
	 *            the string to be decoded
	 * @return an instance of a {@link Reference}
	 * @throws UnsupportedEncodingException
	 *             see {@link URLDecoder#decode(String, String)}
	 */
	public static Reference decode(String text) throws UnsupportedEncodingException {
		return Reference.from(URLDecoder.decode(text, "UTF-8"));
	}

	/**
	 * Creates an instance of reference using the provided name.
	 * 
	 * @param name
	 *            the name of the reference
	 * @return the reference created with name
	 */
	static Reference from(final String name) {
		return new Reference() {

			private static final long serialVersionUID = 1L;

			private final URI uri = URI.create(name);

			@Override
			public URI name() {
				return uri;
			}

			@Override
			public String toString() {
				return uri.toASCIIString();
			}

			@Override
			public int hashCode() {
				return uri.hashCode();
			}
		};
	}
}
