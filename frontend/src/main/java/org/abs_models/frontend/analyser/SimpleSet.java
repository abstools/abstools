package org.abs_models.frontend.analyser;

import java.util.Set;
import java.util.HashSet;

/**
 * A simple set for data flow analysis
 * @param <E> The elements type
 */
public class SimpleSet<E> extends HashSet<E> {

	/**
	 *
	 */
	private static final long serialVersionUID = 1L;

	public SimpleSet() {
		super();
	}

	public SimpleSet(E e) {
		super();
		add(e);
	}

    /**
     * Creates a union with `o`
     * @param o - The other set
     * @return A new union set
     */
	public SimpleSet<E> union(SimpleSet<E> o) {
		SimpleSet<E> s = empty();
		s.addAll(this);
		s.addAll(o);
		return s;
	}

    /**
     * Creates a new set with `e` added
     * @param e - The element to add
     * @return A new union set
     */
	public SimpleSet<E> union(E e) {
		SimpleSet<E> s = empty();
		s.addAll(this);
		s.add(e);
		return s;
	}

    /**
     * Creates an intersection with `o`
     * @param o - The other set
     * @return A new intersection set
     */
	public SimpleSet<E> intersection(SimpleSet<E> o) {
		SimpleSet<E> s = empty();
		for (E e : this) {
			if (o.contains(e)) {
				s.add(e);
			}
		}
		return s;
	}

    /**
     * Creates a new set only containing `e` if it is in this set
     * @param e - The element to intersect with
     * @return A new intersection set
     */
	public SimpleSet<E> intersection(E e) {
		SimpleSet<E> s = empty();
		if (contains(e)) {
			s.add(e);
		}
		return s;
	}

    /**
     * Creates a complement set with `o`
     * @param o - The other set
     * @return A new complement set
     */
	public SimpleSet<E> comp(SimpleSet<E> o) {
		SimpleSet<E> s = empty();
		s.addAll(this);
		s.removeAll(o);
		return s;
	}

    /**
     * Creates a new set with `e` removed
     * @param e - The element to remove
     * @return A new complement set
     */
	public SimpleSet<E> comp(E e) {
		SimpleSet<E> s = empty();
		s.addAll(this);
		s.remove(e);
		return s;
	}

    /**
     * A new empty set
     * @param <E> - The element type
     * @return An empty set
     */
	public static <E> SimpleSet<E> empty() {
		return new SimpleSet<>();
	}

	public String toString() {
		StringBuilder s = new StringBuilder("{");

		for (E e : this) {
			s.append(", ").append(e.toString());
		}

		return s + "}";
	}
}
