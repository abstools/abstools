package org.abs_models.frontend.analyser;

import java.util.Set;
import java.util.HashSet;

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

	public SimpleSet union(SimpleSet<E> o) {
		SimpleSet<E> s = empty();
		s.addAll(this);
		s.addAll(o);
		return s;
	}

	public SimpleSet union(E e) {
		SimpleSet<E> s = empty();
		s.addAll(this);
		s.add(e);
		return s;
	}

	public SimpleSet intersection(SimpleSet<E> o) {
		SimpleSet<E> s = empty();
		for (E e : this) {
			if (o.contains(e)) {
				s.add(e);
			}
		}
		return s;
	}

	public SimpleSet intersection(E e) {
		SimpleSet<E> s = empty();
		if (contains(e)) {
			s.add(e);
		}
		return s;
	}

	public SimpleSet comp(SimpleSet<E> o) {
		SimpleSet<E> s = empty();
		s.addAll(this);
		s.removeAll(o);
		return s;
	}

	public SimpleSet comp(E e) {
		SimpleSet<E> s = empty();
		s.addAll(this);
		s.remove(e);
		return s;
	}

	public static <E> SimpleSet<E> empty() {
		return new SimpleSet();
	}

	public String toString() {
		String s = "{";

		for (E e : this) {
			s += ", " + e.toString();
		}

		return s + "}";
	}
}
