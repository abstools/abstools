package org.abs_models.frontend.typechecker.nullable;

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

	public static <E> SimpleSet<E> empty() {
		return new SimpleSet();
	}
}
