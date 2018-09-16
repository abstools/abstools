package org.abs_models.common;

import org.abs_models.frontend.ast.ASTNode;

import java.util.List;
import java.util.Objects;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.stream.Collectors;
import java.util.stream.IntStream;
import java.util.stream.Stream;

public final class TreeUtilsHelper {

	private TreeUtilsHelper() {

	}

	/**
	 * Finds all children of this node with the specified <code>type</code>. If this node has the
	 * specified type, it will be included in the result.
	 *
	 * @param type the type to search for
	 * @return an unmodifiable list of nodes, never null
	 * @throws NullPointerException if <code>type</code> is null
	 */
	public static <N> java.util.List<N> findChildren(ASTNode<?> node, Class<N> type) {
		return findChildren(node, type, false);
	}

	/**
	 * Finds all children of this node with the specified <code>type</code>. If this node has the
	 * specified type, it will be included in the result.
	 *
	 * @param type the type to search for
	 * @param lazy if true, children of found N nodes will not be included
	 * @return an unmodifiable list of nodes, never null
	 * @throws NullPointerException if <code>type</code> is null
	 */
	public static <N> java.util.List<N> findChildren(ASTNode<?> node, Class<N> type, boolean lazy) {
		return findChildren(node, cast(type), n -> !lazy).collect(Collectors.toList());
	}

	/**
	 * Creates a Function which casts ASTNode instances of the given type, or returns null for incompatible nodes.
	 *
	 * @param type the desired type
	 * @param <N> the returned type
	 * @return a cast function
	 */
	public static <N> Function<Object, N> cast(Class<N> type) {
		Objects.requireNonNull(type);
		return node -> type.isInstance(node) ? type.cast(node) : null;
	}

	/**
	 * Creates a function which casts an ASTNode to any of the given types, or returns null if none match.
	 *
	 * @param types a list of types
	 * @param <N> a common supertype of all given types
	 * @return a cast function
	 */
	public static <N> Function<Object, N> cast(List<Class<? extends N>> types) {
		Objects.requireNonNull(types);
		return node -> types.stream()
				.filter(t -> t.isInstance(node))
				.findAny()
				.map(t -> t.cast(node))
				.orElse(null);
	}

	/**
	 * Creates a predicate that matches any of the given types
	 *
	 * @param recurseTypes a list of types
	 * @param <N> a common supertype of the given types
	 * @return a predicate matching nodes of any of the given types
	 */
	public static <N> Predicate<N> recurse(List<Class<? extends N>> recurseTypes) {
		return node -> recurseTypes.stream().anyMatch(type -> type.isInstance(node));
	}

	/**
	 * Lazily finds children of this node which can be casted by the cast function.
	 * The root node will also be visited.
	 *
	 * <p>The cast function receives an ASTNode and tries to cast it to the desired return type.
	 * If the node can't be casted, it returns null.
	 * See the convenience functions {@link #cast(Class)} and {@link #cast(List)}.
	 * </p>
	 *
	 * <p>The recurse predicate receives a node casted by the cast function and decides
	 * whether to recurse over the nodes children.
	 * See the convenience function {@link #recurse(List)}.
	 * </p>
	 *
	 * @param node the root node for the search
	 * @param cast a function receiving an ASTNode and casts to N, or returns null
	 * @param recurse if the cast function could cast a node, this predicate decides whether to recurse over the casted node's children
	 * @param <N> the type of nodes in the returned stream
	 * @return a stream of nodes
	 */
	public static <N> Stream<N> findChildren(
			ASTNode<?> node,
			Function<? super ASTNode<?>, N> cast,
			Predicate<? super N> recurse) {
		if (node != null) {
			N casted = cast.apply(node);
			Stream<N> current = null;
			if (casted != null) {
				current = Stream.of(casted);
				if (!recurse.test(casted)) {
					return current;
				}
			}

			Stream<N> children = IntStream.range(0, node.getNumChildNoTransform())
					.mapToObj(node::getChildNoTransform)
					.flatMap(n -> findChildren(n, cast, recurse));
			if (current == null) {
				return children;
			} else {
				return Stream.concat(current, children);
			}
		}
		return Stream.empty();
	}
}
