package abs.frontend.pardef;

import abs.frontend.ast.ASTNode;
import java.util.Collections;
import java.util.LinkedList;
import java.util.List;
import java.util.Objects;

public final class TreeUtil {

    private TreeUtil() {
    }

    /**
     * Finds all children of <code>root</code> with the specified <code>type</code>. If <code>root</code> has the
     * specified type, it will be included in the result.
     *
     * @param root a node to use as a root for searching
     * @param type the type to search for
     * @param <T> the child <code>type</code>
     * @return an unmodifiable list of nodes, never null
     * @throws NullPointerException if <code>type</code> is null
     */
    public static <T extends ASTNode<?>> List<T> findChildren(ASTNode<?> root, Class<T> type) {
        List<T> result = new LinkedList<>();
        findChildren(result, root, Objects.requireNonNull(type));
        return Collections.unmodifiableList(result);
    }

    private static <T extends ASTNode<?>> void findChildren(java.util.List<T> list, ASTNode<?> node, Class<T> type) {
        if (node != null) {
            if (type.isInstance(node)) {
                list.add(type.cast(node));
            }

            for (int index = 0; index < node.getNumChildNoTransform(); ++index) {
                findChildren(list, node.getChildNoTransform(index), type);
            }
        }
    }

    /**
     * Finds the nearest parent of the specified node with the specified type. If the specified node is of the searched
     * type, it will be returned.
     *
     * @param node a node
     * @param parentType the type to search for
     * @param <T> the <code>parentType</code>
     * @return the nearest parent of <code>node</code> with type <code>parentType</code>, or null
     * @throws NullPointerException if parentType is null
     */
    public static <T extends ASTNode<?>> T findParent(ASTNode<?> node, Class<T> parentType) {
        if (parentType.isInstance(node)) {
            return parentType.cast(node);
        } else if (node == null) {
            return null;
        } else {
            return findParent(node.getParent(), parentType);
        }
    }

    /**
     * Removes the specified node from its parent.
     *
     * @param node a node
     * @throws NullPointerException if the node is null
     */
    public static void removeFromParent(ASTNode<?> node) {
        ASTNode<?> parent = Objects.requireNonNull(node).getParent();
        parent.removeChild(parent.getIndexOfChild(node));
        node.setParent(null);
    }

    /**
     * Replaces the <code>oldNode</code> with the <code>newNode</code> in the parent node of <code>oldNode</code>.
     *
     * @param oldNode a node to replace
     * @param newNode a node to insert instead
     * @throws NullPointerException if either argument is null
     * @throws IllegalArgumentException if <code>oldNode</code> is detached or <code>newNode</code> is not
     */
    public static void replace(ASTNode<?> oldNode, ASTNode<?> newNode) {
        Objects.requireNonNull(oldNode);
        Objects.requireNonNull(newNode);
        ASTNode<?> parent = oldNode.getParent();
        if (parent == null || newNode.getParent() != null) {
            throw new IllegalArgumentException();
        }
        int index = parent.getIndexOfChild(oldNode);
        parent.setChild(newNode, index);
        oldNode.setParent(null);
    }

    /**
     * Determines whether any nodes of the specified type (or subtype thereof) are present in the tree with the
     * specified root node.
     *
     * @param node a root node
     * @param type a type of ASTNode
     * @return true if no nodes of the specified type are present
     * @throws NullPointerException if either argument is null
     */
    public static boolean isAbsent(ASTNode<?> node, Class<? extends ASTNode<?>> type) {
        Objects.requireNonNull(node);
        Objects.requireNonNull(type);
        // TODO could be implement more efficiently with fail-fast behaviour
        return findChildren(node, type).isEmpty();
    }
}
