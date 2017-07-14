package abs.frontend.pardef;

import abs.frontend.ast.ASTNode;
import abs.frontend.ast.VarUse;
import java.util.Collections;
import java.util.LinkedList;
import java.util.List;

public final class TreeUtil {

    private TreeUtil() {
    }

    public static <T extends ASTNode<?>> List<T> findChildren(ASTNode<?> root, Class<T> type) {
        List<T> result = new LinkedList<T>();
        findChildren(result, root, type);
        return Collections.unmodifiableList(result);
    }

    private static <T extends ASTNode<?>> void findChildren(java.util.List<T> list, ASTNode<?> node, Class<T> type) {
        if (node != null) {
            if (type.isInstance(node)) {
                list.add(type.cast(node));
            }

            for (int index = 0; index < node.getNumChildNoTransform(); ++index) {
                ASTNode<?> child;
                try {
                    child = node.getChildNoTransform(index);
                } catch (RuntimeException e) {
                    if (node instanceof VarUse) {
                        // TODO find out what causes this
                        // TODO change to continue;
                        throw e;
                    } else {
                        throw e;
                    }
                }
                findChildren(list, child, type);
            }
        }
    }

    public static <T extends ASTNode<?>> T findParent(ASTNode<?> node, Class<T> parentType) {
        if (parentType.isInstance(node)) {
            return parentType.cast(node);
        } else if (node == null) {
            return null;
        } else {
            return findParent(node.getParent(), parentType);
        }
    }

    public static void removeFromParent(ASTNode<?> node) {
        ASTNode<?> parent = node.getParent();
        parent.removeChild(parent.getIndexOfChild(node));
        node.setParent(null);
    }

    public static void replace(ASTNode<?> old, ASTNode<?> newNode) {
        ASTNode<?> parent = old.getParent();
        int index = parent.getIndexOfChild(old);
        parent.removeChild(index);
        parent.setChild(newNode, index);
    }

    public static boolean isAbsent(ASTNode<?> node, Class<? extends ASTNode<?>> type) {
        return findChildren(node, type).isEmpty();
    }
}
