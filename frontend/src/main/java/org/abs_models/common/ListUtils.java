/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. This file is licensed under the terms of the
 * Modified BSD License.
 */
package org.abs_models.common;

import org.abs_models.backend.java.lib.types.ABSValue;
import org.abs_models.backend.java.utils.DynamicClassUtils;
import org.abs_models.frontend.ast.ASTNode;
import org.abs_models.frontend.ast.List;

import java.util.ArrayList;
import java.util.HashSet;

public class ListUtils {

    @SuppressWarnings("rawtypes")
    public static <A extends ASTNode> java.util.List<A> toJavaList(List<A> argList) {
        ArrayList<A> res = new ArrayList<>(argList.getNumChild());
        for (A a : argList) {
            res.add(a);
        }
        return res;
    }

    /**
     * Creates an List from a java.util.List. All nodes in the java.util.List have to be detached
     * (parent == null).
     *
     * @param list a java list
     * @param <N> an ASTNode type
     * @return a List with the same elements as the passed java list.
     */
    public static <N extends ASTNode<?>> List<N> toASTList(java.util.List<N> list) {
        List<N> result = new List<>();
        for (N n : list) {
            if (n.getParent() != null) {
                throw new IllegalArgumentException("List contains attached node: " + n);
            }
            result.add(n);
        }
        return result;
    }

    /*
     * Transform a java.util.List into an ABS.StdLib.List
     */
    public static ABSValue toABSList(java.util.List<?> l) {
        if (l.isEmpty()) {
            return DynamicClassUtils.instance("ABS.StdLib.List_Nil");
        } else {
            ArrayList<Object> ml = new ArrayList<>(l); // make sure we can use remove()
            Object head = ml.remove(0);
            return DynamicClassUtils.instance("ABS.StdLib.List_Cons", head, toABSList(ml));
        }
    }

    /*
     * Transform a java.util.Set into an ABS.StdLib.Set
     */
    public static ABSValue toABSSet(java.util.Set<?> set) {
        if (set.isEmpty()) {
            return DynamicClassUtils.instance("ABS.StdLib.Set_EmptySet");
        } else {
            Object value = set.iterator().next();
            set.remove(value);
            return DynamicClassUtils.instance("ABS.StdLib.Set_Insert", value, toABSSet(set));
        }
    }

    /*
     * Transform a list of AST nodes into a java.util.Set
     */
    public static <T extends ASTNode<?>> java.util.Set<T> ASTListToSet(List<T> list) {
        java.util.Set<T> set = new HashSet<>();
        for (T element : list) {
            set.add(element);
        }
        return set;
    }


}
