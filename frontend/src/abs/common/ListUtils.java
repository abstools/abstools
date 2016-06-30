/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved.
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.common;

import java.util.ArrayList;
import java.util.HashSet;

import abs.backend.java.lib.types.ABSValue;
import abs.backend.java.utils.DynamicClassUtils;
import abs.frontend.ast.ASTNode;
import abs.frontend.ast.List;

public class ListUtils {

    @SuppressWarnings("rawtypes")
    public static <A extends ASTNode> java.util.List<A> toJavaList(List<A> argList) {
        ArrayList<A> res = new ArrayList<A>(argList.getNumChild());
        for (A a : argList) {
            res.add(a);
        }
        return res;
    }

    /*
     * Transform a java.util.List into an ABS.StdLib.List
     */
    public static ABSValue toABSList(java.util.List<? extends ABSValue> l) {
        if (l.isEmpty()) {
            return DynamicClassUtils.instance("ABS.StdLib.List_Nil");
        } else {
            ArrayList<ABSValue> ml = new ArrayList<ABSValue>(l); // make sure we can use remove()
            ABSValue head = ml.remove(0);
            return DynamicClassUtils.instance("ABS.StdLib.List_Cons", head, toABSList(ml));
        }
    }

    /*
     * Transform a java.util.Set into an ABS.StdLib.Set
     */
    public static ABSValue toABSSet(java.util.Set<? extends ABSValue> set) {
        if (set.isEmpty()) {
            return DynamicClassUtils.instance("ABS.StdLib.Set_EmptySet");
        } else {
            ABSValue value = set.iterator().next();
            set.remove(value);
            return DynamicClassUtils.instance("ABS.StdLib.Set_Insert", value, toABSSet(set));
        }
    }

    /*
     * Transform a list of AST nodes into a java.util.Set
     */
    public static <T extends ASTNode<?>> java.util.Set<T> ASTListToSet(List<T> list) {
        java.util.Set<T> set = new HashSet<T>();
        for(T element : list)
            set.add(element);
        return set;
    }


}
