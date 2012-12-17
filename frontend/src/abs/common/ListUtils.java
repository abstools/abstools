/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.common;

import java.util.ArrayList;

import abs.backend.java.lib.types.ABSValue;
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
    public static ABS.StdLib.List<ABSValue> toABSList(java.util.List<? extends ABSValue> l) {
        if (l.isEmpty()) {
            return new ABS.StdLib.List_Nil();
        } else {
            ABSValue head = l.remove(0);
            return new ABS.StdLib.List_Cons(head, toABSList(l));
        }
    }

}
