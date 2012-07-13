/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.common;

import java.util.ArrayList;

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

}
