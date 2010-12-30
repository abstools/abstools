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
