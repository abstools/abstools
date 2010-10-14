package abs.backend.java.lib.expr;

import abs.backend.java.lib.types.ABSBool;
import abs.backend.java.lib.types.ABSValue;

public class BinOp {
    public static ABSBool eq(ABSValue v, ABSValue v2) {
        if (v == null)
            return ABSBool.fromBoolean(v2 == null);
        return v.eq(v2);
    }
    
    public static ABSBool notEq(ABSValue v, ABSValue v2) {
        return eq(v,v2).negate();
    }
}
