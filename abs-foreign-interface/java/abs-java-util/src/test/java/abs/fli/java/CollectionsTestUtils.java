package abs.fli.java;

import java.util.Comparator;

import ABS.StdLib.List;
import ABS.StdLib.List_Cons;
import FLI.StreamUtils.Byte;
import abs.backend.java.lib.types.ABSInteger;
import abs.backend.java.lib.types.ABSValue;

public class CollectionsTestUtils {

    public static final Comparator<Byte> BYTE_COMP = new Comparator<Byte>() {
        public int compare(Byte o1, Byte o2) {
            return Integer.valueOf(o1.toByte().getArg0().toInt()).compareTo(
                    Integer.valueOf(o2.toByte().getArg0().toInt()));
        }
    };
    
    public static final Comparator<ABSInteger> INT_COMP = new Comparator<ABSInteger>() {
        public int compare(ABSInteger o1, ABSInteger o2) {
            return Integer.valueOf(o1.toInt()).compareTo(Integer.valueOf(o2.toInt()));
        }
    };
    
    public <A extends ABSValue> boolean equals(Comparator<A> c, List<A> l, List<A> m) {
        if (l.isCons()) {
            if (! m.isCons()) {
                return false;
            }
            
            List_Cons<A> conl = l.toCons();
            List_Cons<A> conm = m.toCons();
            
            return 
                c.compare(conl.getArg0(), conm.getArg0()) == 0 && 
                equals(c, conl.getArg1(),conm.getArg1());
        }
        
        return m.isNil();
        
    } 
    
    
    public <A extends ABSValue> boolean equals(Comparator<A> c, java.util.List<A> l, java.util.List<A> r) {
        for (int i=0; i < l.size(); i++) {
            if (c.compare(l.get(i), r.get(i)) != 0) {
                return false;
            }
        }
        return true;
    }

    
}
