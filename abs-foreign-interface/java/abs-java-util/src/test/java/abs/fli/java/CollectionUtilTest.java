package abs.fli.java;

import static abs.backend.java.lib.types.ABSInteger.fromInt;
import static abs.fli.java.CollectionsTestUtils.INT_COMP;
import static org.junit.Assert.assertTrue;

import java.util.Arrays;

import org.junit.Test;

import ABS.StdLib.List;
import ABS.StdLib.List_Cons;
import ABS.StdLib.List_Nil;
import abs.backend.java.lib.types.ABSInteger;

public class CollectionUtilTest {

    private final CollectionUtil util = new CollectionUtil();
    private final CollectionsTestUtils tutil = new CollectionsTestUtils();
    
    private List<ABSInteger> cons(int i, List<ABSInteger> is) {
        return new List_Cons<ABSInteger>(fromInt(i),is);
    }
    
    @Test
    public final void testConvertListOfA() {
        List<ABSInteger> lint = 
            cons(1,cons(2,cons(3,new List_Nil<ABSInteger>())));
        
        java.util.List<ABSInteger> rint = 
            Arrays.asList(fromInt(1),fromInt(2),fromInt(3));
        
        assertTrue(tutil.equals(INT_COMP,rint,util.convert(lint)));
    }
    
    @Test
    public final void testConvertListOfA1() {
        java.util.List<ABSInteger> rint = 
            Arrays.asList(fromInt(1),fromInt(2),fromInt(3));
        
        List<ABSInteger> lint = 
            cons(1,cons(2,cons(3,new List_Nil<ABSInteger>())));
        
        assertTrue(tutil.equals(INT_COMP,lint,util.convert(rint)));
    }

}
