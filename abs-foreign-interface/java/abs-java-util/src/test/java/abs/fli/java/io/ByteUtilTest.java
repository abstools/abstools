package abs.fli.java.io;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import static abs.fli.java.CollectionsTestUtils.*;

import org.junit.BeforeClass;
import org.junit.Test;
import org.junit.experimental.theories.DataPoints;
import org.junit.experimental.theories.Theories;
import org.junit.experimental.theories.Theory;
import org.junit.runner.RunWith;

import ABS.StdLib.List;
import ABS.StdLib.List_Cons;
import ABS.StdLib.List_Nil;
import FLI.StreamUtils.Byte;
import FLI.StreamUtils.Byte_Byte;
import abs.backend.java.lib.types.ABSInteger;
import abs.fli.java.CollectionsTestUtils;

@RunWith(Theories.class)
public class ByteUtilTest {
    
    private final ByteUtil util = new ByteUtil();
    private final CollectionsTestUtils tutil = new CollectionsTestUtils();
    
    @DataPoints
    public static Byte[] bs = null;
    
    @BeforeClass
    public static void generateData() {
        bs = new Byte[1000];
        for (int i=0; i<1000; i++) {
            bs[i] = new Byte_Byte(ABSInteger.fromInt(i));
        }
    }
    
    @Theory
    public final void testConvertByte(Byte b) {
        assertEquals(
           Integer.valueOf(b.toByte().getArg0().toInt()).byteValue(),
           util.convert(b));
    }
    
    @Test
    public final void testConvert() {
        List<Byte> bytes = new List_Nil<Byte>();
        bytes = con(1,con(2,con(3,bytes)));
        assertArrayEquals(new byte[]{1,2,3}, util.convert(bytes));
        assertTrue(tutil.equals(BYTE_COMP,bytes,util.convert(new byte[]{1,2,3})));
    }

    private List<Byte> con(int i, List<Byte> ls) {
        return new List_Cons<Byte>(new Byte_Byte(ABSInteger.fromInt(i)),ls);
    }
    
}
