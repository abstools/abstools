package abs.fli.java.io;

import java.util.ArrayList;

import ABS.StdLib.List;
import ABS.StdLib.length_f;
import FLI.StreamUtils.Byte;
import FLI.StreamUtils.Byte_Byte;
import abs.backend.java.lib.types.ABSInteger;
import abs.fli.java.CollectionUtil;
import abs.fli.java.Fun;

/**
 * 
 * @author pwong
 *
 */
public class ByteUtil {
    private final CollectionUtil cutil = new CollectionUtil();

    private final Fun<Byte, java.lang.Byte> bTB = new Fun<Byte, java.lang.Byte>() {
        @Override
        public java.lang.Byte evaluate(Byte a) {
            return new java.lang.Integer(a.toByte().getArg0().toInt()).byteValue();
        }
    };
    
    private final Fun<java.lang.Byte, Byte> BTb = new Fun<java.lang.Byte, Byte>() {
        @Override
        public Byte evaluate(java.lang.Byte a) {
            return new Byte_Byte(ABSInteger.fromInt(a.intValue())); 
        }
    };
    
    public byte convert(Byte b) {
        return bTB.evaluate(b);
    }
    
    public Byte convert(byte b) {
        return BTb.evaluate(new java.lang.Byte(b));
    }
    
    public byte[] convert(List<Byte> bytes) {
        byte[] bs = new byte[length_f.apply(bytes).toInt()];
        java.util.List<java.lang.Byte> it = cutil.convert(bTB,bytes);
        for (int i=0; i<bs.length; i++) {
            bs[i] = it.get(i);
        }
        return bs;
    }
    
    public List<Byte> convert(byte[] bytes) {
        java.util.List<java.lang.Byte> java = new ArrayList<java.lang.Byte>();
        for (byte b : bytes) {
            java.add(new java.lang.Byte(b));
        }
        return cutil.convert(BTb,java);
    }
    
}
