package abs.fli.java.io;

import java.io.DataOutputStream;
import java.io.IOException;
import java.util.Iterator;

import ABS.StdLib.List;
import ABS.StdLib.length_f;
import FLI.StreamUtils.Byte;
import FLI.StreamUtils.Feedback;
import FLI.StreamUtils.Feedback_Error;
import FLI.StreamUtils.Feedback_OK;
import FLI.StreamUtils.OutputStream_c;
import abs.backend.java.lib.types.ABSInteger;
import abs.backend.java.lib.types.ABSString;
import abs.backend.java.lib.types.ABSUnit;
import abs.fli.java.CollectionUtil;
import abs.fli.java.Fun;
import abs.fli.java.PrimitiveUtil;

public class OutputStream extends OutputStream_c {

    private DataOutputStream stream;
    private PrimitiveUtil putil = new PrimitiveUtil();
    private CollectionUtil cutil = new CollectionUtil();

    void setStream(DataOutputStream stream) {
        this.stream = stream;
    }

    public Feedback<ABSUnit> fli_writeUTF(ABSString s) {
        try {
            stream.writeUTF(s.getString());
            return new Feedback_OK<ABSUnit>();
        } catch (IOException e) {
            return new Feedback_Error<ABSUnit>(putil.convert(e.getMessage()));
        }
    }

    public Feedback<ABSUnit> fli_writeLong(FLI.StreamUtils.Long s) {
        try {
            stream.writeLong(new Long(s.toLong().getArg0().toInt()));
            return new Feedback_OK<ABSUnit>();
        } catch (IOException e) {
            return new Feedback_Error<ABSUnit>(putil.convert(e.getMessage()));
        }
    }

    public Feedback<ABSUnit> fli_writeInt(ABSInteger s) {
        try {
            stream.write(s.toInt());
            return new Feedback_OK<ABSUnit>();
        } catch (IOException e) {
            return new Feedback_Error<ABSUnit>(putil.convert(e.getMessage()));
        }
    }
    
    private final Fun<Byte, java.lang.Byte> bTb = new Fun<Byte, java.lang.Byte>() {
        @Override
        public java.lang.Byte evaluate(Byte a) {
            return new java.lang.Integer(a.toByte().getArg0().toInt()).byteValue();
        }
    };

    public Feedback<ABSUnit> fli_write(List<FLI.StreamUtils.Byte> b, ABSInteger off, ABSInteger len) {
        byte[] bs = new byte[length_f.apply(b).toInt()];
        Iterator<java.lang.Byte> it = cutil.convert(bTb,b).iterator();
        for (int i=0; i<bs.length; i++) {
            bs[i] = it.next();
        }
        try {
            stream.write(bs, off.toInt(), len.toInt());
            return new Feedback_OK<ABSUnit>();
        } catch (IOException e) {
            return new Feedback_Error<ABSUnit>(putil.convert(e.getMessage()));
        }
    }

}
