package abs.fli.java.io;

import java.io.DataOutputStream;
import java.io.IOException;

import ABS.StdLib.List;
import FLI.StreamUtils.Feedback;
import FLI.StreamUtils.Feedback_Error;
import FLI.StreamUtils.Feedback_OK;
import FLI.StreamUtils.OutputStream_c;
import abs.backend.java.lib.types.ABSInteger;
import abs.backend.java.lib.types.ABSString;
import abs.backend.java.lib.types.ABSUnit;
import abs.fli.java.PrimitiveUtil;

public class OutputStream extends OutputStream_c {

    private DataOutputStream stream;
    private final PrimitiveUtil putil = new PrimitiveUtil();
    private final ByteUtil butil = new ByteUtil();

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
    
    public Feedback<ABSUnit> fli_write(List<FLI.StreamUtils.Byte> b, ABSInteger off, ABSInteger len) {
        try {
            stream.write(butil.convert(b), off.toInt(), len.toInt());
            return new Feedback_OK<ABSUnit>();
        } catch (IOException e) {
            return new Feedback_Error<ABSUnit>(putil.convert(e.getMessage()));
        }
    }

}
