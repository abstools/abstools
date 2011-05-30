package abs.fli.java.io;

import java.io.DataInputStream;
import java.io.IOException;

import ABS.StdLib.List;
import FLI.StreamUtils.Feedback;
import FLI.StreamUtils.Feedback_Error;
import FLI.StreamUtils.Feedback_Result;
import FLI.StreamUtils.InputStream_c;
import abs.backend.java.lib.types.ABSInteger;
import abs.backend.java.lib.types.ABSString;
import abs.fli.java.PrimitiveUtil;

public class InputStream extends InputStream_c {
    
    private DataInputStream stream = null;
    private final PrimitiveUtil putil = new PrimitiveUtil();
    private final ByteUtil butil = new ByteUtil();
    
    void setStream(DataInputStream stream) {
        this.stream = stream;
    }
    
    @Override
    public Feedback<ABSString> fli_readUTF() {
        try {
            return new Feedback_Result<ABSString>(putil.convert(stream.readUTF()));
        } catch (IOException e) {
            return new Feedback_Error<ABSString>(putil.convert(e.getMessage()));
        }
    }
    
    @Override
    public Feedback<ABSInteger> fli_readInt() { 
        try {
            return new Feedback_Result<ABSInteger>(putil.convert(stream.readInt()));
        } catch (IOException e) {
            return new Feedback_Error<ABSInteger>(putil.convert(e.getMessage()));
        }
    }
    
    @Override
    public Feedback<List<FLI.StreamUtils.Byte>> fli_read(List<FLI.StreamUtils.Byte> b, ABSInteger off, ABSInteger len) {
        try {
            byte[] bs = butil.convert(b);
            stream.read(bs, off.toInt(), len.toInt());
            return new Feedback_Result<List<FLI.StreamUtils.Byte>>(butil.convert(bs));
        } catch (IOException e) {
            return new Feedback_Error<List<FLI.StreamUtils.Byte>>(putil.convert(e.getMessage()));
        }
    }
    
}
