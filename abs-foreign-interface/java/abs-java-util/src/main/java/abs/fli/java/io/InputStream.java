package abs.fli.java.io;

import java.io.DataInputStream;
import java.io.IOException;

import abs.backend.java.lib.types.ABSString;
import abs.fli.java.PrimitiveUtil;
import FLI.StreamUtils.Feedback;
import FLI.StreamUtils.Feedback_Error;
import FLI.StreamUtils.Feedback_Result;
import FLI.StreamUtils.InputStream_c;

public class InputStream extends InputStream_c {
    
    private DataInputStream stream = null;
    private PrimitiveUtil putil = new PrimitiveUtil();
    
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
    
}
