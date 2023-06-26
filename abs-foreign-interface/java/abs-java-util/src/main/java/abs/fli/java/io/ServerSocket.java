package abs.fli.java.io;

import java.io.IOException;
import java.net.InetSocketAddress;

import FLI.SocketUtils.ServerSocket_c;
import FLI.SocketUtils.Socket_c;
import FLI.SocketUtils.Socket_i;
import FLI.StreamUtils.Feedback;
import FLI.StreamUtils.Feedback_Error;
import FLI.StreamUtils.Feedback_OK;
import FLI.StreamUtils.Feedback_Result;
import abs.backend.java.lib.types.ABSBool;
import abs.backend.java.lib.types.ABSInteger;
import abs.backend.java.lib.types.ABSUnit;
import abs.fli.java.PrimitiveUtil;

/**
 * 
 * @author pwong
 *
 */
public class ServerSocket extends ServerSocket_c {

    private java.net.ServerSocket socket = null;
    private PrimitiveUtil putil = new PrimitiveUtil();
    
    @Override
    public Feedback<ABSUnit> fli_bind(ABSInteger port) {
        try {
            socket = new java.net.ServerSocket();
            socket.bind(new InetSocketAddress(port.toInt()));
            return new Feedback_OK<ABSUnit>();
        } catch (Exception e) {
            return new Feedback_Error<ABSUnit>(putil.convert(e.getMessage()));
        }
    }
    
    @Override
    public Feedback<ABSUnit> fli_setSoTimeout(ABSInteger timeout){ 
        try {
            socket.setSoTimeout(timeout.toInt());
            return new Feedback_OK<ABSUnit>();
        } catch (IOException e) {
            return new Feedback_Error<ABSUnit>(putil.convert(e.getMessage()));
        }
    }
    
    @Override
    public Feedback<Socket_i> fli_accept() {
        try {
            java.net.Socket s = socket.accept();
            Socket fli_s = Socket_c.createNewCogObject(); 
            fli_s.setSocket(s);
            return new Feedback_Result<Socket_i>(fli_s);
        } catch (IOException e) {
            return new Feedback_Error<Socket_i>(putil.convert(e.getMessage()));
        }
    }
    
    @Override
    public ABSBool fli_isClosed() {
        return putil.convert(socket.isClosed());
    }

    
}
