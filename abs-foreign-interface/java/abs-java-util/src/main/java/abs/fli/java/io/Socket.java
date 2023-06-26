package abs.fli.java.io;

import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;
import java.net.InetAddress;
import java.net.InetSocketAddress;
import java.net.SocketAddress;

import FLI.SocketUtils.Socket_c;
import FLI.StreamUtils.Feedback;
import FLI.StreamUtils.Feedback_Error;
import FLI.StreamUtils.Feedback_OK;
import FLI.StreamUtils.InputStream_c;
import FLI.StreamUtils.OutputStream_c;
import abs.backend.java.lib.runtime.ABSAssertException;
import abs.backend.java.lib.types.ABSBool;
import abs.backend.java.lib.types.ABSInteger;
import abs.backend.java.lib.types.ABSString;
import abs.backend.java.lib.types.ABSUnit;
import abs.fli.java.PrimitiveUtil;

/**
 * 
 * @author pwong
 * 
 */
public class Socket extends Socket_c {

	private InputStream input;
	private OutputStream output;
	private final PrimitiveUtil putil = new PrimitiveUtil();

	private java.net.Socket client_socket;

	public Socket() {
		super();
	}

	void setSocket(java.net.Socket socket) {
		__ABS_init();
		this.client_socket = socket;
		setupStream();
		try {
			input.setStream(new DataInputStream(client_socket.getInputStream()));
			output.setStream(new DataOutputStream(client_socket.getOutputStream()));
		} catch (IOException e) {
			throw new ABSAssertException(e.getLocalizedMessage());
		}
	}
	
	private void setupStream() {
		this.input = InputStream_c.createNewCogObject();
		this.output = OutputStream_c.createNewCogObject();
	}

	@Override
	public Feedback<ABSUnit> fli_connect(ABSString server, ABSInteger port,
			ABSInteger timeout) {
		try {
			client_socket = new java.net.Socket();
			SocketAddress serverAddress = 
			    new InetSocketAddress(InetAddress.getByName(server.getString()), port.toInt());

			client_socket.connect(serverAddress, timeout.toInt());
			setupStream();
			input.setStream(new DataInputStream(client_socket.getInputStream()));
			output.setStream(new DataOutputStream(client_socket.getOutputStream()));
			return new Feedback_OK<ABSUnit>();
		} catch (Exception e) {
			e.printStackTrace();
			return new Feedback_Error<ABSUnit>(putil.convert(e.getMessage()));
		}
	}

	@Override
	public FLI.StreamUtils.InputStream_i fli_getInputStream() {
		return input;
	}

	@Override
	public FLI.StreamUtils.OutputStream_i fli_getOutputStream() {
		return output;
	}

	@Override
	public Feedback<ABSUnit> fli_close() {
		try {
			client_socket.close();
			return new Feedback_OK<ABSUnit>();
		} catch (IOException e) {
			return new Feedback_Error<ABSUnit>(putil.convert(e.getMessage()));
		}
	}

	@Override
	public ABSBool fli_isConnected() {
		return putil.convert(client_socket.isConnected());
	}

}
