package abs.fli.java.io;

import java.io.IOException;

import FLI.SystemUtils.System_c;
import abs.backend.java.lib.types.ABSInteger;
import abs.backend.java.lib.types.ABSString;
import abs.backend.java.lib.types.ABSUnit;

public class System extends System_c {

    public ABSUnit fli_outPrint(ABSString s) {
        java.lang.System.out.print(s.getString());
        return ABSUnit.UNIT;
    }

    public ABSUnit fli_outPrintln(ABSString s) {
        java.lang.System.out.println(s.getString());
        return ABSUnit.UNIT;
    }
    
    public ABSString fli_read() {
        char c = 0;
        try {
            c = (char) java.lang.System.in.read();
        } catch (IOException e) {
            // print stack trace for now...
            e.printStackTrace();
        }
        return ABSString.fromString(String.valueOf(c));
    }
    
    public ABSUnit fli_exit(ABSInteger i) {
    	java.lang.System.exit(i.toInt());
    	return ABSUnit.UNIT;
    }


}
