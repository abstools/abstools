package abs.fli.java.io;

import FLI.SystemUtils.System_c;
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

}
