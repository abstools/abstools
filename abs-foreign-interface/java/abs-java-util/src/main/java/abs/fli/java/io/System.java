package abs.fli.java.io;

import FLI.SystemUtils.System_i;
import abs.backend.java.fli.ABSForeignObject;
import abs.backend.java.lib.types.ABSString;
import abs.backend.java.lib.types.ABSUnit;

public class System extends ABSForeignObject implements System_i {

    public ABSUnit outPrint(ABSString s) {
        java.lang.System.out.print(s.getString());
        return ABSUnit.UNIT;
    }

    public ABSUnit outPrintln(ABSString s) {
        java.lang.System.out.println(s.getString());
        return ABSUnit.UNIT;
    }

}
