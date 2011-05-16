package abs.fli.java.io;

import FLI.FileUtils.File;
import abs.backend.java.fli.ABSForeignObject;
import abs.backend.java.lib.types.ABSString;
import abs.backend.java.lib.types.ABSUnit;
import abs.fli.java.PrimitiveUtil;

abstract class FileSetter extends ABSForeignObject {
    
    protected final PrimitiveUtil putil = new PrimitiveUtil();
    protected FileHandler handler = new FileHandler();
    
    public ABSUnit setFileName(ABSString f) {
        return handler.setFileName(f);
    }

    public ABSUnit setFile(File f) {
        return handler.setFile(f);
    }

    public ABSUnit setFileAt(File parent, ABSString name) {
        return handler.setFileAt(parent, name);
    }
}
