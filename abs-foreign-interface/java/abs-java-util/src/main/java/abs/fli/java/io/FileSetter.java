package abs.fli.java.io;

import FLI.FileUtils.File;
import abs.backend.java.lib.types.ABSString;
import abs.backend.java.lib.types.ABSUnit;
import abs.fli.java.PrimitiveUtil;

class FileSetter {
    
    private final PrimitiveUtil putil = new PrimitiveUtil();
    private final FileHandler handler = new FileHandler();
    
    PrimitiveUtil getPrimitiveUtil() { 
        return putil; 
    }
    
    FileHandler getHandler() { 
        return handler; 
    }
    
    ABSUnit setFileName(ABSString f) {
        return handler.setFileName(f);
    }

    ABSUnit setFile(File f) {
        return handler.setFile(f);
    }

    ABSUnit setFileAt(File parent, ABSString name) {
        return handler.setFileAt(parent, name);
    }
}
