package abs.fli.java.io;

import java.io.IOException;

import FLI.FileUtils.FileWriter_i;
import abs.backend.java.lib.types.ABSBool;
import abs.backend.java.lib.types.ABSString;

public class FileWriter extends FileSetter implements FileWriter_i {

    private java.io.FileWriter writer;
    
    public ABSBool write(ABSString s) {
        try {
            writer.write(s.getString());
            return ABSBool.TRUE;
        } catch (IOException e) {
            return ABSBool.FALSE;
        }
    }

    public ABSBool flush() {
        try {
            writer.flush();
            return ABSBool.TRUE;
        } catch (IOException e) {
            return ABSBool.FALSE;
        }
    }

    public ABSBool close() {
        try {
            writer.close();
            return ABSBool.TRUE;
        } catch (IOException e) {
            return ABSBool.FALSE;
        }
    }

    public ABSBool open() {
        try {
            writer = new java.io.FileWriter(handler.getInternalFile());
            return ABSBool.TRUE;
        } catch (IOException e) {
            return ABSBool.FALSE;  
        }
    }

}
