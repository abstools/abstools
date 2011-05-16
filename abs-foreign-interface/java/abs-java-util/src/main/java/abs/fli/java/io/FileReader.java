package abs.fli.java.io;

import java.io.BufferedReader;
import java.io.FileNotFoundException;
import java.io.IOException;

import FLI.FileUtils.FileReader_i;
import abs.backend.java.lib.types.ABSBool;
import abs.backend.java.lib.types.ABSString;

public class FileReader extends FileSetter implements FileReader_i {
 
    private BufferedReader reader;
    
    public ABSString readLine() {
        try {
            String r = reader.readLine();
            return (r == null) ? ABSString.EMPTY : putil.convert(r);
        } catch (IOException e) {
            return ABSString.EMPTY;
        }
    }
    
    public ABSBool open() {
        try {
            reader = new BufferedReader(new java.io.FileReader(handler.getInternalFile()));
            return ABSBool.TRUE;
        } catch (FileNotFoundException e) {
            return ABSBool.FALSE;  
        }
    }

    public ABSBool close() {
        try {
            reader.close();
            return ABSBool.TRUE;
        } catch (IOException e) {
            return ABSBool.FALSE;  
        }
    }

}
