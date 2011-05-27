package abs.fli.java.io;

import java.io.BufferedReader;
import java.io.FileNotFoundException;
import java.io.IOException;

import FLI.FileUtils.File;
import FLI.FileUtils.FileReader_c;
import abs.backend.java.lib.types.ABSBool;
import abs.backend.java.lib.types.ABSString;
import abs.backend.java.lib.types.ABSUnit;

public class FileReader extends FileReader_c {
 
    private BufferedReader reader;
    private FileSetter setter = new FileSetter();
    
    public ABSString fli_readLine() {
        try {
            String r = reader.readLine();
            return (r == null) ? ABSString.EMPTY : setter.getPrimitiveUtil().convert(r);
        } catch (IOException e) {
            return ABSString.EMPTY;
        }
    }
    
    public ABSBool fli_open() {
        try {
            reader = new BufferedReader(new java.io.FileReader(setter.getHandler().getInternalFile()));
            return ABSBool.TRUE;
        } catch (FileNotFoundException e) {
            return ABSBool.FALSE;  
        }
    }

    public ABSBool fli_close() {
        try {
            reader.close();
            return ABSBool.TRUE;
        } catch (IOException e) {
            return ABSBool.FALSE;  
        }
    }
    
    public ABSUnit fli_setFileName(ABSString f) {
        return setter.getHandler().setFileName(f);
    }

    public ABSUnit fli_setFile(File f) {
        return setter.getHandler().setFile(f);
    }

    public ABSUnit fli_setFileAt(File parent, ABSString name) {
        return setter.getHandler().setFileAt(parent, name);
    }

}
