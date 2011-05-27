package abs.fli.java.io;

import java.io.BufferedWriter;
import java.io.IOException;

import FLI.FileUtils.File;
import FLI.FileUtils.FileWriter_c;
import abs.backend.java.lib.types.ABSBool;
import abs.backend.java.lib.types.ABSString;
import abs.backend.java.lib.types.ABSUnit;

public class FileWriter extends FileWriter_c {

    private BufferedWriter writer;
    private FileSetter setter = new FileSetter();
    
    public ABSBool fli_write(ABSString s) {
        try {
            writer.write(s.getString());
            return ABSBool.TRUE;
        } catch (IOException e) {
            return ABSBool.FALSE;
        }
    }
    
    public ABSBool fli_writeln(ABSString s) {
        try {
            writer.write(s.getString());
            writer.newLine();
            return ABSBool.TRUE;
        } catch (IOException e) {
            return ABSBool.FALSE;
        }
    }
    

    public ABSBool fli_flush() {
        try {
            writer.flush();
            return ABSBool.TRUE;
        } catch (IOException e) {
            return ABSBool.FALSE;
        }
    }

    public ABSBool fli_close() {
        try {
            writer.close();
            return ABSBool.TRUE;
        } catch (IOException e) {
            return ABSBool.FALSE;
        }
    }

    public ABSBool fli_open() {
        try {
            writer = new BufferedWriter(new java.io.FileWriter(setter.getHandler().getInternalFile()));
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
