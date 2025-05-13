package abs.fli.java.io;

import java.util.Arrays;

import ABS.StdLib.List;
import FLI.FileUtils.File;
import FLI.FileUtils.FileHandler_c;
import FLI.FileUtils.File_File;
import FLI.FileUtils.filePath_f;
import abs.backend.java.lib.types.ABSBool;
import abs.backend.java.lib.types.ABSString;
import abs.backend.java.lib.types.ABSUnit;
import abs.backend.java.lib.types.ABSValue;
import abs.fli.java.CollectionUtil;
import abs.fli.java.Fun;
import abs.fli.java.PrimitiveUtil;

/**
 * An implementation of FLI.FileUtils.FileHandler
 * 
 * @author pwong
 *
 */
public class FileHandler extends FileHandler_c {

    private final String sep = java.io.File.pathSeparator;
    private final CollectionUtil cutil = new CollectionUtil();
    private final PrimitiveUtil putil = new PrimitiveUtil();
    
    private final Fun<java.io.File,File> toFile = new Fun<java.io.File,File>() {
        public File evaluate(java.io.File a) {
            return new File_File(putil.convert(a.getAbsolutePath()));
        }
    };
    
    private java.io.File file;
    
    public ABSUnit fli_setFileName(String f) {
        file = new java.io.File(f);
        return ABSUnit.UNIT;
    }

    public ABSUnit fli_setFile(File f) {
        file = file(f);
        return ABSUnit.UNIT;
    }

    public ABSUnit fli_setFileAt(File parent, String name) {
        file = new java.io.File(file(parent),name);
        return ABSUnit.UNIT;
    }
    
    public ABSBool fli_canRead() {
        return putil.convert(file.canRead());
    }

    public ABSBool fli_canWrite() {
        return putil.convert(file.canWrite());
    }

    public ABSBool fli_delete() {
        return putil.convert(file.delete());
    }

    public ABSBool fli_isFile() {
        return putil.convert(file.isFile());
    }

    public ABSBool fli_isDirectory() {
        return putil.convert(file.isDirectory());
    }

    public ABSBool fli_renameTo(File dest) {
        return putil.convert(file.renameTo(file(dest)));
    }
    
    public List<File> fli_listFiles() {
        return cutil.convert(toFile,Arrays.asList(file.listFiles()));
    }

    public String fli_getName() {
        return putil.convert(file.getName());
    }

    public String fli_getParent() {
        return putil.convert(file.getParent());
    }

    public String fli_getAbsolutePath() {
        return putil.convert(file.getAbsolutePath());
    }

    public ABSBool fli_mkdir() {
        return putil.convert(file.mkdir());
    }

    public ABSBool fli_createNewFile() {
        try {
            return putil.convert(file.createNewFile());
        } catch (Exception e) {
            return ABSBool.FALSE;
        }
    }
    
    java.io.File getInternalFile() {
        return file;
    }
    
    private java.io.File file(File f) {
        return new java.io.File(name(f));
    }
    
    private String name(File f) {
        StringBuilder builder = new StringBuilder();
        for (Object value : cutil.convert(filePath_f.apply(f))) {
            if (value instanceof String s) {
                builder.append(s).append(sep);
            }
        }
        return builder.substring(0, builder.length()-1).toString();
    }

}
