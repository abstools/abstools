package abs.fli.java.io;

import java.util.Arrays;

import ABS.StdLib.List;
import FLI.FileUtils.File;
import FLI.FileUtils.FileHandler_i;
import FLI.FileUtils.File_File;
import FLI.FileUtils.filePath_f;
import abs.backend.java.fli.ABSForeignObject;
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
@SuppressWarnings("unchecked")
public class FileHandler extends ABSForeignObject implements FileHandler_i {

    private final String sep = java.io.File.pathSeparator;
    private final CollectionUtil cutil = new CollectionUtil();
    private final PrimitiveUtil putil = new PrimitiveUtil();
    
    private final Fun<java.io.File,File> toFile = new Fun<java.io.File,File>() {
        public File evaluate(java.io.File a) {
            return new File_File(putil.convert(a.getAbsolutePath()));
        }
    };
    
    private java.io.File file;
    
    public ABSUnit setFileName(ABSString f) {
        file = new java.io.File(f.getString());
        return ABSUnit.UNIT;
    }

    public ABSUnit setFile(File f) {
        file = file(f);
        return ABSUnit.UNIT;
    }

    public ABSUnit setFileAt(File parent, ABSString name) {
        file = new java.io.File(file(parent),name.getString());
        return ABSUnit.UNIT;
    }
    
    public ABSBool canRead() {
        return putil.convert(file.canRead());
    }

    public ABSBool canWrite() {
        return putil.convert(file.canWrite());
    }

    public ABSBool delete() {
        return putil.convert(file.delete());
    }

    public ABSBool isFile() {
        return putil.convert(file.isFile());
    }

    public ABSBool isDirectory() {
        return putil.convert(file.isDirectory());
    }

    public ABSBool renameTo(File dest) {
        return putil.convert(file.renameTo(file(dest)));
    }
    
    public List<File> listFiles() {
        return cutil.convert(toFile,Arrays.asList(file.listFiles()));
    }

    public ABSString getName() {
        return putil.convert(file.getName());
    }

    public ABSString getParent() {
        return putil.convert(file.getParent());
    }

    public ABSString getAbsolutePath() {
        return putil.convert(file.getAbsolutePath());
    }

    public ABSBool mkdir() {
        return putil.convert(file.mkdir());
    }

    public ABSBool createNewFile() {
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
        for (ABSValue value : cutil.convert(filePath_f.apply(f))) {
            if (value instanceof ABSString) {
                builder.append(((ABSString) value).getString()).append(sep);   
            }
        }
        return builder.substring(0, builder.length()-1).toString();
    }

}
