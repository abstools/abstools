package abs.backend.java;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.PrintStream;
import java.io.PrintWriter;

import org.junit.Test;

import junit.framework.Assert;

import abs.frontend.ast.Model;
import abs.frontend.parser.Main;

public class JavaBackend {
    public static void main(String[] args) throws Exception {

        
        final Model model = 
            Main.parse(new ByteArrayInputStream(testCode().getBytes()));
        model.generateJava(System.out);
    }

    private static File getTempFile(String testCode) throws IOException {
        File tmpFile = File.createTempFile("abs", "test");
        PrintWriter p = new PrintWriter(new FileOutputStream(tmpFile));
        p.print(testCode);
        p.close();
        tmpFile.deleteOnExit();
        
        return tmpFile;
    }

    private static String testCode() {
        return "interface A { }\n" +
        	   "class B implements A { } \n" +
        	   "data Void { }\n";
    }
    
}
