package abs.common;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileOutputStream;
import java.io.FileReader;
import java.io.IOException;
import java.io.PrintWriter;

public class FileUtils {
    public static StringBuilder fileToStringBuilder(File f) throws IOException {
        BufferedReader fr = new BufferedReader(new FileReader(f));
        StringBuilder result = new StringBuilder();
        String s;
        while ((s = fr.readLine()) != null) {
            result.append(s);
            result.append('\n');
        }
        fr.close();
        return result;
    }
    
    public static void writeStringBuilderToFile(StringBuilder sb, File f) throws IOException {
        PrintWriter pw = new PrintWriter(new FileOutputStream(f));
        pw.write(sb.toString());
        pw.close();
    }

}
