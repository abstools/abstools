/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.common;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.PrintWriter;
import java.nio.charset.Charset;

public class FileUtils {
    
    private static String fileToString(File f) throws IOException {
        InputStream in = new FileInputStream(f);
        try {
            byte[] b = new byte[(int) f.length()];
            int len = b.length;
            int total = 0;

            while (total < len) {
                int result = in.read(b, total, len - total);
                if (result == -1) {
                    break;
                }
                total += result;
            }
            return new String(b, Charset.defaultCharset());
        } finally {
            in.close();
        }
    }
    
    public static StringBuilder fileToStringBuilder(File f) throws IOException {
        return new StringBuilder(fileToString(f));
    }
    
    public static void writeStringBuilderToFile(StringBuilder sb, File f) throws IOException {
        PrintWriter pw = new PrintWriter(new FileOutputStream(f));
        pw.write(sb.toString());
        pw.close();
    }
}
