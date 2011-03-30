/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.common;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.PrintWriter;
import java.nio.MappedByteBuffer;
import java.nio.channels.FileChannel;
import java.nio.charset.Charset;

public class FileUtils {
    public static StringBuilder fileToStringBuilder(File f) throws IOException {
        FileInputStream stream = new FileInputStream(f);
        FileChannel fc = stream.getChannel();
        MappedByteBuffer bb = fc.map(FileChannel.MapMode.READ_ONLY, 0, fc.size());
          /* Instead of using default, pass in a decoder. */
        String res = Charset.defaultCharset().decode(bb).toString();
        stream.close();
        return new StringBuilder(res);
    }
    
    public static void writeStringBuilderToFile(StringBuilder sb, File f) throws IOException {
        PrintWriter pw = new PrintWriter(new FileOutputStream(f));
        pw.write(sb.toString());
        pw.close();
    }

}
