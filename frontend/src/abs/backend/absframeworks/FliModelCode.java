/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */


package abs.backend.absframeworks;

import java.io.BufferedReader;
import java.io.PrintWriter;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import abs.backend.common.CodeStream;
import java.io.UnsupportedEncodingException;


public class FliModelCode {
    private final File srcDir;
    private final List<File> files = new ArrayList<File>();
    private final List<String> mainClasses = new ArrayList<String>();

    public FliModelCode() throws IOException {
        srcDir = File.createTempFile("absjavabackend", Long.toString(System.nanoTime()));
        srcDir.delete();
        srcDir.mkdir();
    }

    public FliModelCode(File srcDir) {
        this.srcDir = srcDir;
    }

    public void addFile(File file) {
        files.add(file);
    }

    public String[] getFileNames() {
        String[] res = new String[files.size()];
        int i = 0;
        for (File f : files) {
            res[i++] = f.getAbsolutePath();
        }
        return res;
    }

    public CodeStream createFliModelSourceFile(String filename) throws FileNotFoundException, UnsupportedEncodingException {
        File f = new File("gen_abs_db_fli", filename + "_fli.java");
        f.getParentFile().mkdirs();
        return new CodeStream(new File("gen_abs_db_fli", filename + ".abs"));
    }

    public CodeStream createFliJavaModelSourceFile(String filename) throws FileNotFoundException, UnsupportedEncodingException {
        File f = new File("gen_java_db_fli", filename + "_fli.java");
        f.getParentFile().mkdirs();
        return new CodeStream(new File("gen_java_db_fli", filename + "_fli.java"));
    }

    public PrintWriter createPrintWriter(File file) throws FileNotFoundException {
        PrintWriter printWriter = new PrintWriter(file);
        return printWriter;
    }

    public File getSrcDir() {
        return srcDir;
    }

    public void deleteCode() {
        deleteDir(srcDir);
    }

    private void deleteDir(File dir) {
        for (File f : dir.listFiles()) {
            if (f.isDirectory()) {
                deleteDir(f);
            } else {
                f.delete();
            }
        }
        dir.delete();
    }

    // public void compile() throws FliModelCodeGenerationException {
    //     compile(srcDir);
    // }

    // public void compile(File destDir) throws FliModelCodeGenerationException {
    //     compile("-classpath", System.getProperty("java.class.path"), "-d", destDir.getAbsolutePath());
    // }

    // public void compile(String... args) throws FliModelCodeGenerationException {
    //     ArrayList<String> args2 = new ArrayList<String>();
    //     args2.addAll(Arrays.asList(args));
    //     args2.addAll(Arrays.asList(getFileNames()));
    //     JavaCompiler.compile(args2.toArray(new String[0]));
    // }

    // public String getFirstMainClass() {
    //     if (mainClasses.isEmpty())
    //         throw new IllegalStateException("There is no main class");

    //     return mainClasses.get(0);
    // }

    // public String toString() {
    //     StringBuilder res = new StringBuilder();

    //     for (File f : files) {
    //         append(res, f);
    //     }

    //     return res.toString();
    // }

    // private void append(StringBuilder res, File f) {
    //     try {
    //         BufferedReader reader = new BufferedReader(new FileReader(f));
    //         try {
    //             while (reader.ready()) {
    //                 res.append(reader.readLine() + "\n");
    //             }
    //         } catch (IOException e) {
    //             e.printStackTrace();
    //         }
    //     } catch (FileNotFoundException e) {
    //         e.printStackTrace();
    //     }
    // }

}
