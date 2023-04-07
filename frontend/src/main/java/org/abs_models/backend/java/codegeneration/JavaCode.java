/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved.
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.abs_models.backend.java.codegeneration;

import org.abs_models.backend.java.JavaBackend;
import org.apache.commons.io.FileUtils;

import javax.tools.*;
import java.io.*;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public class JavaCode {
    private final File srcDir;
    private final List<File> files = new ArrayList<>();
    private final List<String> mainClasses = new ArrayList<>();

    public JavaCode() throws IOException {
        srcDir = Files.createTempDirectory("absjavabackend").toFile();
    }

    public JavaCode(File srcDir) {
        this.srcDir = srcDir;
    }

    public String[] getFileNames() {
        String[] res = new String[files.size()];
        int i = 0;
        for (File f : files) {
            res[i++] = f.getAbsolutePath();
        }
        return res;
    }

    public List<File> getFiles() {
        return files;
    }

    public Package createPackage(String packageName) throws IOException {
        return new Package(packageName);
    }

    public class Package {
        public final String packageName;
        public final File packageDir;
        private String firstPackagePart;

        public Package(String packageName) throws IOException {
            this.packageName = packageName;
            this.packageDir = new File(srcDir, packageName.replace('.', File.separatorChar));
            this.firstPackagePart = packageName.split("\\.")[0];
            if (!packageDir.mkdirs() && !packageDir.isDirectory()) {
                throw new IOException("Could not create directory " + packageDir.toString());
            }
        }

        public File createJavaFile(String name) throws IOException, JavaCodeGenerationException {
            if (name.equals(firstPackagePart)) {
                if (name.equals("Main")) {
                    throw new JavaCodeGenerationException("The Java backend does not support main blocks in " +
                    		"modules with name 'Main'. Please try to use a different name.");
                }
                throw new JavaCodeGenerationException("The Java backend does not support using the name " +
                      name + " as module name, because it collides with a generated classname. " +
                      		"Please try to use a different name.");
            }
            File file = new File(packageDir, name + ".java");
            if (file.exists()) {
                file.delete();
            }
            if (!file.createNewFile()) {
                throw new IOException("Could not create Java file " + file.toString());
            }
            files.add(file);
            return file;
        }

        public void addMainClass(String s) {
            mainClasses.add(packageName + "." + s);
        }
    }

    public File getSrcDir() {
        return srcDir;
    }

    public void deleteCode() throws IOException {
        FileUtils.deleteDirectory(srcDir);
    }

    public void compile() throws JavaCodeGenerationException, IOException {
        compile(srcDir, "-classpath", System.getProperty("java.class.path"));
    }

    public void compile(File directory, String... compiler_args)
        throws JavaCodeGenerationException, IOException
    {
        List<File> files_in_directory = new ArrayList<File>();
        try (Stream<Path> paths = Files.walk(Paths.get(directory.toString()))) {
            files_in_directory = paths.filter(Files::isRegularFile)
                .filter(p -> p.toString().endsWith(".java"))
                .map(Path::toFile)
                .collect(Collectors.toList());
        }
        compile(files_in_directory, compiler_args);
    }

    public void compile(List<File> files, String... compiler_args)
        throws JavaCodeGenerationException, IOException
    {
        javax.tools.JavaCompiler compiler = ToolProvider.getSystemJavaCompiler();
        DiagnosticCollector<JavaFileObject> diagnostics = new DiagnosticCollector<JavaFileObject>();
        StandardJavaFileManager fileManager
            = compiler.getStandardFileManager(diagnostics, null, JavaBackend.CHARSET);
        // TODO (rudi): check whether compiler_args / options is necessary
        // "-classpath", System.getProperty("java.class.path"), 
        List<String> optionList = new ArrayList<String>();
        optionList.add("-classpath");
        optionList.add(System.getProperty("java.class.path"));
        javax.tools.JavaCompiler.CompilationTask task
            = compiler.getTask(null, fileManager, diagnostics, optionList, null,
                               fileManager.getJavaFileObjectsFromFiles(files));
        if (!task.call()) {
            StringBuilder s = new StringBuilder();
            for (Diagnostic<? extends JavaFileObject> diagnostic : diagnostics.getDiagnostics()) {
                s.append(String.format("%s:%d:%d: %s%n",
                                       diagnostic.getSource(),
                                       diagnostic.getLineNumber(),
                                       diagnostic.getColumnNumber(),
                                       diagnostic.getMessage(null)));
            }

            throw new JavaCodeGenerationException("There seems to be a bug in the ABS Java backend. " +
                                                  "The generated code contains errors:\n" + s.toString());
        }
        fileManager.close();
    }

    public String getFirstMainClass() {
        if (mainClasses.isEmpty())
            throw new IllegalStateException("There is no main class");

        return mainClasses.get(0);
    }

    public String toString() {
        StringBuilder res = new StringBuilder();

        for (File f : files) {
            append(res, f);
        }

        return res.toString();
    }

    private void append(StringBuilder res, File f) {
        try {
            BufferedReader reader = new BufferedReader(new FileReader(f));
            try {
                while (reader.ready()) {
                    res.append(reader.readLine() + "\n");
                }
            } catch (IOException e) {
                e.printStackTrace();
            } finally {
                reader.close();
            }
        } catch (FileNotFoundException e) {
            e.printStackTrace();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

}
