/**
 * 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.backend.erlang;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.io.UnsupportedEncodingException;
import java.net.JarURLConnection;
import java.net.URLConnection;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.jar.JarEntry;
import java.util.jar.JarFile;

import org.apache.commons.io.FileUtils;

import sun.net.www.protocol.file.FileURLConnection;
import abs.backend.common.CodeStream;

import com.google.common.collect.ImmutableSet;
import com.google.common.io.ByteStreams;
import com.google.common.io.Files;

/**
 * Represents a to be generate Erlang application.
 * 
 * Provides facilities to create a module and creates a copy of the runtime
 * there.
 * 
 * @author Georg Göri
 * 
 */
public class ErlApp {

    public File destDir;
    public File destCodeDir;
    public File destIncludeDir;

    public Map<String, CodeStream> funMod = new HashMap<String, CodeStream>();

    public ErlApp(File destDir) throws IOException {
        super();
        this.destDir = destDir;
        this.destCodeDir = new File(destDir, "src/");
        this.destIncludeDir = new File(destDir, "include/");
        destDir.mkdirs();
        FileUtils.cleanDirectory(destDir);
        destDir.mkdirs();
        new File(destDir, "ebin").mkdir();
        copyRuntime();
    }

    public CodeStream createSourceFile(String moduleName) throws FileNotFoundException, UnsupportedEncodingException {
        return new CodeStream(new File(destCodeDir, moduleName + ".erl"));
    }

    public CodeStream createIncludeFile(String filename) throws FileNotFoundException, UnsupportedEncodingException {
        return new CodeStream(new File(destIncludeDir, filename + ".hrl"));
    }


    /**
     * All functions for an ABS module are stored in one Erlang module.
     * 
     * This method creates the necessary stream.
     */
    public CodeStream getFunStream(String moduleName) throws FileNotFoundException, UnsupportedEncodingException {
        if (!funMod.containsKey(moduleName)) {
            CodeStream ecs = new CodeStream(new File(destCodeDir, ErlUtil.getModuleName(moduleName)
                    + "_funs.erl"));
            funMod.put(moduleName, ecs);
            ecs.pf("-module(%s).", ErlUtil.getModuleName(moduleName) + "_funs");
            ecs.println("-compile(export_all).");
            ecs.println("-include_lib(\"../include/abs_types.hrl\").");
            ecs.println();
        }

        return funMod.get(moduleName);
    }

    public void close() {
        for (Entry<String, CodeStream> e : funMod.entrySet())
            e.getValue().close();
        funMod.clear();
    }

    private static final Set<String> RUNTIME_FILES = ImmutableSet.of(
            "src/*",
            "include/*",
            "deps/*",
            "Emakefile",
            "Makefile",
            "Dockerfile",
            "start_console",
            "run",
            "rebar.config",
            "gcstats_as_csv.erl",
            "bin/*",
            "link_sources"
            );
    private static final Set<String> EXEC_FILES = ImmutableSet.of(
            "bin/rebar",
            "run",
            "start_console",
            "link_sources"
            );
        
    private static final String RUNTIME_PATH = "abs/backend/erlang/runtime/";

    private void copyRuntime() throws IOException {
        InputStream is = null;
        // TODO: this only works when the erlang compiler is invoked
        // from a jar file.  See http://stackoverflow.com/a/2993908 on
        // how to handle the other case.
        URLConnection resource = getClass().getResource("").openConnection();
        try {
            for (String f : RUNTIME_FILES) {
                if (f.endsWith("/*")) {
                    String dirname = f.substring(0, f.length() - 2);
                    String inname = RUNTIME_PATH + dirname;
                    String outname = destDir + "/" + dirname;
                    new File(outname).mkdirs();
                    if (resource instanceof JarURLConnection) {
                        copyJarDirectory(((JarURLConnection) resource).getJarFile(),
                                inname, outname);
                    } else if (resource instanceof FileURLConnection) {
                        /* stolz: This at least works for the unit tests from within Eclipse */
                        File file = new File("src");
                        assert file.exists();
                        FileUtils.copyDirectory(new File("src/"+RUNTIME_PATH), destDir);
                    } else {
                        throw new UnsupportedOperationException("File type: "+resource);
                    }
                    
                } else {
                    is = ClassLoader.getSystemResourceAsStream(RUNTIME_PATH + f);
                    if (is == null)
                        throw new RuntimeException("Could not locate Runtime file:" + f);
                    String outputFile = f.replace('/', File.separatorChar);
                    File file = new File(destDir, outputFile);
                    file.getParentFile().mkdirs();
                    ByteStreams.copy(is, Files.newOutputStreamSupplier(file));
                }
            }
        } finally {
            if (is != null)
                is.close();
        }
        for (String f : EXEC_FILES) {
            new File(destDir, f).setExecutable(true, false);
        }
    }

    private void copyJarDirectory(JarFile jarFile, String inname, String outname)
            throws IOException {
        InputStream is = null;
        for (JarEntry entry : Collections.list(jarFile.entries())) {
            if (entry.getName().startsWith(inname)) {
                String relFilename = entry.getName().substring(inname.length());
                if (!entry.isDirectory()) {
                    is = jarFile.getInputStream(entry);
                    ByteStreams.copy(is, 
                            Files.newOutputStreamSupplier(new File(outname, relFilename)));
                } else {
                    new File(outname, relFilename).mkdirs();
                }
            }
        }
        is.close();
    }

    public void generateModuleDefinitions(String absModulename, String erlModulename) throws FileNotFoundException, UnsupportedEncodingException {
        CodeStream hcs = createIncludeFile("absmodulename");
        hcs.println("%%This file is licensed under the terms of the Modified BSD License.");
        hcs.println("-undef(ABSMAINMODULE).");
        hcs.println("-define(ABSMAINMODULE," + erlModulename + ").");
        hcs.close();
    }
}
