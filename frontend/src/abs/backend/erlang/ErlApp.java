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

    public Map<String, ErlangCodeStream> funMod = new HashMap<String, ErlangCodeStream>();

    public ErlApp(File destDir) throws IOException {
        super();
        this.destDir = destDir;
        destDir.mkdirs();
        FileUtils.cleanDirectory(destDir);
        destDir.mkdirs();
        new File(destDir, "ebin").mkdir();
        copyRuntime();
    }

    public ErlangCodeStream createFile(String moduleName) throws FileNotFoundException, UnsupportedEncodingException {
        return new ErlangCodeStream(new File(destDir, moduleName + ".erl"));
    }

    /**
     * All functions for an ABS module are stored in one Erlang module.
     * 
     * This method creates the necessary stream.
     */
    public ErlangCodeStream getFunStream(String moduleName) throws FileNotFoundException, UnsupportedEncodingException {
        if (!funMod.containsKey(moduleName)) {
            ErlangCodeStream ecs = new ErlangCodeStream(new File(destDir, ErlUtil.getModuleName(moduleName)
                    + "_funs.erl"));
            funMod.put(moduleName, ecs);
            ecs.pf("-module(%s).", ErlUtil.getModuleName(moduleName) + "_funs");
            ecs.println("-compile(export_all).");
            ecs.println("-include_lib(\"runtime/include/abs_types.hrl\").");
            ecs.println();
        }

        return funMod.get(moduleName);
    }

    public void close() {
        for (Entry<String, ErlangCodeStream> e : funMod.entrySet())
            e.getValue().close();
        funMod.clear();
    }

    private static final Set<String> RUNTIME_FILES = ImmutableSet.of(
            "src/*",
            "include/*",
            "Emakefile",
            "Makefile",
            "gcstats_as_csv.erl",
            "lib/*"
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
                    // all directories are copied below runtime/
                    String outname = destDir + "/runtime/" + dirname;
                    new File(outname).mkdirs();
                    if (resource instanceof JarURLConnection) {
                        copyJarDirectory(((JarURLConnection) resource).getJarFile(),
                                inname, outname);
                    } else {
                        // TODO: untested; might only copy directory itself.  Check out Files.walkFileTree().
                        Files.copy(new File(inname), new File(outname));
                    }
                    
                } else {
                    is = ClassLoader.getSystemResourceAsStream(RUNTIME_PATH + f);
                    if (is == null)
                        throw new RuntimeException("Could not locate Runtime file:" + f);
                    String outputFile = ("Emakefile".equals(f) || "Makefile".equals(f) || "gcstats_as_csv.erl".equals(f)
                                         ? f
                                         : "runtime/" + f).replace('/', File.separatorChar);
                    File file = new File(destDir, outputFile);
                    file.getParentFile().mkdirs();
                    ByteStreams.copy(is, Files.newOutputStreamSupplier(file));
                }
            }
        } finally {
            if (is != null)
                is.close();
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
}
