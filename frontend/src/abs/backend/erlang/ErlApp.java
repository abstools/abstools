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
import java.util.HashMap;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;

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

    public ErlangCodeStream getFunStream(String moduleName) throws FileNotFoundException, UnsupportedEncodingException {
        if (!funMod.containsKey(moduleName)) {
            ErlangCodeStream ecs = new ErlangCodeStream(new File(destDir, ErlUtil.getModuleName(moduleName)
                    + "_funs.erl"));
            funMod.put(moduleName, ecs);
            ecs.pf("-module(%s).", ErlUtil.getModuleName(moduleName) + "_funs");
            ecs.println("-compile(export_all).");
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
            "src/cog.erl",
            "src/init_task.erl",
            "src/main_task.erl",
            "src/object.erl",
            "src/runtime.erl",
            "src/task.erl",
            "src/async_call_task.erl",
            "src/builtin.erl",
	    "src/object_tracker.erl",
	    "src/future.erl",
 	    "src/active_object_task.erl",
	    "src/error_transform.erl",
            "include/abs_types.hrl",
            "include/log.hrl",
            "Emakefile",
            "Makefile",
            "lib/rationals.erl",
            "lib/intar.erl",
            "lib/cmp.erl",
            "lib/console_logger.erl",
            "lib/eventstream.erl",
            "lib/cog_monitor.erl",
            "lib/getopt.erl"
            );
    private static final String RUNTIME_PATH = "abs/backend/erlang/runtime/";

    private void copyRuntime() throws IOException {
        InputStream is = null;
        try {
            for (String f : RUNTIME_FILES) {
                is = ClassLoader.getSystemResourceAsStream(RUNTIME_PATH + f);
                if (is == null)
                    throw new RuntimeException("Could not locate Runtime file:" + f);
                String outputFile = ("Emakefile".equals(f) || "Makefile".equals(f) ? f : "runtime/" + f).replace('/',
                        File.separatorChar);
                File file = new File(destDir, outputFile);
                file.getParentFile().mkdirs();
                ByteStreams.copy(is, Files.newOutputStreamSupplier(file));
            }
        } finally {
            if (is != null)
                is.close();
        }
    }
}
