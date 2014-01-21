/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.backend.erlang;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.List;
import java.util.Set;

import abs.backend.java.codegeneration.JavaCodeGenerationException;
import abs.frontend.ast.Model;
import abs.frontend.parser.Main;

import com.google.common.collect.ImmutableSet;
import com.google.common.io.ByteStreams;
import com.google.common.io.Files;

public class ErlangBackend extends Main {

    public final static String CHARSET = "UTF-8";

    public static void main(final String... args) {
        try {
            new ErlangBackend().compile(args);
        } catch (Exception e) {
            System.err.println("An error occurred during compilation: " + e.getMessage());
            // if (Arrays.asList(args).contains("-debug")) {
            {
                e.printStackTrace();
            }
            System.exit(1);
        }
    }

    @Override
    public List<String> parseArgs(String[] args) {
        List<String> restArgs = super.parseArgs(args);
        List<String> remainingArgs = new ArrayList<String>();

        for (int i = 0; i < restArgs.size(); i++) {
            String arg = restArgs.get(i);
            if (arg.equals("-erlang")) {
                // nothing to do
            } else {
                remainingArgs.add(arg);
            }
        }

        return remainingArgs;
    }

    private static final File DEFAULT_DEST_DIR = new File("gen/erl/");

    @Override
    protected void printUsage() {
        super.printUsage();
        System.out.println("Erlang Backend:\n");
    }

    private void compile(String[] args) throws Exception {
        final Model model = parse(args);
        if (model.hasParserErrors() || model.hasErrors() || model.hasTypeErrors())
            return;

        compile(model, DEFAULT_DEST_DIR);
    }

    public static void compile(Model m, File destDir) throws IOException, JavaCodeGenerationException {
        ErlApp erlApp = new ErlApp(destDir);
        m.generateErlangCode(erlApp);
        erlApp.close();
        copyRuntime(destDir);
    }

    private static final Set<String> RUNTIME_FILES = ImmutableSet.of("src/cog.erl", "src/init_task.erl",
            "src/main_task.erl", "src/object.erl", "src/runtime.erl", "src/task.erl", "src/async_call_task.erl",
            "src/builtin.erl", "include/abs_types.hrl", "Emakefile", "Makefile", "lib/rationals.erl", "lib/intar.erl",
            "lib/cmp.erl");
    private static final String RUNTIME_PATH = "abs/backend/erlang/runtime/";

    private static void copyRuntime(File destDir) throws IOException {
        new File(destDir, "ebin").mkdir();
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
