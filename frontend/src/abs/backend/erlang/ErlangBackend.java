/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.backend.erlang;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
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

    private File destDir = new File("gen/erl/");
    private boolean sourceOnly = false;

    @Override
    protected void printUsage() {
        super.printUsage();
        System.out.println("Erlang Backend:\n");
    }

    private void compile(String[] args) throws Exception {
        final Model model = parse(args);
        if (model.hasParserErrors() || model.hasErrors() || model.hasTypeErrors())
            return;

        compile(model, destDir);
    }

    private void compile(Model m, File destDir) throws IOException, JavaCodeGenerationException {

        ErlApp erlApp = new ErlApp(destDir);
        if (verbose)
            System.out.println("Generating Erlang code...");
        m.generateErlangCode(erlApp);
        erlApp.close();
        copyRuntime(destDir);
        if (!sourceOnly) {
            if (verbose)
                System.out.println("Compiling generated Java code...");
            // javaCode.compile();
        }
    }

    private static final Set<String> files = ImmutableSet.of("src/cog.erl", "src/init_task.erl", "src/main_task.erl",
            "src/object.erl", "src/runtime.erl", "src/task.erl", "src/async_call_task.erl", "src/builtin.erl",
            "include/abs_types.hrl", "Emakefile", "Makefile");
    private static final String RUNTIME_PATH = "abs/backend/erlang/runtime/";

    private void copyRuntime(File destDir) throws IOException {
        new File(destDir, "ebin").mkdir();
        InputStream is = null;
        try {
            for (String f : files) {
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
