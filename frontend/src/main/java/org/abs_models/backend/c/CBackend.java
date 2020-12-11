package org.abs_models.backend.c;

import org.abs_models.Absc;
import org.abs_models.backend.c.codegen.CProject;
import org.abs_models.backend.common.InternalBackendException;
import org.abs_models.backend.java.JavaBackend;
import org.abs_models.common.NotImplementedYetException;
import org.abs_models.common.WrongProgramArgumentException;
import org.abs_models.frontend.ast.Model;
import org.abs_models.frontend.parser.Main;

import java.io.File;
import java.io.IOException;

public class CBackend extends Main {
    public static int doMain(Absc args) {
        int result = 0;
        CBackend backEnd = new CBackend();
        backEnd.arguments = args;
        try {
            result = backEnd.compile();
        } catch (NotImplementedYetException e) {
            System.err.println(e.getMessage());
            result = 1;
        } catch (Exception e) {
            System.err.println("An error occurred during compilation:\n" + e.getMessage());

            if (backEnd.arguments.debug) {
                e.printStackTrace();
            }
            result = 1;
        }
        return result;
    }

    private int compile() throws WrongProgramArgumentException, InternalBackendException, IOException {
        final Model model = parse(arguments.files);
        if (model.hasParserErrors() || model.hasErrors() || model.hasTypeErrors()) {
            printErrorMessage();
            return 1;
        }

        File outdir = arguments.destDir;
        if (outdir.getPath().equals("gen")) {
            // KLUDGE: "gen/" is the default path for java and erlang;
            // keep old erlang behavior of defaulting to "gen/erl/".
            // Note that we can't generate directly into "gen/" since
            // we don't know if the user explicitly specified "gen/"
            // or didn't say anything about the output directory
            outdir = new File("gen/c/");
        }

        compile(model, outdir);
        return 0;
    }

    public CProject compile(Model model, File outdir) throws IOException {
        CProject project = new CProject(outdir);
        project.copyFromResources("/c");
        project.writeMain();
        return project;
    }
}

