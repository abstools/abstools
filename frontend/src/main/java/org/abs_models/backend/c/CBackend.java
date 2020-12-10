package org.abs_models.backend.c;

import org.abs_models.Absc;
import org.abs_models.backend.common.InternalBackendException;
import org.abs_models.backend.java.JavaBackend;
import org.abs_models.common.NotImplementedYetException;
import org.abs_models.common.WrongProgramArgumentException;
import org.abs_models.frontend.ast.Model;
import org.abs_models.frontend.parser.Main;

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

        throw new NotImplementedYetException(model);
    }
}

