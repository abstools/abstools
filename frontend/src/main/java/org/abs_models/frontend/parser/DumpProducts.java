/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved.
 * This file is licensed under the terms of the Modified BSD License.
 *
 * Prints all defined products, separated by a space for use e.g. in shell scripts.
 * @author stolz
 */
package org.abs_models.frontend.parser;

import java.io.IOException;
import java.util.Iterator;

import org.abs_models.backend.common.InternalBackendException;
import org.abs_models.common.WrongProgramArgumentException;
import org.abs_models.frontend.ast.Model;
import org.abs_models.frontend.ast.ProductDecl;
import org.abs_models.frontend.delta.DeltaModellingException;

public class DumpProducts extends Main {

    @Override
    public Model parse(final String[] args) throws DeltaModellingException, IOException, WrongProgramArgumentException, InternalBackendException {
        Model m = super.parse(args);
        if (m.hasParserErrors()) {
            // Main.parse() already printed a list of parse errors in this case.
            throw new ParseException("Can't parse input.");
        }
        Iterator<ProductDecl> pi = m.getProductDecls().iterator();
        while (pi.hasNext()) {
            System.out.print(pi.next().getName());
            if (pi.hasNext())
                System.out.print(' ');
        }
        return m;
    }

    public static void main(final String... args)  {
        new DumpProducts().mainMethod(args);
    }

    public static void printUsage() {
        printHeader();
        System.out.println(""
                + "Usage: java DumpProducts"
                + " [options] <absfiles>\n\n"
                + "  <absfiles>     ABS files/directories/packages to parse\n\n"
                + "Prints all defined products, separated by a space for use e.g. in shell scripts.");
    }
}
