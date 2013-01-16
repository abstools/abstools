/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 * 
 * Prints all defined products, separated by a space for use e.g. in shell scripts.
 * @author stolz
 */
package abs.frontend.parser;

import java.io.IOException;
import java.util.Iterator;

import abs.common.WrongProgramArgumentException;
import abs.frontend.ast.Model;
import abs.frontend.ast.Product;
import abs.frontend.delta.exceptions.DeltaModellingException;

public class DumpProducts extends Main {

    @Override
    public Model parse(final String[] args) throws DeltaModellingException, IOException, WrongProgramArgumentException {
        Model m = super.parse(args);
        if (m.hasParserErrors()) {
            // Main.parse() already printed a list of parse errors in this case.
            throw new ParseException("Can't parse input.");
        }
        Iterator<Product> pi = m.getProducts().iterator();
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

    @Override
    protected void printUsage() {
        printHeader();
        System.out.println(""
                + "Usage: java " + this.getClass().getName()
                + " [options] <absfiles>\n\n" 
                + "  <absfiles>     ABS files/directories/packages to parse\n\n"
                + "Prints all defined products, separated by a space for use e.g. in shell scripts.");
    }
}
