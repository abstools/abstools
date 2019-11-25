/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved.
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.abs_models.backend.autodeploy;

import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.PrintWriter;

import org.abs_models.Absc;
import org.abs_models.backend.common.InternalBackendException;
import org.abs_models.common.NotImplementedYetException;
import org.abs_models.common.WrongProgramArgumentException;
import org.abs_models.frontend.ast.Model;
import org.abs_models.frontend.delta.DeltaModellingException;
import org.abs_models.frontend.parser.Main;


public class Tester extends Main {

  public static int doMain(Absc args) {
      int result = 0;
      Tester tester = new Tester();
      tester.arguments = args;
      try {
          result = tester.compile();
      } catch (NotImplementedYetException e) {
            System.err.println(e.getMessage());
            result = 1;
      } catch (Exception e) {
          System.err.println("An error occurred during compilation:\n" + e.getMessage());
          if (args.debug) { e.printStackTrace(); }
          result = 1;
      }
      return result;
  }

  private int compile()
      throws DeltaModellingException, IOException, WrongProgramArgumentException, FileNotFoundException, InternalBackendException {
      // NOTE: we don't handle "-JSON=..." argument any more; all
      // callers of the shell script must switch to "-o"
      final Model model = this.parse(arguments.files);
      // the extraction of the cost annotations can proceed even if
      // the code is not type safe.  This is exploited in the
      // SmartDeploy code generator since this tool takes in input a
      // program using some classes that are not defined (they will be
      // added later with a delta).
      if (model == null || model.hasParserErrors()
          // || model.hasErrors()
          ) return 1;
      if (arguments.verbose) {
          System.out.println("Starting Dependency information extraction...");
      }
      DeployInformation di = new DeployInformation();
      di.extractInformation(model);
      if (arguments.verbose) {
          System.out.println("Starting JSON generation...");
      }
      if (arguments.outputfile != null) {
          PrintWriter f = new PrintWriter(arguments.outputfile);
          di.generateJSON(f);
          f.close();
      } else {
          di.generateJSON(new PrintWriter(System.out));
      }
      return 0;
  }
}


