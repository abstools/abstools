/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved.
 * This file is licensed under the terms of the Modified BSD License.
 */
package autodeploy;

import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.PrintStream;
import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.List;

import org.abs_models.Absc;
import org.abs_models.backend.common.InternalBackendException;
import org.abs_models.common.NotImplementedYetException;
import org.abs_models.common.WrongProgramArgumentException;
import org.abs_models.frontend.ast.Model;
import org.abs_models.frontend.delta.DeltaModellingException;
import org.abs_models.frontend.parser.Main;


public class Tester extends Main {

  public static int doMain(Absc args) {
      Tester tester = new Tester();
      tester.arguments = args;
      try {
          tester.compile();
      } catch (NotImplementedYetException e) {
            System.err.println(e.getMessage());
            return 1;
      } catch (Exception e) {
          System.err.println("An error occurred during compilation:\n" + e.getMessage());
          if (args.debug) { e.printStackTrace(); }
          return 1;
      }
      return 0;
  }

  private void compile()
      throws DeltaModellingException, IOException, WrongProgramArgumentException, FileNotFoundException, InternalBackendException {
      // FIXME: we don't handle "-JSON=..." argument; should switch to "-o"
      final Model model = this.parse(arguments.files);
      // the extraction of the cost annotations can proceed even if
      // the code is not type safe.  This is exploited in the
      // SmartDeploy code generator since this tool takes in input a
      // program using some classes that are not defined (they will be
      // added later with a delta).
      if (model.hasParserErrors() || model.hasErrors() ) return;
      if (arguments.verbose) {
          System.out.println("Starting Dependency information extraction...");
      }
      DeployInformation di = new DeployInformation();
      di.extractInformation(model);
      if (arguments.verbose) {
          System.out.println("Starting JSON generation...");
      }
      PrintStream stream = System.out;
      if (arguments.outputfile != null) {
          stream = new PrintStream(arguments.outputfile);
      }
      // PrintWriter f = new PrintWriter(new File(_JSONName));
      di.generateJSON(new PrintWriter(stream));
      stream.close();
  }



  @Override
  public List<String> parseArgs(String[] args) throws InternalBackendException {
    List<String> restArgs = super.parseArgs(args);
    List<String> remainingArgs = new ArrayList<>();
    for (int i = 0; i < restArgs.size(); i++) {
      String arg = restArgs.get(i);
      if (arg.startsWith("-JSON=")){
        // try{ _JSONName = arg.split("=")[1]; }
        // catch (Exception e) {
        //   System.err.println("The number of iterations (-it) should be an integer");
        //   System.exit(1);
        // }
      } else { remainingArgs.add(arg); }
    }
    return remainingArgs;
  }


  public static void printUsage() {
    Main.printUsage();
    System.out.println("Deadlock analyzer:\n  -JSON=<var>     name of the generated JSON file\n");
  }

}


