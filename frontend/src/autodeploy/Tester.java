/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package autodeploy;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import javax.xml.parsers.ParserConfigurationException;

import abs.common.NotImplementedYetException;
import abs.common.WrongProgramArgumentException;
import abs.frontend.ast.Model;
import abs.frontend.delta.DeltaModellingException;
import abs.frontend.parser.Main;

public class Tester extends Main {
    
  private String _JSONName = "toto.json";

  public static void main(final String... args) {
    try {
        new Tester().compile(args);
    } catch (NotImplementedYetException e) {
            System.err.println(e.getMessage());
            System.exit(0);
    } catch (Exception e) {
      System.err.println("An error occurred during compilation:\n" + e.getMessage());
      if (Arrays.asList(args).contains("-debug")) { e.printStackTrace(); }
      System.exit(1);  
    }
  }

  private void compile(String[] args)
      throws DeltaModellingException, IOException, WrongProgramArgumentException, ParserConfigurationException,FileNotFoundException {
    final Model model = this.parse(args);
    // the extraction of the cost annotations can proceed even if the code
    // is not type safe.
    // This is exploited in the SmartDeploy code generator since this tool takes
    // in input a program using some classes that are not defined (they will be
    // added later with a delta).
		if (model.hasParserErrors() || model.hasErrors() ) return;
    if (verbose) { System.out.println("Starting Dependency information extraction..."); }
    DeployInformation di = new DeployInformation();
    di.extractInformation(model);
    if (verbose) { System.out.println("Starting JSON generation..."); }
    PrintWriter f = new PrintWriter(new File(_JSONName));
    di.generateJSON(f);
    f.close();
  }



  @Override
  public List<String> parseArgs(String[] args) {
    List<String> restArgs = super.parseArgs(args);
    List<String> remainingArgs = new ArrayList<String>();
    for (int i = 0; i < restArgs.size(); i++) {
      String arg = restArgs.get(i);
      if (arg.startsWith("-JSON=")){
        try{ _JSONName = arg.split("=")[1]; }
        catch (Exception e) {
          System.err.println("The number of iterations (-it) should be an integer");
          System.exit(1);
        }
      } else { remainingArgs.add(arg); }
    }
    return remainingArgs;
  }
    
  @Override
  protected void printUsage() {
    super.printUsage();
    System.out.println("Deadlock analyzer:\n  -JSON=<var>     name of the generated JSON file\n");
  }

}


