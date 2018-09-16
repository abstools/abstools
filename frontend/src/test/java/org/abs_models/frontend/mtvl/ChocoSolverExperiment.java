/*
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved.
 * This file is licensed under the terms of the Modified BSD License.
 */

package org.abs_models.frontend.mtvl;

import java.text.MessageFormat;

import choco.Choco;
import choco.cp.model.CPModel;
import choco.cp.solver.CPSolver;
import choco.kernel.common.logging.ChocoLogging;
import choco.kernel.common.logging.Verbosity;
import choco.kernel.model.constraints.Constraint;
import choco.kernel.model.variables.integer.IntegerVariable;

public class ChocoSolverExperiment {

  @SuppressWarnings("unused")
public static void main(final String[] args) throws Exception {

    CPModel m = new CPModel();
    boolean verbose = false;

    if (!verbose) ChocoLogging.setVerbosity(Verbosity.OFF);

    IntegerVariable i1 = Choco.makeIntVar("i1",-10,100); //m.addVariable(i1);
    IntegerVariable i2 = Choco.makeIntVar("i2",-10,100); //m.addVariable(i2);
    IntegerVariable i3 = Choco.makeIntVar("i3",-10,100); //m.addVariable(i3);
    IntegerVariable i4 = Choco.makeIntVar("i4",-10,100); //m.addVariable(i4);

    IntegerVariable b1 = Choco.makeBooleanVar("b1"); //m.addVariable(b1);
    IntegerVariable b2 = Choco.makeBooleanVar("b2"); //m.addVariable(b2);
    IntegerVariable b3 = Choco.makeBooleanVar("b3"); //m.addVariable(b3);
    IntegerVariable b4 = Choco.makeBooleanVar("b4"); //m.addVariable(b4);
    IntegerVariable b5 = Choco.makeBooleanVar("b5"); //m.addVariable(b5);

    m.addConstraint(
//      Choco.and(Choco.eq(i1, 1), Choco.TRUE)
//      Choco.or(Choco.eq(b1,1),Choco.eq(b2,1)) // b1 && b2
//            Choco.and( Choco.lt(i1, i2) , Choco.lt(i2, 7) )
            Choco.or(Choco.eq(i1,-3), Choco.eq(i1,5))
//       Choco.and(Choco.and(b1),Choco.TRUE)
//       Choco.ifOnlyIf( Choco.and( Choco.leq(i3, 9), Choco.eq(i1, Choco.mult(10, Choco.abs(i2))) ), Choco.TRUE )
    );

    // Build the solver
    CPSolver s = new CPSolver();

    if (verbose)
      System.out.println("####" + s.getConfiguration().stringPropertyNames());

    // print the problem
    if (verbose)
      System.out.println(m.pretty());
    // Read the model
    s.read(m);
    // Solve the model
//     s.solve();
//    s.setObjective(s.getVar(i1))
    Boolean solved = //s.solve(); //
            s.maximize(s.getVar(i1), true);
    if (solved) {
        System.out.println("i1: "+s.getVar(i1).getVal());
//        System.out.println("i2: "+s.getVar(i2).getVal());
    }
    else {
        System.out.println("no sol...");
    }
    try {
//      s.getVar(b1).setVal(1);
////      s.getVar(b2).setVal(0);
////      s.getVar(b3).setVal(1);
//      System.out.println("$$$ check sol: "+s.checkSolution()+" $$$");
//      System.out.println("$$$ b1: "+s.getVar(b1).isInstantiated()+" $$$");
//      System.out.println("$$$ b2: "+s.getVar(b2).isInstantiated()+" $$$");
    }
    catch (Exception e1) {
      // Catch-all
      System.err.println("$$$ Failed to check solution... $$$");
//       e1.printStackTrace();
    }
    if (verbose)
      System.out.println(s.pretty());
  }



  public static void othermain(final String[] args) throws Exception {

    // Constant declaration
    int n = 3; // Order of the magic square
    int magicSum = n * (n * n + 1) / 2; // Magic sum
    // Build the model
    CPModel m = new CPModel();

    // Creation of an array of variables
    IntegerVariable[][] var = new IntegerVariable[n][n];
    // For each variable, we define its name and the boundaries of its domain.
    for (int i = 0; i < n; i++) {
      for (int j = 0; j < n; j++) {
        var[i][j] = Choco.makeIntVar("var_" + i + "_" + j, 1, n * n);
        // Associate the variable to the model.
        m.addVariable(var[i][j]);
      }
    }


    // All cells of the matrix must be different
    for (int i = 0; i < n * n; i++) {
      for (int j = i + 1; j < n * n; j++) {
        Constraint c = (Choco.neq(var[i / n][i % n], var[j / n][j % n])); m.addConstraint(c);
      }
    }

    // All rows must be equal to the magic sum
    for (int i = 0; i < n; i++) {
      m.addConstraint(Choco.eq(Choco.sum(var[i]), magicSum));
    }

    IntegerVariable[][] varCol = new IntegerVariable[n][n];
    for (int i = 0; i < n; i++) {
      for (int j = 0; j < n; j++) { // Copy of var in the column order
        varCol[i][j] = var[j][i];
      }
      // Each column?s sum is equal to the magic sum
      m.addConstraint(Choco.eq(Choco.sum(varCol[i]), magicSum));
    }

    IntegerVariable[] varDiag1 = new IntegerVariable[n]; IntegerVariable[] varDiag2 = new IntegerVariable[n]; for (int i = 0; i < n; i++) {
      varDiag1[i] = var[i][i]; // Copy of var in varDiag1
      varDiag2[i] = var[(n - 1) - i][i]; // Copy of var in varDiag2
    }
    // Every diagonal?s sum has to be equal to the magic sum
    m.addConstraint(Choco.eq(Choco.sum(varDiag1), magicSum)); m.addConstraint(Choco.eq(Choco.sum(varDiag2), magicSum));

    // Build the solver
    CPSolver s = new CPSolver();

    // print the problem
    System.out.println(m.pretty());
    // Read the model
    s.read(m);
    // Solve the model
    s.solve();
    // Print the solution
    for (int i = 0; i < n; i++) {
      for (int j = 0; j < n; j++) {
        System.out.print(MessageFormat.format("{0} ", s.getVar(var[i][j]).getVal()));
      }
      System.out.println();
    }
  }
}
