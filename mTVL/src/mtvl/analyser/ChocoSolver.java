/*
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved.
 * This file is licensed under the terms of the Modified BSD License.
 */

package mtvl.analyser;

import mtvl.ast.Model;

import java.text.MessageFormat;
import java.util.Map;
import java.util.HashMap;
import java.util.Set;
import java.util.HashSet;
import java.util.Iterator;
import choco.Choco;
import choco.cp.model.CPModel;
import choco.kernel.model.variables.integer.IntegerVariable;
import choco.kernel.model.constraints.Constraint;
import choco.kernel.model.variables.integer.IntegerExpressionVariable;
import choco.cp.solver.CPSolver;
import choco.kernel.common.logging.ChocoLogging;
import choco.kernel.common.logging.Verbosity;
import choco.kernel.solver.ContradictionException;

public class ChocoSolver {

  public final CPModel m;
  public final CPSolver s;
  public boolean solved = false;
  public final Map<String,IntegerVariable> vars;
  private boolean verbose = false;
  
  
  public ChocoSolver() {
    // Build the model
    m = new CPModel();
    s = new CPSolver();
    ChocoLogging.setVerbosity(Verbosity.SILENT);
    vars = new HashMap<String,IntegerVariable>();
    ChocoLogging.setVerbosity(Verbosity.OFF);
}

  public ChocoSolver(boolean v) {
    this();
    verbose = v;
    if (!verbose) ChocoLogging.setVerbosity(Verbosity.OFF);
  }  
  public void setVerbose(boolean v) {
    verbose = v;
    if (!verbose) ChocoLogging.setVerbosity(Verbosity.OFF);
    else          ChocoLogging.setVerbosity(Verbosity.DEFAULT);
  }

    /** add int variable
     * @param name - name of the variable to be added
     * @param from - lowerlimit of the domain of the variable
     * @param to - upper limit of the domain of the variable **/
  public void addIntVar(String name, int from, int to) {
    IntegerVariable v = Choco.makeIntVar(name, from, to);
    vars.put(name,v);
    //m.addVariable(v);
  }
  public void addIntVar(String name) {
    IntegerVariable v = Choco.makeIntVar(name);
    vars.put(name,v);
    //m.addVariable(v);
  }
  /** add bool variable **/
  public void addBoolVar(String name) {
    IntegerVariable v = Choco.makeBooleanVar(name);
    vars.put(name,v);
    //m.addVariable(v);
  }
  /** set a bool variable to true **/
  public void forceTrue(String name) {
    IntegerVariable v = Choco.makeIntVar(name,1,1);
    vars.put(name,v);
    m.addVariable(v);
  }
  /** add choco constraint **/
  public void addConstraint(Constraint c) {
    m.addConstraint(c);
  }
  
  public IntegerVariable getVar(String var) {
    return (IntegerVariable) vars.get(var);
  }
  
  public boolean solve() {
    // show the problem
    if (verbose) {
      System.out.print("## The constraints:");
      System.out.println(m.pretty());
    }
    
    // Read the model
    s.read(m);
    // Solve the model
    solved = s.solve();
    //solved = true;
    return solved;
  }

  public Map<String,Integer> getSolution() {
    if (!solved) solve();
    
    HashMap<String,Integer> result = new HashMap<String,Integer>();
    
    Iterator<IntegerVariable> it = m.getIntVarIterator();
    while (it.hasNext()) {
       IntegerVariable var = (IntegerVariable) it.next();
       result.put(var.getName(), s.getVar(var).getVal());
    }
    return result;
  }


  public String resultToString() {
    if (!solved) solve();

    if (!solved)
      return "-- No solution found --\n";

    String result = "";

    Iterator<IntegerVariable> it = m.getIntVarIterator();
    while (it.hasNext()) {
      IntegerVariable var = (IntegerVariable) it.next();
      if (verbose || !var.getName().startsWith("$"))
        result = result + var.getName() + " -> "+s.getVar(var).getVal() + "\n";
    }
    return result;
  }


  public boolean checkSolution(Map<String,Integer> solution) {
    return checkSolution(solution,null);
  }
  
  public boolean checkSolution(Map<String,Integer> solution, Model model) {
    // Read the model
    CPSolver s = new CPSolver();
    s.read(m);


    if (verbose)
      System.out.println("solution to check:\n"+solution);
    
//     HashMap<String,Integer> selection = new HashMap<String,Integer>();
    
    Iterator<IntegerVariable> it = m.getIntVarIterator();
    try {
      // aux variables
      int val;
      Set<String> newFeatures = new HashSet<String>();
      Set<String> newParents  = new HashSet<String>();
          
      if (verbose)
        System.out.println("Adding new values:");
      while (it.hasNext()) {
        IntegerVariable var = (IntegerVariable) it.next();
        // IF used variable is present in the solution, update it!
        if (solution.containsKey(var.getName())) {
          val = solution.get(var.getName());
          if (verbose)
            System.out.println("  "+var+" -> "+val);
          s.getVar(var).setVal(val);
          // Possible feature name -- include later the parents.
          if (val==1) newFeatures.add(var.getName());
        }
      }
      // collect parents of 'newFeatures'
      if (model != null)
        model.collectParents(newFeatures,newParents);
      // add newParents and default values to the solution
      it = m.getIntVarIterator();
      while (it.hasNext()) {
        IntegerVariable var = (IntegerVariable) it.next();
        
        // If it is a parent to include, set 
        if (newParents.contains(var.getName())) {
          if (verbose)
            System.out.println("  "+var+" (parent) -> 1");
          s.getVar(var).setVal(1);
        }
        // ELSE use default value
        else if (!solution.containsKey(var.getName())) {
          // By default, the optional wrapper "$..." is ALWAYS true
          if (var.getName().startsWith("$")) {
            if (verbose) System.out.println("  "+var+" (default) -> 1");
            s.getVar(var).setVal(1);
          // By default, unrefered features & attributes are false
          } else {
            if (verbose) System.out.println("  "+var+" (default) -> 0");
            s.getVar(var).setVal(0);
          }
        }
      }
    }
    catch (ContradictionException e1) {
      if (verbose) System.err.println("$$$ Contradiction found... $$$");
    }
    // Catch-all
    catch (Exception e1) {
      // Catch-all
      if (verbose) {
        System.err.println("$$$ Failed to check solution... $$$");
        e1.printStackTrace();
      }
    }

//     if (verbose) {
//       String result = "";
//       it = m.getIntVarIterator();
//       while (it.hasNext()) {
//         IntegerVariable var = (IntegerVariable) it.next();
//         result = result + var.getName() + " -> "+s.getVar(var).getVal() + "\n";
//       }
//       System.out.println("Trying:\n"+result);
//     }
  
    return s.checkSolution();
  }


  public static Constraint eqeq(IntegerVariable v1, IntegerVariable v2) {
    return Choco.eq(v1,v2);
  }
  public static Constraint eqeq(IntegerExpressionVariable v1, IntegerExpressionVariable v2) {
    return Choco.eq(v1,v2);
  }
  public static Constraint eqeq(IntegerExpressionVariable v1, int v2) {
    return Choco.eq(v1,v2);
  }
  public static Constraint isTrue(IntegerExpressionVariable v1) {
    return Choco.eq(v1,1);
  }



  /* *************
  Experiments with the constraint solver!
  ************* */
  
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
      Choco.or(Choco.eq(b1,1),Choco.eq(b2,1)) // b1 && b2
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
    try {
      s.getVar(b1).setVal(1);
//      s.getVar(b2).setVal(0);
//      s.getVar(b3).setVal(1);
      System.out.println("$$$ check sol: "+s.checkSolution()+" $$$");
      System.out.println("$$$ b1: "+s.getVar(b1).isInstantiated()+" $$$");
      System.out.println("$$$ b2: "+s.getVar(b2).isInstantiated()+" $$$");
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
