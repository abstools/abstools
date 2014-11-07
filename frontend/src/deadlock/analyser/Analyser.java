/**************************************************************************/
/*  Implementation of a simple deadlock analysis system                   */
/*  Copyright (C) 2012. Michael Lienhardt and Carlo Garzia                */
/*                                                                        */
/*  This program is free software; you can redistribute it and/or modify  */
/*  it under the terms of the GNU General Public License as published by  */
/*  the Free Software Foundation; version 2 of the License.               */
/*                                                                        */
/*  This program is distributed in the hope that it will be useful, but   */
/*  WITHOUT ANY WARRANTY; without even the implied warranty of            */
/*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     */
/*  General Public License for more details.                              */
/*                                                                        */
/*  You should have received a copy of the GNU General Public License     */
/*  along with this program; if not, write to the Free Software           */
/*  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA         */
/*  02110-1301 USA                                                        */
/*                                                                        */
/**************************************************************************/

package deadlock.analyser;


import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.HashMap;
import java.util.ArrayList;
import java.io.PrintStream;

import abs.frontend.ast.Model;
import abs.frontend.ast.InterfaceDecl;
import abs.frontend.ast.ClassDecl;
import deadlock.analyser.factory.*;
import deadlock.constraints.term.*;
import deadlock.analyser.generation.*;
import deadlock.analyser.inference.ContractInference;
import deadlock.analyser.detection.*;
import deadlock.constraints.constraint.*;
import deadlock.constraints.substitution.*;

public class Analyser {
    
private static int FixPoint1_0 = 1;
private static int FixPoint2_0 = 2;
    
  public void deadlockAnalysis(Model m, boolean verbose, int nbIteration, int fixPointVersion, PrintStream out) {
      
      Variable.varCounter =0;
      Long totalTimeInMs = 0L;
      
      
    
    /* 0, Create the initial data */
    ContractInference ci = new ContractInference();
    Factory df = new Factory(verbose);
    Map<InterfaceDecl, ClassDecl> mapInterfaceToClass = ci.getMapInterfaceToClass(m);
    TypingEnvironment g = ci.environment(m, df, mapInterfaceToClass, verbose);

     /* 1. Generate contracts */
    String ident = null; 
    if(verbose) { out.println("Analyzing dependencies  to look for deadlocks..."); ident = ""; }
    
    Long nanoTime = System.nanoTime();
    Long ellapsedTime = (System.nanoTime() - nanoTime) / 1000000L;
    totalTimeInMs += ellapsedTime;
    ResultInference InferenceOutput = ci.typeInference(m, ident, g, df, mapInterfaceToClass);
    Map<String, MethodContract> methodMap = InferenceOutput.getMethods();
    deadlock.constraints.constraint.Constraint c = InferenceOutput.getConstraint();
    
    
    if(verbose) {
      out.println("###############################################################\n");
      out.println("Contract and Constraint generation finished...");
      out.println("Ellapsed time: " + ellapsedTime + "ms");
      out.println("  Initial constraint:\n  -------------------");
      out.println(InferenceOutput.getConstraint().toString() + "\n");
      out.println("  Initial contracts:\n  -----------------");
      Term contract;
      for(String k : methodMap.keySet()){
        contract = methodMap.get(k);
        out.println("    \"" + k + "\": " + ((contract != null) ? (contract.toString()) : ("null")));
      }
      out.println("###############################################################\n");
      out.println("Solving constraint...");
      //c.setDebugFile(System.out);
    }

    // 1.2. Solve the constraint and check for errors
    nanoTime = System.nanoTime();
    c.solve();
    ellapsedTime = (System.nanoTime() - nanoTime) / 1000000L;
    totalTimeInMs += ellapsedTime;
    
    if(verbose && (!c.getErrors().isEmpty())) {
      out.println("Generation of Contract failed: constraint not satisfiable");
      out.println("###############################################################\n");
    }
    else if(verbose)
    {
        out.println("Constraint solving completed");
        out.println("Ellapsed time: " + ellapsedTime + "ms");
        out.println("###############################################################\n");
    }

    ArrayList<GenerationError> errors = new ArrayList<GenerationError>(c.getErrors().size());
    for(SolvingError err : c.getErrors()) {
      if(err instanceof SolvingErrorLoop) { errors.add(new ErrorLoop((SolvingErrorLoop)err)); }
      else { errors.add(new ErrorUnif((SolvingErrorUnif)err)); }
    }

    for(GenerationError err : errors) { System.err.println(err.getHelpMessage()); }
    if(!errors.isEmpty()) { return; }


    // 1.3. apply it to the contracts
    nanoTime = System.nanoTime();
    Substitution s = c.getSubstitution();

    for(String k : methodMap.keySet()){
      MethodContract mc = (MethodContract)s.apply(methodMap.get(k));
      //mc.clean();
      if(methodMap.get(k) != null)   methodMap.put(k, mc);
    }
    ellapsedTime = (System.nanoTime() - nanoTime) / 1000000L;
    totalTimeInMs += ellapsedTime;
    
    if(verbose) {
      out.println("###############################################################\n");
      out.println("Substitution completed");
      out.println("Ellapsed time: " + ellapsedTime + "ms");
      
        
      out.println("###############################################################\n");
      out.println("Contract and Constraint computation finished...");
      out.println("  Constraint:\n  -----------");
      out.println(c.toString() + "\n");
      out.println("  Substitution:\n  -------------");
      out.println(s.toString() + "\n");
      out.println("  Contracts:\n  ---------");
      for(Map.Entry<String, MethodContract> entry : methodMap.entrySet()){
        out.println("    \"" + entry.getKey() + "\": " + entry.getValue());
      }
      out.println("###############################################################\n");
      out.println("Initiating contract analysis");
    }

    /* 2. Analyze the contract */
    
    //if(verbose) out.println("Creating CCT...");
    //nanoTime = System.nanoTime();
//    Map<String, Term> cct = new HashMap<String, Term>();
    
//    for(String k : methodMap.keySet()){
//      if(methodMap.get(k) != null){   
//          cct.put(k, methodMap.get(k));
//      }
//    }
    if(verbose) out.println("Applying substitution to Main Contract...");
    
    //cct.put("Main.main", s.apply(InferenceOutput.getMainContractPresent()));
    MainMethodContract mmc = new MainMethodContract((Contract)s.apply(InferenceOutput.getMainContractPresent()), (Contract)s.apply(InferenceOutput.getMainContractFuture()));
//    List<Contract> mainContracts = new LinkedList<Contract>();
//    mainContracts.add(InferenceOutput.getMainContractPresent());
//    mainContracts.add(InferenceOutput.getMainContractFuture());
//    cct.put("Main.main", s.apply(df.newContractSequence(mainContracts)));
    
    ellapsedTime = (System.nanoTime() - nanoTime)/1000000L;
    totalTimeInMs += ellapsedTime;
//    
    if(verbose) {
        out.println("CCT creation completed");
        out.println("Ellapsed time: " + ellapsedTime + "ms");
        out.println("*****CONTRACTS*******");
        Term contract;
        for(String k : methodMap.keySet()){
            contract = methodMap.get(k);
            out.println("    \"" + k + "\": " + ((contract != null) ? (contract.toString()) : ("null")));
        }
        out.println("    \"MAIN.main\": " + mmc.toString());
        out.println("*****END CONTRACTS*******");
    }
//    
    if(verbose){
        out.println("###############################################################\n");
        out.println("Computing Dependencies...");
    }
    
    nanoTime = System.nanoTime();
    
    DASolver solver = (fixPointVersion == FixPoint2_0)? new FixPointSolver2(df, methodMap, mmc):new FixPointSolver1(df, methodMap, mmc, nbIteration);
    

    solver.computeSolution();
    ellapsedTime = (System.nanoTime() - nanoTime) / 1000000L;
    totalTimeInMs += ellapsedTime;
    
    if(verbose) {
        out.println("Dependency analysis completed");
        out.println("Ellapsed time: " + ellapsedTime + "ms");
    }
            
    out.println("### LOCK INFORMATION RESULTED BY THE ANALYSIS ###\n");

      out.println("Possible Deadlock in Main:    " + solver.isDeadlockMain());
      out.println("Current Version:              " + solver.getName() );
      if(fixPointVersion == FixPoint1_0){
        out.println("Saturation:                   " + ((FixPointSolver1)solver).isSatured());
        out.println("Possible Livelock in Main:    " + ((FixPointSolver1)solver).isLivelockMain());
      }
    out.println("Analysis Duration:            " + totalTimeInMs + "ms");
    }

}


