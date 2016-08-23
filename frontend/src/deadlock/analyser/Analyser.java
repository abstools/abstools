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
import com.gzoumix.semisolver.term.*;
import deadlock.analyser.generation.*;
import deadlock.analyser.inference.ContractInference;
import deadlock.analyser.detection.*;
import com.gzoumix.semisolver.constraint.*;
import com.gzoumix.semisolver.substitution.*;

public class Analyser {
    
  private static int FixPoint1_0 = 1;
  private static int FixPoint2_0 = 2;
    
  public void deadlockAnalysis(Model m, boolean verbose, int nbIteration, int fixPointVersion, PrintStream out) {
    
    
      
    Variable.varCounter =0;
    Long totalTimeInMs = 0L;
    Long nanoTime = System.nanoTime();
    DeadlockPreanalysis p = new DeadlockPreanalysis(m);
    p.analyzeModel();
    Long ellapsedTime = (System.nanoTime() - nanoTime) / 1000000L;
    totalTimeInMs += ellapsedTime;
    if(p.isDeadlockFree()){
        
        out.println("### LOCK INFORMATION RESULTED BY THE ANALYSIS ###\n");

        out.println("Possible Deadlock in Main:    false");
        out.println("Current Version:              no solver was used");
        out.println("Note: necessary conditions for deadlocks were not present");
        out.println();
        out.println("Analysis Duration:            " + totalTimeInMs + "ms");
        return;
    }
      
    
    /* 0, Create the initial data */
    Factory df = new Factory(verbose);
    AnalyserLog log = new AnalyserLog();
    if(verbose) { log.verbose(); }
    ContractInference ci = new ContractInference(log, df, m);


    ci.computeMapInterfaceToClass();
    ci.computeEnvironment();

     /* 1. Generate contracts */
    log.logDebug("Analyzing dependencies  to look for deadlocks...");
    
    nanoTime = System.nanoTime();
    ellapsedTime = (System.nanoTime() - nanoTime) / 1000000L;
    totalTimeInMs += ellapsedTime;
    ResultInference InferenceOutput = ci.typeInference();
    Map<String, MethodContract> methodMap = InferenceOutput.getMethods();
    Constraint c = InferenceOutput.getConstraint();
   
    log.logDebug("###############################################################\n");
    log.logDebug("Contract and Constraint generation finished...");
    log.logDebug("Ellapsed time: " + ellapsedTime + "ms");
    log.logDebug("  Initial constraint:\n  -------------------");
    log.logDebug(InferenceOutput.getConstraint().toString() + "\n");
    log.logDebug("  Initial contracts:\n  -----------------");
    for(Map.Entry<String, MethodContract> entry : methodMap.entrySet()){
      log.logDebug("    \"" + entry.getKey() + "\": " + ((entry.getValue() != null) ? (entry.getValue().toString()) : ("null")));
    }
    log.logDebug("    \"main\": " + ((InferenceOutput.getMainContractPresent() != null) ? ("< " + InferenceOutput.getMainContractPresent() + ", " + InferenceOutput.getMainContractFuture() + ">") : ("null")));
    log.logDebug("###############################################################\n");
    log.logDebug("Solving constraint...");

    // 1.2. Solve the constraint and check for errors
    nanoTime = System.nanoTime();
    c.solve();
    ellapsedTime = (System.nanoTime() - nanoTime) / 1000000L;
    totalTimeInMs += ellapsedTime;
    
    if(!c.getErrors().isEmpty()) {
      log.logDebug("Generation of Contract failed: constraint not satisfiable");
      log.logDebug("###############################################################\n");
    } else {
      log.logDebug("Constraint solving completed");
      log.logDebug("Ellapsed time: " + ellapsedTime + "ms");
      log.logDebug("###############################################################\n");
    }

    ArrayList<GenerationError> errors = new ArrayList<GenerationError>(c.getErrors().size());
    for(SolvingError err : c.getErrors()) {
      if(err instanceof SolvingErrorLoop) { errors.add(new ErrorLoop((SolvingErrorLoop)err)); }
      else { errors.add(new ErrorUnif((SolvingErrorUnif)err)); }
    }

    out.println();
    if(!errors.isEmpty()) {
        out.println("### Analysis failed due to some errors ###");
        out.println();
        int i = 1;
        for(GenerationError err : errors){
            out.println(i++ + ":");
            out.println(err.getHelpMessage()); 
            out.println();
        }
        
        
        return; 
    }


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
      
      if(solver.isDeadlockMain()){
          out.println("Deadlock details: ");
          solver.printDeadlockDetails(out);
      }
      
      if(fixPointVersion == FixPoint1_0){
        out.println("Saturation:                   " + ((FixPointSolver1)solver).isSatured());
        out.println("Possible Livelock in Main:    " + ((FixPointSolver1)solver).isLivelockMain());
      }
    out.println("Analysis Duration:            " + totalTimeInMs + "ms");
    }

}


