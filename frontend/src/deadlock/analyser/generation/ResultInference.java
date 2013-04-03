package deadlock.analyser.generation;

import java.util.Map;
import java.util.HashMap;

import deadlock.constraints.constraint.Constraint;
import deadlock.constraints.term.Term;
import deadlock.analyser.factory.MethodContract;
import deadlock.analyser.factory.Contract;

public class ResultInference {

  HashMap<String, MethodContract> methods;
  Constraint constraint;

  Contract main;

  /* Constructor */
  public ResultInference(){
    this.methods = new HashMap<String, MethodContract>();
    this.constraint = null;
    this.main = null;
  }

  /* Basic Get */

  public Map<String, MethodContract> getMethods(){ return this.methods; }
  public Constraint getConstraint(){ return this.constraint; }
  public Contract getMainContract() { return main; }


  /* Basic Estension */
  public void add(Constraint c) {
    if(this.constraint == null) { this.constraint = c; }
    else if(c != null) { this.constraint.add(c); }	
  }

  public void add(String className, String methodName, MethodContract contract){
    this.methods.put(className + "." + methodName, contract);
  }

  public void add(ResultInference r) {
    this.methods.putAll(r.methods);
    this.add(r.constraint);
  }

  public void setMain(Contract c) { this.main = c; }

}


