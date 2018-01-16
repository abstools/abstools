package deadlock.analyser.generation;

import java.util.Map;
import java.util.HashMap;

import com.gzoumix.semisolver.constraint.Constraint;
import com.gzoumix.semisolver.term.Term;
import deadlock.analyser.factory.MethodContract;
import deadlock.analyser.factory.Contract;

public class ResultInference {

  private HashMap<String, MethodContract> methods;
  private Constraint constraint;

  private Contract mainCP;
  private Contract mainCF;

  /* Constructor */
  public ResultInference(){
    this.methods = new HashMap<>();
    this.constraint = null;
    this.mainCP = null;
    this.mainCF = null;
    }

  /* Basic Get */

  public Map<String, MethodContract> getMethods(){ return this.methods; }
  public Constraint getConstraint(){ return this.constraint; }
  public Contract getMainContractPresent() { return mainCP; }
  public Contract getMainContractFuture() { return mainCF; }


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
    if(r.getMainContractPresent() != null) {
      setMain(r.getMainContractPresent(), r.getMainContractFuture());
    }
  }

  public void setMain(Contract cp, Contract cf) { this.mainCP = cp; this.mainCF = cf; }

}


