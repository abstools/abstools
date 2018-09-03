package deadlock.analyser.generation;

import java.util.LinkedList;
import java.util.List;

import com.gzoumix.semisolver.constraint.Constraint;
import com.gzoumix.semisolver.term.Term;
import deadlock.analyser.factory.IRecord;
import deadlock.analyser.factory.Contract;

public class ResultInferenceStmt {


  private Contract contract;
  private Constraint constraint;
  private List<TypingEnvironment> environment;

  /* Constructor */
  public ResultInferenceStmt(Contract contract, Constraint constraint, TypingEnvironment env) {

    //this.record = res;
    this.contract = contract;
    this.constraint = constraint;
    this.environment = new LinkedList<>();
    environment.add(env);
  }

  /* Constructor */
  public ResultInferenceStmt(Contract contract, Constraint constraint, List<TypingEnvironment> envList) {

    //this.record = res;
    this.contract = contract;
    this.constraint = constraint;
    this.environment = new LinkedList<>();
    environment.addAll(envList);
  }

  /* Basic Get */

  //public Record getRecord() { return this.record; }
  public Contract getContract() { return this.contract; }
  public Constraint getConstraint() { return this.constraint; }

  public List<TypingEnvironment> getEnvironment/*List*/() { return this.environment; }

  //public TypingEnvironment getEnvironment() { return this.environment.get(0); }


}

