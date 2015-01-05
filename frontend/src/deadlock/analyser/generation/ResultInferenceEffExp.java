package deadlock.analyser.generation;

import com.gzoumix.semisolver.constraint.Constraint;
import com.gzoumix.semisolver.term.Term;
import deadlock.analyser.factory.IRecord;
import deadlock.analyser.factory.Contract;
import deadlock.analyser.factory.ITypingEnvironmentVariableType;

public class ResultInferenceEffExp {


  private ITypingEnvironmentVariableType record;
  private Contract contract;
  private Constraint constraint;
  private TypingEnvironment environment;


  public ResultInferenceEffExp( ITypingEnvironmentVariableType res, Contract contract, Constraint constraint, TypingEnvironment env) {

    this.record = res;
    this.contract = contract;
    this.constraint = constraint;
    this.environment = env;
  }


  public ITypingEnvironmentVariableType getRecord() { return this.record; }
  public Contract getContract() { return this.contract; }
  public Constraint getConstraint() { return this.constraint; }
  public TypingEnvironment getEnvironment() { return this.environment; }


}

