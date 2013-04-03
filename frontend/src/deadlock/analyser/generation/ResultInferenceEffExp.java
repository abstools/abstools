package deadlock.analyser.generation;

import deadlock.constraints.constraint.Constraint;
import deadlock.constraints.term.Term;
import deadlock.analyser.factory.Record;
import deadlock.analyser.factory.Contract;

public class ResultInferenceEffExp {

  String id;
  private Record record;
  private Contract contract;
  private Constraint constraint;
  private Environment environment;


  public ResultInferenceEffExp(String id, Record res, Contract contract, Constraint constraint, Environment env) {
    this.id = id;
    this.record = res;
    this.contract = contract;
    this.constraint = constraint;
    this.environment = env;
  }

/*
  public ResultInferenceEffExp(ResultInferenceEffExp c) {
    this.id = null;
    this.record = c.getRecord();
    this.contract = c.getContract();
    this.constraint = c.getConstraint();
    this.environment = c.getEnvironment();
  }
*/
  public String getId() { return this.id; }
  public Record getRecord() { return this.record; }
  public Contract getContract() { return this.contract; }
  public Constraint getConstraint() { return this.constraint; }
  public Environment getEnvironment() { return this.environment; }

/*
public void setId(String name){
this.id = name;
}
public void setRecord(Record r){
this.record = r;
}
public void setContract(Contract c){
this.contract = c;
}
public void setConstraint(Constraint c){
this.constraint = c;
}


public void setEnvironment(Environment e){
this.environment = e;
}
*/	
}

