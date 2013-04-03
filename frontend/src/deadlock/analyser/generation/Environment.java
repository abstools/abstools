package deadlock.analyser.generation;

import java.util.Map;
import java.util.HashMap;

import deadlock.constraints.term.Term;
import deadlock.constraints.constraint.Constraint;

import deadlock.analyser.factory.Factory;
import deadlock.analyser.factory.Record;
import deadlock.analyser.factory.RecordPresent;
import deadlock.analyser.factory.MethodInterface;

public class Environment {	
  private Map<String, MethodInterface> methods;
  private Map<String, Record> variables;

  /* Constructor */
  public Environment() {
    methods = new HashMap<String, MethodInterface>(); 
    variables = new HashMap<String, Record>(); 
  }
	
  public Environment clone() {
    Environment res = new Environment();
    res.variables.putAll(this.variables);
    res.methods.putAll(this.methods); return res; }

  /* Basic Get */
  public MethodInterface getMethod(String className, String methodName) { return methods.get(className + "." + methodName); }
  public Record getVariable(String name) {
    Record res = variables.get(name);
    if(res == null) {
      RecordPresent t = (RecordPresent)(variables.get("this"));
      res = t.getField(name);
    }
   return res; 
  }

  public boolean isField(String name) { return (variables.get(name) == null); }

  /* Basic Extension */
  public void putMethod(String className, String methodName, MethodInterface mi) { methods.put(className + "." + methodName, mi); }
  public void putVariable(String name, Record record) { variables.put(name, record); }

  public void add(Environment e) {
    this.variables.putAll(e.variables);
    this.methods.putAll(e.methods);
  }

  public void updateValues(Environment e) {
    for(Map.Entry<String, Record> entry : e.variables.entrySet()) {
      if(this.variables.containsKey(entry.getKey())) { this.variables.put(entry.getKey(), entry.getValue()); }
    }
    for(Map.Entry<String, MethodInterface> entry : e.methods.entrySet()) {
      if(this.methods.containsKey(entry.getKey())) { this.methods.put(entry.getKey(), entry.getValue()); }
    }
  }

  public Constraint unify(Factory df, ASTNodeInformation info, Environment e) { // we perform the control only on variables
    Constraint c = df.newConstraint(); Record tmp;
    for(Map.Entry<String, Record> entry : e.variables.entrySet()) {
      tmp = this.variables.get(entry.getKey());
      if(tmp != null) { c.addEquation(info, tmp, entry.getValue()); }
    }
    return c;
  }

  /* toString */
  public String toString(){
    String res = "";
    for(Map.Entry<String, Record> e : variables.entrySet()){
      res = res + "\t" + e.getKey() + " := " + e.getValue().toString() + "\n";
    }
    for(Map.Entry<String, MethodInterface> e : methods.entrySet()){
      res = res + "\t" + e.getKey() + " := " + e.getValue().toString() + "\n";
    }
    return res;
  }

} // end class Environment


