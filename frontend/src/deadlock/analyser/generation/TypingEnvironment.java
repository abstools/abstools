package deadlock.analyser.generation;

import java.util.LinkedList;
import java.util.Map;
import java.util.HashMap;

import abs.frontend.ast.ASTNode;
import abs.frontend.typechecker.DataTypeType;
import deadlock.constraints.term.Term;
import deadlock.constraints.constraint.Constraint;
import deadlock.analyser.factory.Contract;
import deadlock.analyser.factory.ContractElementParallel;
import deadlock.analyser.factory.Factory;
import deadlock.analyser.factory.FunctionInterface;
import deadlock.analyser.factory.IRecord;
import deadlock.analyser.factory.ITypingEnvironmentFutureType;
import deadlock.analyser.factory.ITypingEnvironmentVariableType;
import deadlock.analyser.factory.RecordPresent;
import deadlock.analyser.factory.MethodInterface;
import deadlock.analyser.factory.TypingEnvironmentFutureTypeUntick;
import deadlock.analyser.factory.TypingEnvironmentVariableTypeFuture;

public class TypingEnvironment {	
  private Map<String, MethodInterface> methods;
  private Map<String, ITypingEnvironmentVariableType> variables;
  private Map<TypingEnvironmentVariableTypeFuture, ITypingEnvironmentFutureType> futures;
  private Map<String, FunctionInterface> functions; //functions
  private Map<String, DataTypeType> types; //DataTypes declarations

  /* Constructor */
  public TypingEnvironment() {
    methods = new HashMap<String, MethodInterface>(); 
    variables = new HashMap<String, ITypingEnvironmentVariableType>(); 
    futures = new HashMap<TypingEnvironmentVariableTypeFuture, ITypingEnvironmentFutureType>();
    functions = new HashMap<>();
    types = new HashMap<>();
  }
	
  public TypingEnvironment clone() {
    TypingEnvironment res = new TypingEnvironment();
    
//    res.variables.putAll(this.variables);
//    res.methods.putAll(this.methods); 
    
    res.add(this);
    
    return res; 
    }

  /* Basic Get */
  public MethodInterface getMethod(String className, String methodName) { return methods.get(className + "." + methodName); }
  public ITypingEnvironmentVariableType getVariable(String name) {
    ITypingEnvironmentVariableType res = variables.get(name);
    if(res == null) {
      RecordPresent t = (RecordPresent)(variables.get("this"));
      res = t.getField(name);
    }
   return res; 
  }

  public boolean isField(String name) { return (variables.get(name) == null); }

  /* Basic Extension */
  public void putMethod(String className, String methodName, MethodInterface mi) { methods.put(className + "." + methodName, mi); }
  public void putVariable(String name, ITypingEnvironmentVariableType record) { variables.put(name, record); }
  public void putFuture(TypingEnvironmentVariableTypeFuture f, ITypingEnvironmentFutureType z) { futures.put(f, z); }
  public void putDataType(String name, DataTypeType type) { types.put(name, type); }
  public void putFunction(String name, FunctionInterface func) { functions.put(name, func); }

  public void add(TypingEnvironment e) {
    this.variables.putAll(e.variables);
    this.methods.putAll(e.methods);
    
    this.functions.putAll(e.functions);
    this.futures.putAll(e.futures);
    this.types.putAll(e.types);
  }

  public void updateValues(TypingEnvironment e) {
    for(Map.Entry<String, ITypingEnvironmentVariableType> entry : e.variables.entrySet()) {
      if(this.variables.containsKey(entry.getKey())) { this.variables.put(entry.getKey(), entry.getValue()); }
    }
    for(Map.Entry<String, MethodInterface> entry : e.methods.entrySet()) {
      if(this.methods.containsKey(entry.getKey())) { this.methods.put(entry.getKey(), entry.getValue()); }
    }
  }
  // TODO: ARGH, we need to change the rule for the IF, because now, we have contracts in the typing environment.
  // This change is not too complicated but far from trivial, we'll see it later
//  public Constraint unify(Factory df, ASTNodeInformation info, TypingEnvironment e) { // we perform the control only on variables
//    Constraint c = df.newConstraint(); 
//    ITypingEnvironmentVariableType tmp;
//    for(Map.Entry<String, ITypingEnvironmentVariableType> entry : e.variables.entrySet()) {
//      tmp = this.variables.get(entry.getKey());
//      if(tmp != null && tmp instanceof IRecord) { c.addEquation(info, (IRecord)tmp, entry.getValue()); }
//    }
//    return c;
//  }
  // end Problem
  
  /* toString */
  public String toString(){
    String res = "";
    for(Map.Entry<String, ITypingEnvironmentVariableType> e : variables.entrySet()){
      res = res + "\t" + e.getKey() + " := " + e.getValue().toString() + "\n";
    }
    for(Map.Entry<String, MethodInterface> e : methods.entrySet()){
      res = res + "\t" + e.getKey() + " := " + e.getValue().toString() + "\n";
    }
    return res;
  }

  public ITypingEnvironmentFutureType getFuture(TypingEnvironmentVariableTypeFuture fut){
      return futures.get(fut);
  }
  
public IRecord getVariableRecord(String name) {
    ITypingEnvironmentVariableType var = getVariable(name);
    
    return (var instanceof IRecord)?(IRecord)var : getFuture((TypingEnvironmentVariableTypeFuture)var).getRecord();
}

public ContractElementParallel unsync(ASTNode pos) {
  LinkedList<Contract> contracts = new LinkedList<Contract>();
  Contract c;
  
 for(ITypingEnvironmentFutureType t: this.futures.values()) {
     if(t instanceof TypingEnvironmentFutureTypeUntick) {
         c = new Contract();
         c.getSubTerms().add(((TypingEnvironmentFutureTypeUntick)t).getContract());
         contracts.add(c);
     }
 }
  return new ContractElementParallel(pos, contracts);
}

  public IRecord getRecord(ITypingEnvironmentVariableType x) {
      return (x instanceof IRecord)? (IRecord)x:
          this.getFuture(((TypingEnvironmentVariableTypeFuture)x)).getRecord();
  }

  public DataTypeType getBoolType() {
    return types.get("Bool");
  }

  public DataTypeType getIntType() {
    return types.get("Int");
  }

  public DataTypeType getUnitType() {
    return types.get("Unit");
  }

   public DataTypeType getStringType() {
    return types.get("String");
  }

  public FunctionInterface getFunction(String constructor) {
    return functions.get(constructor);
  }


} // end class Environment


