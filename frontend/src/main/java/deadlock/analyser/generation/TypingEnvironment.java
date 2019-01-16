package deadlock.analyser.generation;

import java.util.List;
import java.util.LinkedList;
import java.util.Map;
import java.util.HashMap;
import java.util.Set;
import java.util.HashSet;

import org.abs_models.frontend.ast.ASTNode;
import deadlock.analyser.factory.DataTypeInterface;
import deadlock.analyser.factory.Contract;
import deadlock.analyser.factory.ContractElementParallel;
import deadlock.analyser.factory.FunctionInterface;
import deadlock.analyser.factory.IRecord;
import deadlock.analyser.factory.ITypingEnvironmentFutureType;
import deadlock.analyser.factory.ITypingEnvironmentVariableType;
import deadlock.analyser.factory.RecordPresent;
import deadlock.analyser.factory.MethodInterface;
import deadlock.analyser.factory.TypingEnvironmentFutureTypeUntick;
import deadlock.analyser.factory.TypingEnvironmentVariableTypeFuture;

public class TypingEnvironment {

  private static final String _connector = ".";


  // 1. Fields for declarations
  private Map<String, MethodInterface> methods; // Methods
  private Map<String, FunctionInterface> functions; //Functions
  private Map<String, DataTypeInterface> types; //DataTypes


  // 2. fields for inference of methods
  private Set<String>searchPath;

  private List<Map<String, ITypingEnvironmentVariableType>> variables;  // stack frame of mapping variable => record or future
  private Map<TypingEnvironmentVariableTypeFuture, ITypingEnvironmentFutureType> futures;  // mapping future => checked or unchecked



  /* Constructor */
  public TypingEnvironment() {
    methods    = new HashMap<>();
    functions  = new HashMap<>();
    types      = new HashMap<>();
    searchPath = new HashSet<>();
    variables  = new LinkedList<>();
    futures    = new HashMap<>();
  }

  public TypingEnvironment copy() {
    TypingEnvironment res = new TypingEnvironment();
    res.methods = this.methods;
    res.functions = this.functions;
    res.types = this.types;
    res.searchPath = this.searchPath;
    res.variables = new LinkedList<>(this.variables);
    res.futures = new HashMap<>(this.futures);
    return res;
  }


  /* Basic Gets */
  // declarations
  public MethodInterface getMethod(String className, String methodName) {
    MethodInterface res = null;
    for(String moduleName: this.searchPath) {
      System.out.println("checking with \"" + moduleName + _connector + className + _connector + methodName + "\"");
      res = methods.get(moduleName + _connector + className + _connector + methodName);
      if(res != null) return res;
    }
    return null;
  }

  public FunctionInterface getFunction(String function) {
    FunctionInterface res = null;
    for(String moduleName: this.searchPath) {
      res = functions.get(moduleName + _connector + function);
      if(res != null) return res;
    }
    return null;
  }

  public DataTypeInterface getDataType(String dataType) {
    DataTypeInterface res = null;
    for(String moduleName: this.searchPath) {
      res = types.get(moduleName + _connector + dataType);
      if(res != null) return res;
    }
    return null;
  }


  // Variables
  public ITypingEnvironmentVariableType getVariable(String name) {
    ITypingEnvironmentVariableType res = null;
    for(Map<String, ITypingEnvironmentVariableType> map: variables) {
      res = map.get(name);
      if(res != null) return res;
    }
    if((res == null) && (!(name.equals("this")))) {
      RecordPresent t = (RecordPresent)(getVariable("this"));
      assert t != null : name;
      res = t.getField(name); // possible nullPointerException
    }
    return res;
  }

  public IRecord getVariableRecord(String name) {
    ITypingEnvironmentVariableType var = this.getVariable(name);

    return (var instanceof IRecord)?(IRecord)var : this.getFuture((TypingEnvironmentVariableTypeFuture)var).getRecord();
  }

  public boolean isField(String name) {
    boolean res = true;
    for(Map<String, ITypingEnvironmentVariableType> map: variables) {
      res = (map.get(name) == null);
      if(!res) return false;
    }
    return true;
  }


  // Other gets
  public ITypingEnvironmentFutureType getFuture(TypingEnvironmentVariableTypeFuture fut){
      return futures.get(fut);
  }

  public ContractElementParallel unsync(ASTNode<?> pos) {
    LinkedList<Contract> contracts = new LinkedList<>();
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
      if (x instanceof IRecord)
          return (IRecord)x;
      else {
          final ITypingEnvironmentFutureType f = getFuture(((TypingEnvironmentVariableTypeFuture)x));
          assert f != null : "I want the future now: "+x;
          return f.getRecord();
      }
  }



  /* Basic Extension */
  public void putMethod(String moduleName, String className, String methodName, MethodInterface mi) { methods.put(moduleName + _connector + className + _connector + methodName, mi); }
  public void putFunction(String moduleName, String name, FunctionInterface func)                   { functions.put(moduleName + _connector + name, func); }
  public void putDataType(String moduleName, String name, DataTypeInterface type)                        { types.put(moduleName + _connector + name, type); }

  public void newScope() { variables.add(0, new HashMap<>()); }
  public void popScope() { variables.remove(0); }
  public void putVariable(String name, ITypingEnvironmentVariableType record) { variables.get(0).put(name, record); }
  public void putFuture(TypingEnvironmentVariableTypeFuture f, ITypingEnvironmentFutureType z) { futures.put(f, z); }

  public void clearFutures() { futures = new HashMap<>(); }
  public void clearVariables() { variables = new LinkedList<>(); }

  public void addSearchPath(String name) { searchPath.add(name); }
  public void resetSearchPath() { searchPath = new HashSet<>(); }
  public void setSearchPath(Set<String> path) { searchPath = path; }



/*
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
  }*/

  /* toString */
  public String toStringSearchPath() {
    String res = "{ ";
    for(String s: searchPath) { res = res + s + " "; } res = res + "}";
    return res;
  }

  public String toStringVariables() {
    String res = "";
    Set<String> checkedVariables = new HashSet<>();
    for(Map<String, ITypingEnvironmentVariableType> el: variables) {
      for(Map.Entry<String, ITypingEnvironmentVariableType> e : el.entrySet()){
        if(!checkedVariables.contains(e.getKey())) {
          res = res + "\t" + e.getKey() + " := " + e.getValue().toString() + "\n";
          checkedVariables.add(e.getKey());
        }
      }
    }
    return res;
  }

  public String toString() {
    String res = "{ ";
    for(String s: searchPath) { res = res + s + " "; } res = res + "}\n";
    Set<String> checkedVariables = new HashSet<>();
    for(Map<String, ITypingEnvironmentVariableType> el: variables) {
      for(Map.Entry<String, ITypingEnvironmentVariableType> e : el.entrySet()){
        if(!checkedVariables.contains(e.getKey())) {
          res = res + "\t" + e.getKey() + " := " + e.getValue().toString() + "\n";
          checkedVariables.add(e.getKey());
        }
      }
    }
    for(Map.Entry<String, MethodInterface> e : methods.entrySet()){
      res = res + "\t" + e.getKey() + " := " + e.getValue().toString() + "\n";
    }
    return res;
  }


/*
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
*/


} // end class Environment


