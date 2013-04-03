package deadlock.analyser.generation;

import java.util.List;
import java.util.LinkedList;

import deadlock.analyser.factory.Factory;
import deadlock.analyser.factory.Record;
import deadlock.analyser.factory.RecordVariable;

public class ResultInferencePureExp {

  private String vname;
  private LinkedList<Record> obj;
  private Factory factory;
  //private boolean isDataType;

  /* Constructor */
  public ResultInferencePureExp(Factory df) {
    factory = df; vname = null; obj = new LinkedList<Record>(); obj.add(df.newRecordVariableFanthom()); }
  public ResultInferencePureExp(Factory df, String id, Record r) {
    factory = df; vname = id; obj = new LinkedList<Record>(); obj.add(r); }

  /* Basic Get */
  public String getId() { return vname; }
  public List<Record> getObjs() { return obj; }
  public Record getRepresentative() { return obj.getLast(); }
  public Record getRepresentative(Record r) {
    if(this.isDataType()) { return r; }
    else {
      for(Record maybe : obj) { if(maybe.equals(r)) { return r; } }
      return obj.getLast();
  } }

  public boolean isDataType() { return (obj.getFirst() instanceof RecordVariable) && (((RecordVariable)obj.getFirst()).isDataType()); }

  /* Basic Extension */
  public void add(ResultInferencePureExp r) {
    if(!r.isDataType()) {
      if(this.isDataType()) { this.vname = r.vname; this.obj = r.obj; }
      else { obj.addAll(r.obj); vname = null; }
  } }
	
}

