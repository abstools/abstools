package deadlock.analyser.generation;

import java.util.List;
import java.util.LinkedList;

import deadlock.analyser.factory.Factory;
import deadlock.analyser.factory.IRecord;
import deadlock.analyser.factory.ITypingEnvironmentVariableType;
import deadlock.analyser.factory.RecordDataType;
import deadlock.analyser.factory.RecordVariable;
import com.gzoumix.semisolver.constraint.Constraint;

public class ResultInferencePureExp {

  private ITypingEnvironmentVariableType r;
  private Factory factory;
  private Constraint constraint;
  //private boolean isDataType;

  /* Constructor */
  //public ResultInferencePureExp(Factory df) { this(df, null, df.newConstraint()); }
  public ResultInferencePureExp(Factory df, ITypingEnvironmentVariableType r) { this(df, r, df.newConstraint()); }
  public ResultInferencePureExp(Factory df, ITypingEnvironmentVariableType r, Constraint constraint) {
    factory = df; this.r = r; this.constraint = constraint; }

  /* Basic Get */
  public ITypingEnvironmentVariableType getVariableType() { return r; }
  public Constraint getConstraint() {return this.constraint; }

  public boolean isDataType() { return (r instanceof RecordDataType); }

	
}

