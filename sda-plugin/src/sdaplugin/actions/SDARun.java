package sdaplugin.actions;

import java.io.PrintStream;

import abs.frontend.ast.Model;
import deadlock.analyser.*;


public class SDARun implements Runnable {
	private Model m;
	private boolean verbose;
	private int nbIteration;
	private PrintStream out;
	
	
  public SDARun(Model m, boolean verbose, int nbIteration, PrintStream out) {
    this.m = m;
    this.verbose = verbose;
    this.nbIteration = nbIteration;
    this.out = out;
  }
	
  @Override
  public void run() {
    (new Analyser()).deadlockAnalysis(this.m, this.verbose, this.nbIteration, this.out);
  }
}

