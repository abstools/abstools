package sdaplugin.actions;

import java.io.PrintStream;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;

import abs.frontend.ast.Model;
import deadlock.analyser.Analyser;

public class SDARun extends Job {
	private Model m;
	private boolean verbose;
	private int nbIteration;
	private PrintStream out;


	public SDARun(Model m, boolean verbose, int nbIteration, PrintStream out) {
		super("SDA Deadlock Analysis");
		this.m = m;
		this.verbose = verbose;
		this.nbIteration = nbIteration;
		this.out = out;
	}

	@Override
	protected IStatus run(IProgressMonitor monitor) {
		(new Analyser()).deadlockAnalysis(this.m, this.verbose, this.nbIteration, this.out);
		return Status.OK_STATUS;
	}
}

