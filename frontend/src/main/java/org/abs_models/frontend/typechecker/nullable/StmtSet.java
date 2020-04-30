package org.abs_models.frontend.typechecker.nullable;

import org.abs_models.frontend.ast.Stmt;
import java.util.Set;
import java.util.HashSet;

public class StmtSet extends HashSet<Stmt> {

	/**
	 *
	 */
	private static final long serialVersionUID = 1L;

	public StmtSet() {
		super();
	}

	public StmtSet(Stmt s) {
		super();
		add(s);
	}

	public StmtSet union(StmtSet o) {
		StmtSet s = empty();
		s.addAll(this);
		s.addAll(o);
		return s;
	}

	public StmtSet union(Stmt stmt) {
		StmtSet s = empty();
		s.addAll(this);
		s.add(stmt);
		return s;
	}

	public static StmtSet empty() {
		return new StmtSet();
	}
}
