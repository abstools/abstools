package abs.frontend.typechecker;

import abs.frontend.ast.Decl;

/**
 * Can be the name for a function, a data type, a type synonym, a class, an interface, a data constructor
 *  
 * @author jan
 *
 */
public class ResolvedEntityName extends ResolvedName {

	private Decl decl;
	private ResolvedModuleName moduleName;

	public ResolvedEntityName(ResolvedModuleName moduleName, Decl decl) {
		this.decl = decl;
		this.moduleName = moduleName;
	}
	
	@Override
	public String getQualifiedString() {
		return moduleName.getQualifiedString()+"."+decl.getName();
	}

	@Override
	public ResolvedModuleName getModuleName() {
		return moduleName;
	}

	public Decl getDecl() {
		return decl;
	}
}
