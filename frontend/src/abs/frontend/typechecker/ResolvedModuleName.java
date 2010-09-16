package abs.frontend.typechecker;

import abs.frontend.ast.ModuleDecl;

public class ResolvedModuleName extends ResolvedName {
	private ModuleDecl decl;

	public ResolvedModuleName(ModuleDecl decl) {
		this.decl = decl;
	}

	@Override
	public String getQualifiedString() {
		return decl.getName().getString();
	}

	@Override
	public ResolvedModuleName getModuleName() {
		return this;
	}
}
