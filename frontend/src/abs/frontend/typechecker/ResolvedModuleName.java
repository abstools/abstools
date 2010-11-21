package abs.frontend.typechecker;

import abs.frontend.ast.ModuleDecl;
import abs.frontend.typechecker.KindedName.Kind;

public class ResolvedModuleName extends ResolvedName {
    private ModuleDecl decl;

    public ResolvedModuleName(ModuleDecl decl) {
        this.decl = decl;
    }

    @Override
    public KindedName getQualifiedName() {
        return new KindedName(getKind(), decl.getName());
    }

    @Override
    public Kind getKind() {
        return Kind.MODULE;
    }

    @Override
    public ResolvedModuleName getModuleName() {
        return this;
    }
}
