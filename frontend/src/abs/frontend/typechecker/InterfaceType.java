package abs.frontend.typechecker;

import abs.frontend.ast.InterfaceDecl;
import abs.frontend.ast.InterfaceTypeUse;
import abs.frontend.ast.MethodSig;

public class InterfaceType extends ReferenceType {
    private final InterfaceDecl decl;

    public InterfaceType(InterfaceDecl decl) {
        this.decl = decl;
    }

    public InterfaceDecl getDecl() {
        return decl;
    }

    @Override
    public boolean isInterfaceType() {
        return true;
    }

    @Override
    public boolean equals(Object o) {
        if (!super.equals(o))
            return false;
        if (!(o instanceof InterfaceType))
            return false;
        InterfaceType t = (InterfaceType) o;
        return t.decl.equals(this.decl);
    }

    @Override
    public int hashCode() {
        return decl.hashCode();
    }

    @Override
    public boolean isAssignable(Type t, boolean considerSubtyping) {
        if (super.isAssignable(t))
            return true;

        if (considerSubtyping) {
            for (InterfaceTypeUse i : decl.getExtendedInterfaceUses()) {
                if (i.getType().isAssignable(t))
                    return true;
            }
        }
        return false;
    }

    @Override
    public boolean isAssignable(Type t) {
        return this.isAssignable(t, true);
    }

    @Override
    public String toString() {
        return decl.getName();
    }
    
    @Override
    public String getModuleName() {
        return decl.getModuleDecl().getName();
    }
    
    @Override
    public String getSimpleName() {
        return decl.getName();
    }

    @Override
    public MethodSig lookupMethod(String name) {
        return decl.lookupMethod(name);
    }

    @Override
    public Type copy() {
        return new InterfaceType(decl);
    }

}
