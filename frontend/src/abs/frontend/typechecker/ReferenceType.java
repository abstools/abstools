package abs.frontend.typechecker;

public abstract class ReferenceType extends Type {

    @Override
    public boolean isReferenceType() {
        return true;
    }

}
