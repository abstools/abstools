package abs.frontend.typechecker;

public final class UnknownType extends Type {
    public static final UnknownType INSTANCE = new UnknownType();
    
    private UnknownType() { }
    
    @Override
    public boolean isUnknownType() {
        return true;
    }
}
