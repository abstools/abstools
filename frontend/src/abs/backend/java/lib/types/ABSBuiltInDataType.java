package abs.backend.java.lib.types;

import abs.backend.java.lib.expr.PatternBinding;
import abs.backend.java.lib.expr.PatternConstructor;

public abstract class ABSBuiltInDataType extends ABSDataType {
    public final String constructorName;

    protected ABSBuiltInDataType(String constructorName) {
        this.constructorName = constructorName;
    }

    public String getConstructorName() {
        return constructorName;
    }

    @Override
    public boolean match(PatternConstructor p, PatternBinding b) {
        if (p.constructorName.equals(constructorName)) {
            return true;
        } else {
            return false;
        }
    }

    @Override
    public boolean isBuiltIn() {
        return true;
    }

}
