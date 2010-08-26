package abs.backend.java.lib.expr;

import abs.backend.java.lib.types.ABSDataType;
import abs.backend.java.lib.types.ABSValue;

public class PatternConstructor extends Pattern {
    public final Pattern[] subpattern;
    public final String constructorName;
    public PatternConstructor(String constructorName, Pattern... subpattern) {
        this.constructorName = constructorName;
        this.subpattern = subpattern;
    }
    
    @Override
    public boolean match(ABSValue dt, PatternBinding b) {
        if (dt instanceof ABSDataType) {
            return ((ABSDataType)dt).match(this, b);
        } else {
            return false;
        }
    }

}
