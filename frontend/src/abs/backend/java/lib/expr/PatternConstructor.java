package abs.backend.java.lib.expr;

import abs.backend.java.lib.types.ABSDataType;

public class PatternConstructor extends Pattern {
    public final Pattern[] subpattern;
    public final String constructorName;
    public PatternConstructor(String constructorName, Pattern... subpattern) {
        this.constructorName = constructorName;
        this.subpattern = subpattern;
    }
    
    @Override
    public boolean match(ABSDataType dt, PatternBinding b) {
        return dt.match(this, b);
    }

}
