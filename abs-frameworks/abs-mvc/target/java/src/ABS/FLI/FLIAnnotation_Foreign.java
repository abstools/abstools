package ABS.FLI;
// abslang.abs:603:21: 
public final class FLIAnnotation_Foreign extends FLIAnnotation {
    public FLIAnnotation_Foreign() {
    }
    protected abs.backend.java.lib.types.ABSValue[] getArgs() { return new abs.backend.java.lib.types.ABSValue[] {  }; }
    public java.lang.String getConstructorName() { return "Foreign";} 
    public abs.backend.java.lib.types.ABSBool eq(abs.backend.java.lib.types.ABSValue o) {
        if (! (o instanceof FLIAnnotation_Foreign)) return abs.backend.java.lib.types.ABSBool.FALSE;
        FLIAnnotation_Foreign other = (FLIAnnotation_Foreign) o;
        return abs.backend.java.lib.types.ABSBool.TRUE;
    }
    public boolean match(abs.backend.java.lib.expr.PatternConstructor c, abs.backend.java.lib.expr.PatternBinding b) {
        if (!c.constructorClass.equals(this.getClass())) return false;
        return true;
    }
}
