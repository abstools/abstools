package ABS.StdLib;
// abslang.abs:537:23: 
public final class FinalAnnotation_Final extends FinalAnnotation {
    public FinalAnnotation_Final() {
    }
    protected abs.backend.java.lib.types.ABSValue[] getArgs() { return new abs.backend.java.lib.types.ABSValue[] {  }; }
    public java.lang.String getConstructorName() { return "Final";} 
    public abs.backend.java.lib.types.ABSBool eq(abs.backend.java.lib.types.ABSValue o) {
        if (! (o instanceof FinalAnnotation_Final)) return abs.backend.java.lib.types.ABSBool.FALSE;
        FinalAnnotation_Final other = (FinalAnnotation_Final) o;
        return abs.backend.java.lib.types.ABSBool.TRUE;
    }
    public boolean match(abs.backend.java.lib.expr.PatternConstructor c, abs.backend.java.lib.expr.PatternBinding b) {
        if (!c.constructorClass.equals(this.getClass())) return false;
        return true;
    }
}
