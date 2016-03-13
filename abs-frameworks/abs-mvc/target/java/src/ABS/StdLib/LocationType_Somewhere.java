package ABS.StdLib;
// abslang.abs:523:33: 
public final class LocationType_Somewhere extends LocationType {
    public LocationType_Somewhere() {
    }
    protected abs.backend.java.lib.types.ABSValue[] getArgs() { return new abs.backend.java.lib.types.ABSValue[] {  }; }
    public java.lang.String getConstructorName() { return "Somewhere";} 
    public abs.backend.java.lib.types.ABSBool eq(abs.backend.java.lib.types.ABSValue o) {
        if (! (o instanceof LocationType_Somewhere)) return abs.backend.java.lib.types.ABSBool.FALSE;
        LocationType_Somewhere other = (LocationType_Somewhere) o;
        return abs.backend.java.lib.types.ABSBool.TRUE;
    }
    public boolean match(abs.backend.java.lib.expr.PatternConstructor c, abs.backend.java.lib.expr.PatternBinding b) {
        if (!c.constructorClass.equals(this.getClass())) return false;
        return true;
    }
}
