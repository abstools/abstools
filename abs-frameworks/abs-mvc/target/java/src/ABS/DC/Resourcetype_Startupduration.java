package ABS.DC;
// abslang.abs:623:20: 
public final class Resourcetype_Startupduration extends Resourcetype {
    public Resourcetype_Startupduration() {
    }
    protected abs.backend.java.lib.types.ABSValue[] getArgs() { return new abs.backend.java.lib.types.ABSValue[] {  }; }
    public java.lang.String getConstructorName() { return "Startupduration";} 
    public abs.backend.java.lib.types.ABSBool eq(abs.backend.java.lib.types.ABSValue o) {
        if (! (o instanceof Resourcetype_Startupduration)) return abs.backend.java.lib.types.ABSBool.FALSE;
        Resourcetype_Startupduration other = (Resourcetype_Startupduration) o;
        return abs.backend.java.lib.types.ABSBool.TRUE;
    }
    public boolean match(abs.backend.java.lib.expr.PatternConstructor c, abs.backend.java.lib.expr.PatternBinding b) {
        if (!c.constructorClass.equals(this.getClass())) return false;
        return true;
    }
}
