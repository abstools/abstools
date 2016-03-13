package ABS.StdLib;
// abslang.abs:117:0: 
public abstract class Maybe<A extends abs.backend.java.lib.types.ABSValue> extends abs.backend.java.lib.types.ABSDataType {
    public final boolean isNothing() { return this instanceof Maybe_Nothing; }
    public final Maybe_Nothing<A> toNothing() { return (Maybe_Nothing) this; }
    public final boolean isJust() { return this instanceof Maybe_Just; }
    public final Maybe_Just<A> toJust() { return (Maybe_Just) this; }
}
