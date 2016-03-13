package ABS.StdLib;
// abslang.abs:122:0: 
public abstract class Either<A extends abs.backend.java.lib.types.ABSValue,B extends abs.backend.java.lib.types.ABSValue> extends abs.backend.java.lib.types.ABSDataType {
    public final boolean isLeft() { return this instanceof Either_Left; }
    public final Either_Left<A,B> toLeft() { return (Either_Left) this; }
    public final boolean isRight() { return this instanceof Either_Right; }
    public final Either_Right<A,B> toRight() { return (Either_Right) this; }
}
