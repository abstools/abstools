package ABS.StdLib;
// abslang.abs:473:0: 
public abstract class Duration extends abs.backend.java.lib.types.ABSDataType {
    public final boolean isDuration() { return this instanceof Duration_Duration; }
    public final Duration_Duration toDuration() { return (Duration_Duration) this; }
    public final boolean isInfDuration() { return this instanceof Duration_InfDuration; }
    public final Duration_InfDuration toInfDuration() { return (Duration_InfDuration) this; }
}
