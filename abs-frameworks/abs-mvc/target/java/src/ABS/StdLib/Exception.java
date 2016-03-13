package ABS.StdLib;
// abslang.abs:70:0: 
public abstract class Exception extends abs.backend.java.lib.types.ABSDataType {
    public final boolean isDivisionByZeroException() { return this instanceof Exception_DivisionByZeroException; }
    public final Exception_DivisionByZeroException toDivisionByZeroException() { return (Exception_DivisionByZeroException) this; }
    public final boolean isAssertionFailException() { return this instanceof Exception_AssertionFailException; }
    public final Exception_AssertionFailException toAssertionFailException() { return (Exception_AssertionFailException) this; }
    public final boolean isPatternMatchFailException() { return this instanceof Exception_PatternMatchFailException; }
    public final Exception_PatternMatchFailException toPatternMatchFailException() { return (Exception_PatternMatchFailException) this; }
    public final boolean isNullPointerException() { return this instanceof Exception_NullPointerException; }
    public final Exception_NullPointerException toNullPointerException() { return (Exception_NullPointerException) this; }
    public final boolean isStackOverflowEcxeption() { return this instanceof Exception_StackOverflowEcxeption; }
    public final Exception_StackOverflowEcxeption toStackOverflowEcxeption() { return (Exception_StackOverflowEcxeption) this; }
    public final boolean isHeapOverflowException() { return this instanceof Exception_HeapOverflowException; }
    public final Exception_HeapOverflowException toHeapOverflowException() { return (Exception_HeapOverflowException) this; }
    public final boolean isKeyboardInterruptException() { return this instanceof Exception_KeyboardInterruptException; }
    public final Exception_KeyboardInterruptException toKeyboardInterruptException() { return (Exception_KeyboardInterruptException) this; }
}
