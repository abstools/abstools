import abs.backend.java.lib.types.*; import abs.backend.java.lib.expr.*; import abs.backend.java.lib.expr.*;
public class Main extends abs.backend.java.lib.runtime.ABSObject {
   public static void main(java.lang.String[] args) {
       abs.backend.java.lib.runtime.COG cog = new abs.backend.java.lib.runtime.COG(Main.class);
       abs.backend.java.lib.runtime.ABSRuntime.asyncCall(new MainTask(new Main(cog)));
   }
   public Main(abs.backend.java.lib.runtime.COG cog) { super(cog); }
   static class MainTask extends abs.backend.java.lib.runtime.Task<Main> {
      public MainTask(Main target) { super(target); }
      public Object execute() { target.start(); return null;}
      public java.lang.String methodName() { return "start"; }
   }
  public void start() 
 {
      abs.backend.java.lib.types.ABSBool testresult = abs.backend.java.lib.types.ABSBool.fromBoolean(true);      I i = null;      i = new C();
      abs.backend.java.lib.runtime.ABSRuntime.asyncCall(new abs.backend.java.lib.runtime.Task<I>(i) {
    public abs.backend.java.lib.runtime.Task<?> init(
) {   return this; } public java.lang.String methodName() { return "m"; }
    public Object execute() {
        return target.m();    }
}.init());
   System.out.println(testresult.toBoolean());
   }
}
abstract class Unit extends ABSDataType {
}
class ABSConstructor__Unit extends Unit {
   public ABSConstructor__Unit() {
   }
   public ABSBool eq(ABSDataType o) {
      if (! (o instanceof ABSConstructor__Unit)) return ABSBool.FALSE;
      ABSConstructor__Unit other = (ABSConstructor__Unit) o;
      return ABSBool.TRUE;
   }
   public boolean match(abs.backend.java.lib.expr.PatternConstructor c, abs.backend.java.lib.expr.PatternBinding b) {
       if (!c.constructorName.equals("Unit")) return false;
      return true;
   }
}
abstract class String extends ABSDataType {
}
abstract class Int extends ABSDataType {
}
abstract class Bool extends ABSDataType {
}
class ABSConstructor__True extends Bool {
   public ABSConstructor__True() {
   }
   public ABSBool eq(ABSDataType o) {
      if (! (o instanceof ABSConstructor__True)) return ABSBool.FALSE;
      ABSConstructor__True other = (ABSConstructor__True) o;
      return ABSBool.TRUE;
   }
   public boolean match(abs.backend.java.lib.expr.PatternConstructor c, abs.backend.java.lib.expr.PatternBinding b) {
       if (!c.constructorName.equals("True")) return false;
      return true;
   }
}
class ABSConstructor__False extends Bool {
   public ABSConstructor__False() {
   }
   public ABSBool eq(ABSDataType o) {
      if (! (o instanceof ABSConstructor__False)) return ABSBool.FALSE;
      ABSConstructor__False other = (ABSConstructor__False) o;
      return ABSBool.TRUE;
   }
   public boolean match(abs.backend.java.lib.expr.PatternConstructor c, abs.backend.java.lib.expr.PatternBinding b) {
       if (!c.constructorName.equals("False")) return false;
      return true;
   }
}
abstract class Fut<A extends abs.backend.java.lib.types.ABSDataType> extends ABSDataType {
}
final class ABSFunction__and implements ABSFunction {
   private ABSFunction__and() { }
   public static abs.backend.java.lib.types.ABSBool apply(final abs.backend.java.lib.types.ABSBool a, final abs.backend.java.lib.types.ABSBool b) {
      return new Case<abs.backend.java.lib.types.ABSBool,abs.backend.java.lib.types.ABSBool>() {
      public abs.backend.java.lib.types.ABSBool of(final abs.backend.java.lib.types.ABSBool __ABS_value) { 
final abs.backend.java.lib.expr.PatternBinding __ABS_binding0 = new abs.backend.java.lib.expr.PatternConstructor("True").match(__ABS_value);
if (__ABS_binding0 != null) return new Object() {
  public abs.backend.java.lib.types.ABSBool execute() { return b; }}.execute();
final abs.backend.java.lib.expr.PatternBinding __ABS_binding1 = new abs.backend.java.lib.expr.AnyPattern().match(__ABS_value);
if (__ABS_binding1 != null) return new Object() {
  public abs.backend.java.lib.types.ABSBool execute() { return abs.backend.java.lib.types.ABSBool.fromBoolean(false); }}.execute();
throw new abs.backend.java.lib.expr.UnmatchedCaseException();
}}.of(a);
   }
}
final class ABSFunction__not implements ABSFunction {
   private ABSFunction__not() { }
   public static abs.backend.java.lib.types.ABSBool apply(final abs.backend.java.lib.types.ABSBool a) {
      return new Case<abs.backend.java.lib.types.ABSBool,abs.backend.java.lib.types.ABSBool>() {
      public abs.backend.java.lib.types.ABSBool of(final abs.backend.java.lib.types.ABSBool __ABS_value) { 
final abs.backend.java.lib.expr.PatternBinding __ABS_binding0 = new abs.backend.java.lib.expr.PatternConstructor("True").match(__ABS_value);
if (__ABS_binding0 != null) return new Object() {
  public abs.backend.java.lib.types.ABSBool execute() { return abs.backend.java.lib.types.ABSBool.fromBoolean(false); }}.execute();
final abs.backend.java.lib.expr.PatternBinding __ABS_binding1 = new abs.backend.java.lib.expr.PatternConstructor("False").match(__ABS_value);
if (__ABS_binding1 != null) return new Object() {
  public abs.backend.java.lib.types.ABSBool execute() { return abs.backend.java.lib.types.ABSBool.fromBoolean(true); }}.execute();
throw new abs.backend.java.lib.expr.UnmatchedCaseException();
}}.of(a);
   }
}
abstract class Maybe<A extends abs.backend.java.lib.types.ABSDataType> extends ABSDataType {
}
class ABSConstructor__Nothing<A extends abs.backend.java.lib.types.ABSDataType> extends Maybe<A> {
   public ABSConstructor__Nothing() {
   }
   public ABSBool eq(ABSDataType o) {
      if (! (o instanceof ABSConstructor__Nothing)) return ABSBool.FALSE;
      ABSConstructor__Nothing other = (ABSConstructor__Nothing) o;
      return ABSBool.TRUE;
   }
   public boolean match(abs.backend.java.lib.expr.PatternConstructor c, abs.backend.java.lib.expr.PatternBinding b) {
       if (!c.constructorName.equals("Nothing")) return false;
      return true;
   }
}
class ABSConstructor__Just<A extends abs.backend.java.lib.types.ABSDataType> extends Maybe<A> {
   public final A arg0;
   public ABSConstructor__Just(   final A arg0) {
this.arg0 = arg0;
   }
   public ABSBool eq(ABSDataType o) {
      if (! (o instanceof ABSConstructor__Just)) return ABSBool.FALSE;
      ABSConstructor__Just other = (ABSConstructor__Just) o;
if (!this.arg0.eq(other.arg0).toBoolean()) return ABSBool.FALSE;
      return ABSBool.TRUE;
   }
   public boolean match(abs.backend.java.lib.expr.PatternConstructor c, abs.backend.java.lib.expr.PatternBinding b) {
       if (!c.constructorName.equals("Just")) return false;
       if (!c.subpattern[0].match(this.arg0,b)) return false;
      return true;
   }
}
final class ABSFunction__fromJust implements ABSFunction {
   private ABSFunction__fromJust() { }
   public static <A extends abs.backend.java.lib.types.ABSDataType>A apply(final Maybe a) {
      return new Case<Maybe<A>,A>() {
      public A of(final Maybe<A> __ABS_value) { 
final abs.backend.java.lib.expr.PatternBinding __ABS_binding0 = new abs.backend.java.lib.expr.PatternConstructor("Just",new abs.backend.java.lib.expr.PatternVariable("j")).match(__ABS_value);
if (__ABS_binding0 != null) return new Object() {
  public A execute(final A j) { return j; }}.execute((A) __ABS_binding0.getBinding(0));
throw new abs.backend.java.lib.expr.UnmatchedCaseException();
}}.of(a);
   }
}
final class ABSFunction__isJust implements ABSFunction {
   private ABSFunction__isJust() { }
   public static <A extends abs.backend.java.lib.types.ABSDataType>abs.backend.java.lib.types.ABSBool apply(final Maybe a) {
      return new Case<Maybe<A>,abs.backend.java.lib.types.ABSBool>() {
      public abs.backend.java.lib.types.ABSBool of(final Maybe<A> __ABS_value) { 
final abs.backend.java.lib.expr.PatternBinding __ABS_binding0 = new abs.backend.java.lib.expr.PatternConstructor("Just",new abs.backend.java.lib.expr.PatternVariable("j")).match(__ABS_value);
if (__ABS_binding0 != null) return new Object() {
  public abs.backend.java.lib.types.ABSBool execute(final A j) { return abs.backend.java.lib.types.ABSBool.fromBoolean(true); }}.execute((A) __ABS_binding0.getBinding(0));
final abs.backend.java.lib.expr.PatternBinding __ABS_binding1 = new abs.backend.java.lib.expr.PatternConstructor("Nothing").match(__ABS_value);
if (__ABS_binding1 != null) return new Object() {
  public abs.backend.java.lib.types.ABSBool execute() { return abs.backend.java.lib.types.ABSBool.fromBoolean(false); }}.execute();
throw new abs.backend.java.lib.expr.UnmatchedCaseException();
}}.of(a);
   }
}
abstract class Either<A extends abs.backend.java.lib.types.ABSDataType,B extends abs.backend.java.lib.types.ABSDataType> extends ABSDataType {
}
class ABSConstructor__Left<A extends abs.backend.java.lib.types.ABSDataType,B extends abs.backend.java.lib.types.ABSDataType> extends Either<A,B> {
   public final A arg0;
   public ABSConstructor__Left(   final A arg0) {
this.arg0 = arg0;
   }
   public ABSBool eq(ABSDataType o) {
      if (! (o instanceof ABSConstructor__Left)) return ABSBool.FALSE;
      ABSConstructor__Left other = (ABSConstructor__Left) o;
if (!this.arg0.eq(other.arg0).toBoolean()) return ABSBool.FALSE;
      return ABSBool.TRUE;
   }
   public boolean match(abs.backend.java.lib.expr.PatternConstructor c, abs.backend.java.lib.expr.PatternBinding b) {
       if (!c.constructorName.equals("Left")) return false;
       if (!c.subpattern[0].match(this.arg0,b)) return false;
      return true;
   }
}
class ABSConstructor__Right<A extends abs.backend.java.lib.types.ABSDataType,B extends abs.backend.java.lib.types.ABSDataType> extends Either<A,B> {
   public final B arg0;
   public ABSConstructor__Right(   final B arg0) {
this.arg0 = arg0;
   }
   public ABSBool eq(ABSDataType o) {
      if (! (o instanceof ABSConstructor__Right)) return ABSBool.FALSE;
      ABSConstructor__Right other = (ABSConstructor__Right) o;
if (!this.arg0.eq(other.arg0).toBoolean()) return ABSBool.FALSE;
      return ABSBool.TRUE;
   }
   public boolean match(abs.backend.java.lib.expr.PatternConstructor c, abs.backend.java.lib.expr.PatternBinding b) {
       if (!c.constructorName.equals("Right")) return false;
       if (!c.subpattern[0].match(this.arg0,b)) return false;
      return true;
   }
}
final class ABSFunction__left implements ABSFunction {
   private ABSFunction__left() { }
   public static <A extends abs.backend.java.lib.types.ABSDataType,B extends abs.backend.java.lib.types.ABSDataType>A apply(final Either val) {
      return new Case<Either<A,B>,A>() {
      public A of(final Either<A,B> __ABS_value) { 
final abs.backend.java.lib.expr.PatternBinding __ABS_binding0 = new abs.backend.java.lib.expr.PatternConstructor("Left",new abs.backend.java.lib.expr.PatternVariable("x")).match(__ABS_value);
if (__ABS_binding0 != null) return new Object() {
  public A execute(final A x) { return x; }}.execute((A) __ABS_binding0.getBinding(0));
throw new abs.backend.java.lib.expr.UnmatchedCaseException();
}}.of(val);
   }
}
final class ABSFunction__right implements ABSFunction {
   private ABSFunction__right() { }
   public static <A extends abs.backend.java.lib.types.ABSDataType,B extends abs.backend.java.lib.types.ABSDataType>B apply(final Either val) {
      return new Case<Either<A,B>,B>() {
      public B of(final Either<A,B> __ABS_value) { 
final abs.backend.java.lib.expr.PatternBinding __ABS_binding0 = new abs.backend.java.lib.expr.PatternConstructor("Right",new abs.backend.java.lib.expr.PatternVariable("x")).match(__ABS_value);
if (__ABS_binding0 != null) return new Object() {
  public B execute(final B x) { return x; }}.execute((B) __ABS_binding0.getBinding(0));
throw new abs.backend.java.lib.expr.UnmatchedCaseException();
}}.of(val);
   }
}
final class ABSFunction__isLeft implements ABSFunction {
   private ABSFunction__isLeft() { }
   public static <A extends abs.backend.java.lib.types.ABSDataType,B extends abs.backend.java.lib.types.ABSDataType>abs.backend.java.lib.types.ABSBool apply(final Either val) {
      return new Case<Either<A,B>,abs.backend.java.lib.types.ABSBool>() {
      public abs.backend.java.lib.types.ABSBool of(final Either<A,B> __ABS_value) { 
final abs.backend.java.lib.expr.PatternBinding __ABS_binding0 = new abs.backend.java.lib.expr.PatternConstructor("Left",new abs.backend.java.lib.expr.PatternVariable("x")).match(__ABS_value);
if (__ABS_binding0 != null) return new Object() {
  public abs.backend.java.lib.types.ABSBool execute(final A x) { return abs.backend.java.lib.types.ABSBool.fromBoolean(true); }}.execute((A) __ABS_binding0.getBinding(0));
final abs.backend.java.lib.expr.PatternBinding __ABS_binding1 = new abs.backend.java.lib.expr.AnyPattern().match(__ABS_value);
if (__ABS_binding1 != null) return new Object() {
  public abs.backend.java.lib.types.ABSBool execute() { return abs.backend.java.lib.types.ABSBool.fromBoolean(false); }}.execute();
throw new abs.backend.java.lib.expr.UnmatchedCaseException();
}}.of(val);
   }
}
final class ABSFunction__isRight implements ABSFunction {
   private ABSFunction__isRight() { }
   public static <A extends abs.backend.java.lib.types.ABSDataType,B extends abs.backend.java.lib.types.ABSDataType>abs.backend.java.lib.types.ABSBool apply(final Either val) {
      return ABSFunction__isLeft.apply(val).negate();
   }
}
abstract class Pair<A extends abs.backend.java.lib.types.ABSDataType,B extends abs.backend.java.lib.types.ABSDataType> extends ABSDataType {
}
class ABSConstructor__Pair<A extends abs.backend.java.lib.types.ABSDataType,B extends abs.backend.java.lib.types.ABSDataType> extends Pair<A,B> {
   public final A arg0;
   public final B arg1;
   public ABSConstructor__Pair(   final A arg0,   final B arg1) {
this.arg0 = arg0;
this.arg1 = arg1;
   }
   public ABSBool eq(ABSDataType o) {
      if (! (o instanceof ABSConstructor__Pair)) return ABSBool.FALSE;
      ABSConstructor__Pair other = (ABSConstructor__Pair) o;
if (!this.arg0.eq(other.arg0).toBoolean()) return ABSBool.FALSE;
if (!this.arg1.eq(other.arg1).toBoolean()) return ABSBool.FALSE;
      return ABSBool.TRUE;
   }
   public boolean match(abs.backend.java.lib.expr.PatternConstructor c, abs.backend.java.lib.expr.PatternBinding b) {
       if (!c.constructorName.equals("Pair")) return false;
       if (!c.subpattern[0].match(this.arg0,b)) return false;
       if (!c.subpattern[1].match(this.arg1,b)) return false;
      return true;
   }
}
final class ABSFunction__fst implements ABSFunction {
   private ABSFunction__fst() { }
   public static <A extends abs.backend.java.lib.types.ABSDataType,B extends abs.backend.java.lib.types.ABSDataType>A apply(final Pair p) {
      return new Case<Pair<A,B>,A>() {
      public A of(final Pair<A,B> __ABS_value) { 
final abs.backend.java.lib.expr.PatternBinding __ABS_binding0 = new abs.backend.java.lib.expr.PatternConstructor("Pair",new abs.backend.java.lib.expr.PatternVariable("s"),new abs.backend.java.lib.expr.PatternVariable("f")).match(__ABS_value);
if (__ABS_binding0 != null) return new Object() {
  public A execute(final A s,final B f) { return s; }}.execute((A) __ABS_binding0.getBinding(0),(B) __ABS_binding0.getBinding(1));
throw new abs.backend.java.lib.expr.UnmatchedCaseException();
}}.of(p);
   }
}
final class ABSFunction__snd implements ABSFunction {
   private ABSFunction__snd() { }
   public static <A extends abs.backend.java.lib.types.ABSDataType,B extends abs.backend.java.lib.types.ABSDataType>B apply(final Pair p) {
      return new Case<Pair<A,B>,B>() {
      public B of(final Pair<A,B> __ABS_value) { 
final abs.backend.java.lib.expr.PatternBinding __ABS_binding0 = new abs.backend.java.lib.expr.PatternConstructor("Pair",new abs.backend.java.lib.expr.PatternVariable("s"),new abs.backend.java.lib.expr.PatternVariable("f")).match(__ABS_value);
if (__ABS_binding0 != null) return new Object() {
  public B execute(final A s,final B f) { return f; }}.execute((A) __ABS_binding0.getBinding(0),(B) __ABS_binding0.getBinding(1));
throw new abs.backend.java.lib.expr.UnmatchedCaseException();
}}.of(p);
   }
}
abstract class Triple<A extends abs.backend.java.lib.types.ABSDataType,B extends abs.backend.java.lib.types.ABSDataType,C extends abs.backend.java.lib.types.ABSDataType> extends ABSDataType {
}
class ABSConstructor__Triple<A extends abs.backend.java.lib.types.ABSDataType,B extends abs.backend.java.lib.types.ABSDataType,C extends abs.backend.java.lib.types.ABSDataType> extends Triple<A,B,C> {
   public final A arg0;
   public final B arg1;
   public final C arg2;
   public ABSConstructor__Triple(   final A arg0,   final B arg1,   final C arg2) {
this.arg0 = arg0;
this.arg1 = arg1;
this.arg2 = arg2;
   }
   public ABSBool eq(ABSDataType o) {
      if (! (o instanceof ABSConstructor__Triple)) return ABSBool.FALSE;
      ABSConstructor__Triple other = (ABSConstructor__Triple) o;
if (!this.arg0.eq(other.arg0).toBoolean()) return ABSBool.FALSE;
if (!this.arg1.eq(other.arg1).toBoolean()) return ABSBool.FALSE;
if (!this.arg2.eq(other.arg2).toBoolean()) return ABSBool.FALSE;
      return ABSBool.TRUE;
   }
   public boolean match(abs.backend.java.lib.expr.PatternConstructor c, abs.backend.java.lib.expr.PatternBinding b) {
       if (!c.constructorName.equals("Triple")) return false;
       if (!c.subpattern[0].match(this.arg0,b)) return false;
       if (!c.subpattern[1].match(this.arg1,b)) return false;
       if (!c.subpattern[2].match(this.arg2,b)) return false;
      return true;
   }
}
final class ABSFunction__fstT implements ABSFunction {
   private ABSFunction__fstT() { }
   public static <A extends abs.backend.java.lib.types.ABSDataType,B extends abs.backend.java.lib.types.ABSDataType,C extends abs.backend.java.lib.types.ABSDataType>A apply(final Triple p) {
      return new Case<Triple<A,B,C>,A>() {
      public A of(final Triple<A,B,C> __ABS_value) { 
final abs.backend.java.lib.expr.PatternBinding __ABS_binding0 = new abs.backend.java.lib.expr.PatternConstructor("Triple",new abs.backend.java.lib.expr.PatternVariable("s"),new abs.backend.java.lib.expr.PatternVariable("f"),new abs.backend.java.lib.expr.PatternVariable("g")).match(__ABS_value);
if (__ABS_binding0 != null) return new Object() {
  public A execute(final A s,final B f,final C g) { return s; }}.execute((A) __ABS_binding0.getBinding(0),(B) __ABS_binding0.getBinding(1),(C) __ABS_binding0.getBinding(2));
throw new abs.backend.java.lib.expr.UnmatchedCaseException();
}}.of(p);
   }
}
final class ABSFunction__sndT implements ABSFunction {
   private ABSFunction__sndT() { }
   public static <A extends abs.backend.java.lib.types.ABSDataType,B extends abs.backend.java.lib.types.ABSDataType,C extends abs.backend.java.lib.types.ABSDataType>B apply(final Triple p) {
      return new Case<Triple<A,B,C>,B>() {
      public B of(final Triple<A,B,C> __ABS_value) { 
final abs.backend.java.lib.expr.PatternBinding __ABS_binding0 = new abs.backend.java.lib.expr.PatternConstructor("Triple",new abs.backend.java.lib.expr.PatternVariable("s"),new abs.backend.java.lib.expr.PatternVariable("f"),new abs.backend.java.lib.expr.PatternVariable("g")).match(__ABS_value);
if (__ABS_binding0 != null) return new Object() {
  public B execute(final A s,final B f,final C g) { return f; }}.execute((A) __ABS_binding0.getBinding(0),(B) __ABS_binding0.getBinding(1),(C) __ABS_binding0.getBinding(2));
throw new abs.backend.java.lib.expr.UnmatchedCaseException();
}}.of(p);
   }
}
final class ABSFunction__trd implements ABSFunction {
   private ABSFunction__trd() { }
   public static <A extends abs.backend.java.lib.types.ABSDataType,B extends abs.backend.java.lib.types.ABSDataType,C extends abs.backend.java.lib.types.ABSDataType>C apply(final Triple p) {
      return new Case<Triple<A,B,C>,C>() {
      public C of(final Triple<A,B,C> __ABS_value) { 
final abs.backend.java.lib.expr.PatternBinding __ABS_binding0 = new abs.backend.java.lib.expr.PatternConstructor("Triple",new abs.backend.java.lib.expr.PatternVariable("s"),new abs.backend.java.lib.expr.PatternVariable("f"),new abs.backend.java.lib.expr.PatternVariable("g")).match(__ABS_value);
if (__ABS_binding0 != null) return new Object() {
  public C execute(final A s,final B f,final C g) { return g; }}.execute((A) __ABS_binding0.getBinding(0),(B) __ABS_binding0.getBinding(1),(C) __ABS_binding0.getBinding(2));
throw new abs.backend.java.lib.expr.UnmatchedCaseException();
}}.of(p);
   }
}
abstract class Set<A extends abs.backend.java.lib.types.ABSDataType> extends ABSDataType {
}
class ABSConstructor__EmptySet<A extends abs.backend.java.lib.types.ABSDataType> extends Set<A> {
   public ABSConstructor__EmptySet() {
   }
   public ABSBool eq(ABSDataType o) {
      if (! (o instanceof ABSConstructor__EmptySet)) return ABSBool.FALSE;
      ABSConstructor__EmptySet other = (ABSConstructor__EmptySet) o;
      return ABSBool.TRUE;
   }
   public boolean match(abs.backend.java.lib.expr.PatternConstructor c, abs.backend.java.lib.expr.PatternBinding b) {
       if (!c.constructorName.equals("EmptySet")) return false;
      return true;
   }
}
class ABSConstructor__Insert<A extends abs.backend.java.lib.types.ABSDataType> extends Set<A> {
   public final A arg0;
   public final Set arg1;
   public ABSConstructor__Insert(   final A arg0,   final Set arg1) {
this.arg0 = arg0;
this.arg1 = arg1;
   }
   public ABSBool eq(ABSDataType o) {
      if (! (o instanceof ABSConstructor__Insert)) return ABSBool.FALSE;
      ABSConstructor__Insert other = (ABSConstructor__Insert) o;
if (!this.arg0.eq(other.arg0).toBoolean()) return ABSBool.FALSE;
if (!this.arg1.eq(other.arg1).toBoolean()) return ABSBool.FALSE;
      return ABSBool.TRUE;
   }
   public boolean match(abs.backend.java.lib.expr.PatternConstructor c, abs.backend.java.lib.expr.PatternBinding b) {
       if (!c.constructorName.equals("Insert")) return false;
       if (!c.subpattern[0].match(this.arg0,b)) return false;
       if (!c.subpattern[1].match(this.arg1,b)) return false;
      return true;
   }
}
final class ABSFunction__set implements ABSFunction {
   private ABSFunction__set() { }
   public static <A extends abs.backend.java.lib.types.ABSDataType>Set apply(final List l) {
      return new Case<List<A>,Set<A>>() {
      public Set<A> of(final List<A> __ABS_value) { 
final abs.backend.java.lib.expr.PatternBinding __ABS_binding0 = new abs.backend.java.lib.expr.PatternConstructor("Nil").match(__ABS_value);
if (__ABS_binding0 != null) return new Object() {
  public Set<A> execute() { return new ABSConstructor__EmptySet(); }}.execute();
final abs.backend.java.lib.expr.PatternBinding __ABS_binding1 = new abs.backend.java.lib.expr.PatternConstructor("Cons",new abs.backend.java.lib.expr.PatternVariable("x"),new abs.backend.java.lib.expr.PatternVariable("xs")).match(__ABS_value);
if (__ABS_binding1 != null) return new Object() {
  public Set<A> execute(final A x,final List<A> xs) { return new ABSConstructor__Insert(x, ABSFunction__set.apply(xs)); }}.execute((A) __ABS_binding1.getBinding(0),(List<A>) __ABS_binding1.getBinding(1));
throw new abs.backend.java.lib.expr.UnmatchedCaseException();
}}.of(l);
   }
}
final class ABSFunction__contains implements ABSFunction {
   private ABSFunction__contains() { }
   public static <A extends abs.backend.java.lib.types.ABSDataType>abs.backend.java.lib.types.ABSBool apply(final Set ss, final A e) {
      return new Case<Set<A>,abs.backend.java.lib.types.ABSBool>() {
      public abs.backend.java.lib.types.ABSBool of(final Set<A> __ABS_value) { 
final abs.backend.java.lib.expr.PatternBinding __ABS_binding0 = new abs.backend.java.lib.expr.PatternConstructor("EmptySet").match(__ABS_value);
if (__ABS_binding0 != null) return new Object() {
  public abs.backend.java.lib.types.ABSBool execute() { return abs.backend.java.lib.types.ABSBool.fromBoolean(false); }}.execute();
final abs.backend.java.lib.expr.PatternBinding __ABS_binding1 = new abs.backend.java.lib.expr.PatternConstructor("Insert",new abs.backend.java.lib.expr.PatternValue(e),new abs.backend.java.lib.expr.AnyPattern()).match(__ABS_value);
if (__ABS_binding1 != null) return new Object() {
  public abs.backend.java.lib.types.ABSBool execute() { return abs.backend.java.lib.types.ABSBool.fromBoolean(true); }}.execute();
final abs.backend.java.lib.expr.PatternBinding __ABS_binding2 = new abs.backend.java.lib.expr.PatternConstructor("Insert",new abs.backend.java.lib.expr.AnyPattern(),new abs.backend.java.lib.expr.PatternVariable("xs")).match(__ABS_value);
if (__ABS_binding2 != null) return new Object() {
  public abs.backend.java.lib.types.ABSBool execute(final Set<A> xs) { return ABSFunction__contains.apply(xs, e); }}.execute((Set<A>) __ABS_binding2.getBinding(0));
throw new abs.backend.java.lib.expr.UnmatchedCaseException();
}}.of(ss);
   }
}
final class ABSFunction__emptySet implements ABSFunction {
   private ABSFunction__emptySet() { }
   public static <A extends abs.backend.java.lib.types.ABSDataType>abs.backend.java.lib.types.ABSBool apply(final Set xs) {
      return xs.eq(new ABSConstructor__EmptySet());
   }
}
final class ABSFunction__size implements ABSFunction {
   private ABSFunction__size() { }
   public static <A extends abs.backend.java.lib.types.ABSDataType>abs.backend.java.lib.types.ABSInteger apply(final Set xs) {
      return new Case<Set<A>,abs.backend.java.lib.types.ABSInteger>() {
      public abs.backend.java.lib.types.ABSInteger of(final Set<A> __ABS_value) { 
final abs.backend.java.lib.expr.PatternBinding __ABS_binding0 = new abs.backend.java.lib.expr.PatternConstructor("EmptySet").match(__ABS_value);
if (__ABS_binding0 != null) return new Object() {
  public abs.backend.java.lib.types.ABSInteger execute() { return abs.backend.java.lib.types.ABSInteger.fromString("0"); }}.execute();
final abs.backend.java.lib.expr.PatternBinding __ABS_binding1 = new abs.backend.java.lib.expr.PatternConstructor("Insert",new abs.backend.java.lib.expr.PatternVariable("s"),new abs.backend.java.lib.expr.PatternVariable("ss")).match(__ABS_value);
if (__ABS_binding1 != null) return new Object() {
  public abs.backend.java.lib.types.ABSInteger execute(final A s,final Set<A> ss) { return abs.backend.java.lib.types.ABSInteger.fromString("1").add(ABSFunction__size.apply(ss)); }}.execute((A) __ABS_binding1.getBinding(0),(Set<A>) __ABS_binding1.getBinding(1));
throw new abs.backend.java.lib.expr.UnmatchedCaseException();
}}.of(xs);
   }
}
final class ABSFunction__remove implements ABSFunction {
   private ABSFunction__remove() { }
   public static <A extends abs.backend.java.lib.types.ABSDataType>Set apply(final Set xs, final A e) {
      return new Case<Set<A>,Set<A>>() {
      public Set<A> of(final Set<A> __ABS_value) { 
final abs.backend.java.lib.expr.PatternBinding __ABS_binding0 = new abs.backend.java.lib.expr.PatternConstructor("EmptySet").match(__ABS_value);
if (__ABS_binding0 != null) return new Object() {
  public Set<A> execute() { return new ABSConstructor__EmptySet(); }}.execute();
final abs.backend.java.lib.expr.PatternBinding __ABS_binding1 = new abs.backend.java.lib.expr.PatternConstructor("Insert",new abs.backend.java.lib.expr.PatternValue(e),new abs.backend.java.lib.expr.PatternVariable("ss")).match(__ABS_value);
if (__ABS_binding1 != null) return new Object() {
  public Set<A> execute(final Set<A> ss) { return ss; }}.execute((Set<A>) __ABS_binding1.getBinding(0));
final abs.backend.java.lib.expr.PatternBinding __ABS_binding2 = new abs.backend.java.lib.expr.PatternConstructor("Insert",new abs.backend.java.lib.expr.PatternVariable("s"),new abs.backend.java.lib.expr.PatternVariable("ss")).match(__ABS_value);
if (__ABS_binding2 != null) return new Object() {
  public Set<A> execute(final A s,final Set<A> ss) { return new ABSConstructor__Insert(s, ABSFunction__remove.apply(ss, e)); }}.execute((A) __ABS_binding2.getBinding(0),(Set<A>) __ABS_binding2.getBinding(1));
throw new abs.backend.java.lib.expr.UnmatchedCaseException();
}}.of(xs);
   }
}
final class ABSFunction__hasNext implements ABSFunction {
   private ABSFunction__hasNext() { }
   public static <A extends abs.backend.java.lib.types.ABSDataType>abs.backend.java.lib.types.ABSBool apply(final Set s) {
      return ABSFunction__emptySet.apply(s).negate();
   }
}
final class ABSFunction__next implements ABSFunction {
   private ABSFunction__next() { }
   public static <A extends abs.backend.java.lib.types.ABSDataType>Pair apply(final Set s) {
      return new Case<Set<A>,Pair<Set<A>,A>>() {
      public Pair<Set<A>,A> of(final Set<A> __ABS_value) { 
final abs.backend.java.lib.expr.PatternBinding __ABS_binding0 = new abs.backend.java.lib.expr.PatternConstructor("Insert",new abs.backend.java.lib.expr.PatternVariable("e"),new abs.backend.java.lib.expr.PatternVariable("set2")).match(__ABS_value);
if (__ABS_binding0 != null) return new Object() {
  public Pair<Set<A>,A> execute(final A e,final Set<A> set2) { return new ABSConstructor__Pair(set2, e); }}.execute((A) __ABS_binding0.getBinding(0),(Set<A>) __ABS_binding0.getBinding(1));
throw new abs.backend.java.lib.expr.UnmatchedCaseException();
}}.of(s);
   }
}
abstract class List<A extends abs.backend.java.lib.types.ABSDataType> extends ABSDataType {
}
class ABSConstructor__Nil<A extends abs.backend.java.lib.types.ABSDataType> extends List<A> {
   public ABSConstructor__Nil() {
   }
   public ABSBool eq(ABSDataType o) {
      if (! (o instanceof ABSConstructor__Nil)) return ABSBool.FALSE;
      ABSConstructor__Nil other = (ABSConstructor__Nil) o;
      return ABSBool.TRUE;
   }
   public boolean match(abs.backend.java.lib.expr.PatternConstructor c, abs.backend.java.lib.expr.PatternBinding b) {
       if (!c.constructorName.equals("Nil")) return false;
      return true;
   }
}
class ABSConstructor__Cons<A extends abs.backend.java.lib.types.ABSDataType> extends List<A> {
   public final A arg0;
   public final List arg1;
   public ABSConstructor__Cons(   final A arg0,   final List arg1) {
this.arg0 = arg0;
this.arg1 = arg1;
   }
   public ABSBool eq(ABSDataType o) {
      if (! (o instanceof ABSConstructor__Cons)) return ABSBool.FALSE;
      ABSConstructor__Cons other = (ABSConstructor__Cons) o;
if (!this.arg0.eq(other.arg0).toBoolean()) return ABSBool.FALSE;
if (!this.arg1.eq(other.arg1).toBoolean()) return ABSBool.FALSE;
      return ABSBool.TRUE;
   }
   public boolean match(abs.backend.java.lib.expr.PatternConstructor c, abs.backend.java.lib.expr.PatternBinding b) {
       if (!c.constructorName.equals("Cons")) return false;
       if (!c.subpattern[0].match(this.arg0,b)) return false;
       if (!c.subpattern[1].match(this.arg1,b)) return false;
      return true;
   }
}
final class ABSFunction__list implements ABSFunction {
   private ABSFunction__list() { }
   public static <A extends abs.backend.java.lib.types.ABSDataType>List apply(final List l) {
      return l;
   }
}
final class ABSFunction__length implements ABSFunction {
   private ABSFunction__length() { }
   public static <A extends abs.backend.java.lib.types.ABSDataType>abs.backend.java.lib.types.ABSInteger apply(final List list) {
      return new Case<List<A>,abs.backend.java.lib.types.ABSInteger>() {
      public abs.backend.java.lib.types.ABSInteger of(final List<A> __ABS_value) { 
final abs.backend.java.lib.expr.PatternBinding __ABS_binding0 = new abs.backend.java.lib.expr.PatternConstructor("Nil").match(__ABS_value);
if (__ABS_binding0 != null) return new Object() {
  public abs.backend.java.lib.types.ABSInteger execute() { return abs.backend.java.lib.types.ABSInteger.fromString("0"); }}.execute();
final abs.backend.java.lib.expr.PatternBinding __ABS_binding1 = new abs.backend.java.lib.expr.PatternConstructor("Cons",new abs.backend.java.lib.expr.PatternVariable("p"),new abs.backend.java.lib.expr.PatternVariable("l")).match(__ABS_value);
if (__ABS_binding1 != null) return new Object() {
  public abs.backend.java.lib.types.ABSInteger execute(final A p,final List<A> l) { return abs.backend.java.lib.types.ABSInteger.fromString("1").add(ABSFunction__length.apply(l)); }}.execute((A) __ABS_binding1.getBinding(0),(List<A>) __ABS_binding1.getBinding(1));
throw new abs.backend.java.lib.expr.UnmatchedCaseException();
}}.of(list);
   }
}
final class ABSFunction__isEmpty implements ABSFunction {
   private ABSFunction__isEmpty() { }
   public static <A extends abs.backend.java.lib.types.ABSDataType>abs.backend.java.lib.types.ABSBool apply(final List list) {
      return list.eq(new ABSConstructor__Nil());
   }
}
final class ABSFunction__head implements ABSFunction {
   private ABSFunction__head() { }
   public static <A extends abs.backend.java.lib.types.ABSDataType>A apply(final List list) {
      return new Case<List<A>,A>() {
      public A of(final List<A> __ABS_value) { 
final abs.backend.java.lib.expr.PatternBinding __ABS_binding0 = new abs.backend.java.lib.expr.PatternConstructor("Cons",new abs.backend.java.lib.expr.PatternVariable("p"),new abs.backend.java.lib.expr.PatternVariable("l")).match(__ABS_value);
if (__ABS_binding0 != null) return new Object() {
  public A execute(final A p,final List<A> l) { return p; }}.execute((A) __ABS_binding0.getBinding(0),(List<A>) __ABS_binding0.getBinding(1));
throw new abs.backend.java.lib.expr.UnmatchedCaseException();
}}.of(list);
   }
}
final class ABSFunction__tail implements ABSFunction {
   private ABSFunction__tail() { }
   public static <A extends abs.backend.java.lib.types.ABSDataType>List apply(final List list) {
      return new Case<List<A>,List<A>>() {
      public List<A> of(final List<A> __ABS_value) { 
final abs.backend.java.lib.expr.PatternBinding __ABS_binding0 = new abs.backend.java.lib.expr.PatternConstructor("Cons",new abs.backend.java.lib.expr.PatternVariable("p"),new abs.backend.java.lib.expr.PatternVariable("l")).match(__ABS_value);
if (__ABS_binding0 != null) return new Object() {
  public List<A> execute(final A p,final List<A> l) { return l; }}.execute((A) __ABS_binding0.getBinding(0),(List<A>) __ABS_binding0.getBinding(1));
throw new abs.backend.java.lib.expr.UnmatchedCaseException();
}}.of(list);
   }
}
final class ABSFunction__nth implements ABSFunction {
   private ABSFunction__nth() { }
   public static <A extends abs.backend.java.lib.types.ABSDataType>A apply(final List list, final abs.backend.java.lib.types.ABSInteger n) {
      return new Case<abs.backend.java.lib.types.ABSInteger,A>() {
      public A of(final abs.backend.java.lib.types.ABSInteger __ABS_value) { 
final abs.backend.java.lib.expr.PatternBinding __ABS_binding0 = new abs.backend.java.lib.expr.PatternValue(abs.backend.java.lib.types.ABSInteger.fromString("0")).match(__ABS_value);
if (__ABS_binding0 != null) return new Object() {
  public A execute() { return ABSFunction__head.apply(list); }}.execute();
final abs.backend.java.lib.expr.PatternBinding __ABS_binding1 = new abs.backend.java.lib.expr.AnyPattern().match(__ABS_value);
if (__ABS_binding1 != null) return new Object() {
  public A execute() { return ABSFunction__nth.apply(ABSFunction__tail.apply(list), n.subtract(abs.backend.java.lib.types.ABSInteger.fromString("1"))); }}.execute();
throw new abs.backend.java.lib.expr.UnmatchedCaseException();
}}.of(n);
   }
}
final class ABSFunction__concatenate implements ABSFunction {
   private ABSFunction__concatenate() { }
   public static <A extends abs.backend.java.lib.types.ABSDataType>List apply(final List list1, final List list2) {
      return new Case<List<A>,List<A>>() {
      public List<A> of(final List<A> __ABS_value) { 
final abs.backend.java.lib.expr.PatternBinding __ABS_binding0 = new abs.backend.java.lib.expr.PatternConstructor("Nil").match(__ABS_value);
if (__ABS_binding0 != null) return new Object() {
  public List<A> execute() { return list2; }}.execute();
final abs.backend.java.lib.expr.PatternBinding __ABS_binding1 = new abs.backend.java.lib.expr.PatternConstructor("Cons",new abs.backend.java.lib.expr.PatternVariable("head"),new abs.backend.java.lib.expr.PatternVariable("tail")).match(__ABS_value);
if (__ABS_binding1 != null) return new Object() {
  public List<A> execute(final A head,final List<A> tail) { return new ABSConstructor__Cons(head, ABSFunction__concatenate.apply(tail, list2)); }}.execute((A) __ABS_binding1.getBinding(0),(List<A>) __ABS_binding1.getBinding(1));
throw new abs.backend.java.lib.expr.UnmatchedCaseException();
}}.of(list1);
   }
}
final class ABSFunction__appendright implements ABSFunction {
   private ABSFunction__appendright() { }
   public static <A extends abs.backend.java.lib.types.ABSDataType>List apply(final List list, final A p) {
      return ABSFunction__concatenate.apply(list, new ABSConstructor__Cons(p, new ABSConstructor__Nil()));
   }
}
final class ABSFunction__reverse implements ABSFunction {
   private ABSFunction__reverse() { }
   public static <A extends abs.backend.java.lib.types.ABSDataType>List apply(final List list) {
      return new Case<List<A>,List<?>>() {
      public List<?> of(final List<A> __ABS_value) { 
final abs.backend.java.lib.expr.PatternBinding __ABS_binding0 = new abs.backend.java.lib.expr.PatternConstructor("Cons",new abs.backend.java.lib.expr.PatternVariable("hd"),new abs.backend.java.lib.expr.PatternVariable("tl")).match(__ABS_value);
if (__ABS_binding0 != null) return new Object() {
  public List<?> execute(final A hd,final List<A> tl) { return ABSFunction__appendright.apply(ABSFunction__reverse.apply(tl), hd); }}.execute((A) __ABS_binding0.getBinding(0),(List<A>) __ABS_binding0.getBinding(1));
final abs.backend.java.lib.expr.PatternBinding __ABS_binding1 = new abs.backend.java.lib.expr.PatternConstructor("Nil").match(__ABS_value);
if (__ABS_binding1 != null) return new Object() {
  public List<?> execute() { return new ABSConstructor__Nil(); }}.execute();
throw new abs.backend.java.lib.expr.UnmatchedCaseException();
}}.of(list);
   }
}
final class ABSFunction__copy implements ABSFunction {
   private ABSFunction__copy() { }
   public static <A extends abs.backend.java.lib.types.ABSDataType>List apply(final A p, final abs.backend.java.lib.types.ABSInteger n) {
      return new Case<abs.backend.java.lib.types.ABSInteger,List<A>>() {
      public List<A> of(final abs.backend.java.lib.types.ABSInteger __ABS_value) { 
final abs.backend.java.lib.expr.PatternBinding __ABS_binding0 = new abs.backend.java.lib.expr.PatternValue(abs.backend.java.lib.types.ABSInteger.fromString("0")).match(__ABS_value);
if (__ABS_binding0 != null) return new Object() {
  public List<A> execute() { return new ABSConstructor__Nil(); }}.execute();
final abs.backend.java.lib.expr.PatternBinding __ABS_binding1 = new abs.backend.java.lib.expr.PatternVariable("m").match(__ABS_value);
if (__ABS_binding1 != null) return new Object() {
  public List<A> execute(final abs.backend.java.lib.types.ABSInteger m) { return new ABSConstructor__Cons(p, ABSFunction__copy.apply(p, m.subtract(abs.backend.java.lib.types.ABSInteger.fromString("1")))); }}.execute((abs.backend.java.lib.types.ABSInteger) __ABS_binding1.getBinding(0));
throw new abs.backend.java.lib.expr.UnmatchedCaseException();
}}.of(n);
   }
}
abstract class Map<A extends abs.backend.java.lib.types.ABSDataType,B extends abs.backend.java.lib.types.ABSDataType> extends ABSDataType {
}
class ABSConstructor__EmptyMap<A extends abs.backend.java.lib.types.ABSDataType,B extends abs.backend.java.lib.types.ABSDataType> extends Map<A,B> {
   public ABSConstructor__EmptyMap() {
   }
   public ABSBool eq(ABSDataType o) {
      if (! (o instanceof ABSConstructor__EmptyMap)) return ABSBool.FALSE;
      ABSConstructor__EmptyMap other = (ABSConstructor__EmptyMap) o;
      return ABSBool.TRUE;
   }
   public boolean match(abs.backend.java.lib.expr.PatternConstructor c, abs.backend.java.lib.expr.PatternBinding b) {
       if (!c.constructorName.equals("EmptyMap")) return false;
      return true;
   }
}
class ABSConstructor__InsertAssoc<A extends abs.backend.java.lib.types.ABSDataType,B extends abs.backend.java.lib.types.ABSDataType> extends Map<A,B> {
   public final Pair arg0;
   public final Map arg1;
   public ABSConstructor__InsertAssoc(   final Pair arg0,   final Map arg1) {
this.arg0 = arg0;
this.arg1 = arg1;
   }
   public ABSBool eq(ABSDataType o) {
      if (! (o instanceof ABSConstructor__InsertAssoc)) return ABSBool.FALSE;
      ABSConstructor__InsertAssoc other = (ABSConstructor__InsertAssoc) o;
if (!this.arg0.eq(other.arg0).toBoolean()) return ABSBool.FALSE;
if (!this.arg1.eq(other.arg1).toBoolean()) return ABSBool.FALSE;
      return ABSBool.TRUE;
   }
   public boolean match(abs.backend.java.lib.expr.PatternConstructor c, abs.backend.java.lib.expr.PatternBinding b) {
       if (!c.constructorName.equals("InsertAssoc")) return false;
       if (!c.subpattern[0].match(this.arg0,b)) return false;
       if (!c.subpattern[1].match(this.arg1,b)) return false;
      return true;
   }
}
final class ABSFunction__map implements ABSFunction {
   private ABSFunction__map() { }
   public static <A extends abs.backend.java.lib.types.ABSDataType,B extends abs.backend.java.lib.types.ABSDataType>Map apply(final List l) {
      return new Case<List<Pair<A,B>>,Map<A,B>>() {
      public Map<A,B> of(final List<Pair<A,B>> __ABS_value) { 
final abs.backend.java.lib.expr.PatternBinding __ABS_binding0 = new abs.backend.java.lib.expr.PatternConstructor("Nil").match(__ABS_value);
if (__ABS_binding0 != null) return new Object() {
  public Map<A,B> execute() { return new ABSConstructor__EmptyMap(); }}.execute();
final abs.backend.java.lib.expr.PatternBinding __ABS_binding1 = new abs.backend.java.lib.expr.PatternConstructor("Cons",new abs.backend.java.lib.expr.PatternVariable("hd"),new abs.backend.java.lib.expr.PatternVariable("tl")).match(__ABS_value);
if (__ABS_binding1 != null) return new Object() {
  public Map<A,B> execute(final Pair<A,B> hd,final List<Pair<A,B>> tl) { return new ABSConstructor__InsertAssoc(hd, ABSFunction__map.apply(tl)); }}.execute((Pair<A,B>) __ABS_binding1.getBinding(0),(List<Pair<A,B>>) __ABS_binding1.getBinding(1));
throw new abs.backend.java.lib.expr.UnmatchedCaseException();
}}.of(l);
   }
}
final class ABSFunction__keys implements ABSFunction {
   private ABSFunction__keys() { }
   public static <A extends abs.backend.java.lib.types.ABSDataType,B extends abs.backend.java.lib.types.ABSDataType>Set apply(final Map map) {
      return new Case<Map<A,B>,Set<A>>() {
      public Set<A> of(final Map<A,B> __ABS_value) { 
final abs.backend.java.lib.expr.PatternBinding __ABS_binding0 = new abs.backend.java.lib.expr.PatternConstructor("EmptyMap").match(__ABS_value);
if (__ABS_binding0 != null) return new Object() {
  public Set<A> execute() { return new ABSConstructor__EmptySet(); }}.execute();
final abs.backend.java.lib.expr.PatternBinding __ABS_binding1 = new abs.backend.java.lib.expr.PatternConstructor("InsertAssoc",new abs.backend.java.lib.expr.PatternConstructor("Pair",new abs.backend.java.lib.expr.PatternVariable("a"),new abs.backend.java.lib.expr.AnyPattern()),new abs.backend.java.lib.expr.PatternVariable("tail")).match(__ABS_value);
if (__ABS_binding1 != null) return new Object() {
  public Set<A> execute(final A a,final Map<A,B> tail) { return new ABSConstructor__Insert(a, ABSFunction__keys.apply(tail)); }}.execute((A) __ABS_binding1.getBinding(0),(Map<A,B>) __ABS_binding1.getBinding(1));
throw new abs.backend.java.lib.expr.UnmatchedCaseException();
}}.of(map);
   }
}
final class ABSFunction__lookup implements ABSFunction {
   private ABSFunction__lookup() { }
   public static <A extends abs.backend.java.lib.types.ABSDataType,B extends abs.backend.java.lib.types.ABSDataType>B apply(final Map ms, final A k) {
      return new Case<Map<A,B>,B>() {
      public B of(final Map<A,B> __ABS_value) { 
final abs.backend.java.lib.expr.PatternBinding __ABS_binding0 = new abs.backend.java.lib.expr.PatternConstructor("InsertAssoc",new abs.backend.java.lib.expr.PatternConstructor("Pair",new abs.backend.java.lib.expr.PatternValue(k),new abs.backend.java.lib.expr.PatternVariable("y")),new abs.backend.java.lib.expr.AnyPattern()).match(__ABS_value);
if (__ABS_binding0 != null) return new Object() {
  public B execute(final B y) { return y; }}.execute((B) __ABS_binding0.getBinding(0));
final abs.backend.java.lib.expr.PatternBinding __ABS_binding1 = new abs.backend.java.lib.expr.PatternConstructor("InsertAssoc",new abs.backend.java.lib.expr.AnyPattern(),new abs.backend.java.lib.expr.PatternVariable("tm")).match(__ABS_value);
if (__ABS_binding1 != null) return new Object() {
  public B execute(final Map<A,B> tm) { return ABSFunction__lookup.apply(tm, k); }}.execute((Map<A,B>) __ABS_binding1.getBinding(0));
throw new abs.backend.java.lib.expr.UnmatchedCaseException();
}}.of(ms);
   }
}
final class ABSFunction__lookupDefault implements ABSFunction {
   private ABSFunction__lookupDefault() { }
   public static <A extends abs.backend.java.lib.types.ABSDataType,B extends abs.backend.java.lib.types.ABSDataType>B apply(final Map ms, final A k, final B d) {
      return new Case<Map<A,B>,B>() {
      public B of(final Map<A,B> __ABS_value) { 
final abs.backend.java.lib.expr.PatternBinding __ABS_binding0 = new abs.backend.java.lib.expr.PatternConstructor("InsertAssoc",new abs.backend.java.lib.expr.PatternConstructor("Pair",new abs.backend.java.lib.expr.PatternValue(k),new abs.backend.java.lib.expr.PatternVariable("y")),new abs.backend.java.lib.expr.AnyPattern()).match(__ABS_value);
if (__ABS_binding0 != null) return new Object() {
  public B execute(final B y) { return y; }}.execute((B) __ABS_binding0.getBinding(0));
final abs.backend.java.lib.expr.PatternBinding __ABS_binding1 = new abs.backend.java.lib.expr.PatternConstructor("InsertAssoc",new abs.backend.java.lib.expr.AnyPattern(),new abs.backend.java.lib.expr.PatternVariable("tm")).match(__ABS_value);
if (__ABS_binding1 != null) return new Object() {
  public B execute(final Map<A,B> tm) { return ABSFunction__lookupDefault.apply(tm, k, d); }}.execute((Map<A,B>) __ABS_binding1.getBinding(0));
final abs.backend.java.lib.expr.PatternBinding __ABS_binding2 = new abs.backend.java.lib.expr.PatternConstructor("EmptyMap").match(__ABS_value);
if (__ABS_binding2 != null) return new Object() {
  public B execute() { return d; }}.execute();
throw new abs.backend.java.lib.expr.UnmatchedCaseException();
}}.of(ms);
   }
}
final class ABSFunction__insert implements ABSFunction {
   private ABSFunction__insert() { }
   public static <A extends abs.backend.java.lib.types.ABSDataType,B extends abs.backend.java.lib.types.ABSDataType>Map apply(final Map map, final Pair p) {
      return new ABSConstructor__InsertAssoc(p, map);
   }
}
final class ABSFunction__put implements ABSFunction {
   private ABSFunction__put() { }
   public static <A extends abs.backend.java.lib.types.ABSDataType,B extends abs.backend.java.lib.types.ABSDataType>Map apply(final Map ms, final A k, final B v) {
      return new Case<Map<A,B>,Map<A,B>>() {
      public Map<A,B> of(final Map<A,B> __ABS_value) { 
final abs.backend.java.lib.expr.PatternBinding __ABS_binding0 = new abs.backend.java.lib.expr.PatternConstructor("EmptyMap").match(__ABS_value);
if (__ABS_binding0 != null) return new Object() {
  public Map<A,B> execute() { return new ABSConstructor__InsertAssoc(new ABSConstructor__Pair(k, v), new ABSConstructor__EmptyMap()); }}.execute();
final abs.backend.java.lib.expr.PatternBinding __ABS_binding1 = new abs.backend.java.lib.expr.PatternConstructor("InsertAssoc",new abs.backend.java.lib.expr.PatternConstructor("Pair",new abs.backend.java.lib.expr.PatternValue(k),new abs.backend.java.lib.expr.AnyPattern()),new abs.backend.java.lib.expr.PatternVariable("ts")).match(__ABS_value);
if (__ABS_binding1 != null) return new Object() {
  public Map<A,B> execute(final Map<A,B> ts) { return new ABSConstructor__InsertAssoc(new ABSConstructor__Pair(k, v), ts); }}.execute((Map<A,B>) __ABS_binding1.getBinding(0));
final abs.backend.java.lib.expr.PatternBinding __ABS_binding2 = new abs.backend.java.lib.expr.PatternConstructor("InsertAssoc",new abs.backend.java.lib.expr.PatternVariable("p"),new abs.backend.java.lib.expr.PatternVariable("ts")).match(__ABS_value);
if (__ABS_binding2 != null) return new Object() {
  public Map<A,B> execute(final Pair<A,B> p,final Map<A,B> ts) { return new ABSConstructor__InsertAssoc(p, ABSFunction__put.apply(ts, k, v)); }}.execute((Pair<A,B>) __ABS_binding2.getBinding(0),(Map<A,B>) __ABS_binding2.getBinding(1));
throw new abs.backend.java.lib.expr.UnmatchedCaseException();
}}.of(ms);
   }
}
final class ABSFunction__intToString implements ABSFunction {
   private ABSFunction__intToString() { }
   public static abs.backend.java.lib.types.ABSString apply(final abs.backend.java.lib.types.ABSInteger n) {
      return new Case<abs.backend.java.lib.types.ABSBool,abs.backend.java.lib.types.ABSString>() {
      public abs.backend.java.lib.types.ABSString of(final abs.backend.java.lib.types.ABSBool __ABS_value) { 
final abs.backend.java.lib.expr.PatternBinding __ABS_binding0 = new abs.backend.java.lib.expr.PatternConstructor("True").match(__ABS_value);
if (__ABS_binding0 != null) return new Object() {
  public abs.backend.java.lib.types.ABSString execute() { return abs.backend.java.lib.types.ABSString.fromString("-").add(ABSFunction__intToStringPos.apply(n.negate())); }}.execute();
final abs.backend.java.lib.expr.PatternBinding __ABS_binding1 = new abs.backend.java.lib.expr.PatternConstructor("False").match(__ABS_value);
if (__ABS_binding1 != null) return new Object() {
  public abs.backend.java.lib.types.ABSString execute() { return ABSFunction__intToStringPos.apply(n); }}.execute();
throw new abs.backend.java.lib.expr.UnmatchedCaseException();
}}.of(n.lt(abs.backend.java.lib.types.ABSInteger.fromString("0")));
   }
}
final class ABSFunction__intToStringPos implements ABSFunction {
   private ABSFunction__intToStringPos() { }
   public static abs.backend.java.lib.types.ABSString apply(final abs.backend.java.lib.types.ABSInteger n) {
      return new Let<abs.backend.java.lib.types.ABSInteger,abs.backend.java.lib.types.ABSString>() { public abs.backend.java.lib.types.ABSString in(final abs.backend.java.lib.types.ABSInteger div) { return new Let<abs.backend.java.lib.types.ABSInteger,abs.backend.java.lib.types.ABSString>() { public abs.backend.java.lib.types.ABSString in(final abs.backend.java.lib.types.ABSInteger res) { return new Case<abs.backend.java.lib.types.ABSInteger,abs.backend.java.lib.types.ABSString>() {
      public abs.backend.java.lib.types.ABSString of(final abs.backend.java.lib.types.ABSInteger __ABS_value) { 
final abs.backend.java.lib.expr.PatternBinding __ABS_binding0 = new abs.backend.java.lib.expr.PatternValue(abs.backend.java.lib.types.ABSInteger.fromString("0")).match(__ABS_value);
if (__ABS_binding0 != null) return new Object() {
  public abs.backend.java.lib.types.ABSString execute() { return abs.backend.java.lib.types.ABSString.fromString("0"); }}.execute();
final abs.backend.java.lib.expr.PatternBinding __ABS_binding1 = new abs.backend.java.lib.expr.PatternValue(abs.backend.java.lib.types.ABSInteger.fromString("1")).match(__ABS_value);
if (__ABS_binding1 != null) return new Object() {
  public abs.backend.java.lib.types.ABSString execute() { return abs.backend.java.lib.types.ABSString.fromString("1"); }}.execute();
final abs.backend.java.lib.expr.PatternBinding __ABS_binding2 = new abs.backend.java.lib.expr.PatternValue(abs.backend.java.lib.types.ABSInteger.fromString("2")).match(__ABS_value);
if (__ABS_binding2 != null) return new Object() {
  public abs.backend.java.lib.types.ABSString execute() { return abs.backend.java.lib.types.ABSString.fromString("2"); }}.execute();
final abs.backend.java.lib.expr.PatternBinding __ABS_binding3 = new abs.backend.java.lib.expr.PatternValue(abs.backend.java.lib.types.ABSInteger.fromString("3")).match(__ABS_value);
if (__ABS_binding3 != null) return new Object() {
  public abs.backend.java.lib.types.ABSString execute() { return abs.backend.java.lib.types.ABSString.fromString("3"); }}.execute();
final abs.backend.java.lib.expr.PatternBinding __ABS_binding4 = new abs.backend.java.lib.expr.PatternValue(abs.backend.java.lib.types.ABSInteger.fromString("4")).match(__ABS_value);
if (__ABS_binding4 != null) return new Object() {
  public abs.backend.java.lib.types.ABSString execute() { return abs.backend.java.lib.types.ABSString.fromString("4"); }}.execute();
final abs.backend.java.lib.expr.PatternBinding __ABS_binding5 = new abs.backend.java.lib.expr.PatternValue(abs.backend.java.lib.types.ABSInteger.fromString("5")).match(__ABS_value);
if (__ABS_binding5 != null) return new Object() {
  public abs.backend.java.lib.types.ABSString execute() { return abs.backend.java.lib.types.ABSString.fromString("5"); }}.execute();
final abs.backend.java.lib.expr.PatternBinding __ABS_binding6 = new abs.backend.java.lib.expr.PatternValue(abs.backend.java.lib.types.ABSInteger.fromString("6")).match(__ABS_value);
if (__ABS_binding6 != null) return new Object() {
  public abs.backend.java.lib.types.ABSString execute() { return abs.backend.java.lib.types.ABSString.fromString("6"); }}.execute();
final abs.backend.java.lib.expr.PatternBinding __ABS_binding7 = new abs.backend.java.lib.expr.PatternValue(abs.backend.java.lib.types.ABSInteger.fromString("7")).match(__ABS_value);
if (__ABS_binding7 != null) return new Object() {
  public abs.backend.java.lib.types.ABSString execute() { return abs.backend.java.lib.types.ABSString.fromString("7"); }}.execute();
final abs.backend.java.lib.expr.PatternBinding __ABS_binding8 = new abs.backend.java.lib.expr.PatternValue(abs.backend.java.lib.types.ABSInteger.fromString("8")).match(__ABS_value);
if (__ABS_binding8 != null) return new Object() {
  public abs.backend.java.lib.types.ABSString execute() { return abs.backend.java.lib.types.ABSString.fromString("8"); }}.execute();
final abs.backend.java.lib.expr.PatternBinding __ABS_binding9 = new abs.backend.java.lib.expr.PatternValue(abs.backend.java.lib.types.ABSInteger.fromString("9")).match(__ABS_value);
if (__ABS_binding9 != null) return new Object() {
  public abs.backend.java.lib.types.ABSString execute() { return abs.backend.java.lib.types.ABSString.fromString("9"); }}.execute();
final abs.backend.java.lib.expr.PatternBinding __ABS_binding10 = new abs.backend.java.lib.expr.AnyPattern().match(__ABS_value);
if (__ABS_binding10 != null) return new Object() {
  public abs.backend.java.lib.types.ABSString execute() { return ABSFunction__intToStringPos.apply(div).add(ABSFunction__intToStringPos.apply(res)); }}.execute();
throw new abs.backend.java.lib.expr.UnmatchedCaseException();
}}.of(n); }}.in(n.mod(abs.backend.java.lib.types.ABSInteger.fromString("10"))); }}.in(n.divide(abs.backend.java.lib.types.ABSInteger.fromString("10")));
   }
}
final class ABSFunction__substr implements ABSFunction {
   private ABSFunction__substr() { }
   public static abs.backend.java.lib.types.ABSString apply(final abs.backend.java.lib.types.ABSString str, final abs.backend.java.lib.types.ABSInteger start, final abs.backend.java.lib.types.ABSInteger length) {
      return null;
   }
}
final class ABSFunction__strlen implements ABSFunction {
   private ABSFunction__strlen() { }
   public static abs.backend.java.lib.types.ABSInteger apply(final abs.backend.java.lib.types.ABSString str) {
      return null;
   }
}
interface I extends ABSInterfaceType {
   public abs.backend.java.lib.types.ABSBool m();
   public abs.backend.java.lib.types.ABSUnit n();
}
class C extends abs.backend.java.lib.runtime.ABSObject implements abs.backend.java.lib.types.ABSClassType, I {
   public C() {
   }
   public static C __ABS_createNewCOG() {
       final abs.backend.java.lib.runtime.COG __ABS_cog = new abs.backend.java.lib.runtime.COG(C.class);
       final abs.backend.java.lib.runtime.ABSThread __ABS_thread = abs.backend.java.lib.runtime.ABSRuntime.getCurrentThread();
       final abs.backend.java.lib.runtime.COG __ABS_oldCOG = abs.backend.java.lib.runtime.ABSRuntime.getCurrentCOG();
       __ABS_thread.setCOG(__ABS_cog);
       try { 
          final C __ABS_result = new C();
       } finally {
           __ABS_thread.setCOG(__ABS_oldCOG);
       }
       return null;
   }
   public abs.backend.java.lib.types.ABSUnit n(){ __ABS_checkSameCOG(); 
 {
   return ABSUnit.UNIT;
   }
}   public abs.backend.java.lib.types.ABSBool m(){ __ABS_checkSameCOG(); 
 {
      return abs.backend.java.lib.types.ABSBool.fromBoolean(true);
   }
}}
