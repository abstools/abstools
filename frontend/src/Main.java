import abs.backend.java.lib.types.*; import abs.backend.java.lib.expr.*; import abs.backend.java.lib.expr.*;
public class Main extends abs.backend.java.lib.runtime.ABSObject {
   public static void main(java.lang.String[] args) {
       abs.backend.java.lib.runtime.ABSRuntime.systemStarted();
       abs.backend.java.lib.runtime.COG cog = new abs.backend.java.lib.runtime.COG(Main.class);
       Main main = new Main(cog);
       abs.backend.java.lib.runtime.ABSRuntime.cogCreated(main);
       abs.backend.java.lib.runtime.ABSRuntime.asyncCall(new MainTask(main));
   }
   public java.lang.String getClassName() { return "Main"; }
   public Main(abs.backend.java.lib.runtime.COG cog) { super(cog); }
   static class MainTask extends abs.backend.java.lib.runtime.Task<Main> {
      public MainTask(Main target) { super(null,target); }
      public Object execute() { target.start(); return null;}
      public java.lang.String methodName() { return "start"; }
   }
   public void start()  {
      Map items = map__Function.<abs.backend.java.lib.types.ABSInteger,Map>apply(new Cons__Constructor(new Pair__Constructor(abs.backend.java.lib.types.ABSInteger.fromString("1"), map__Function.<abs.backend.java.lib.types.ABSInteger,abs.backend.java.lib.types.ABSInteger>apply(new Cons__Constructor(new Pair__Constructor(abs.backend.java.lib.types.ABSInteger.fromString("1"), abs.backend.java.lib.types.ABSInteger.fromString("2")), new Cons__Constructor(new Pair__Constructor(abs.backend.java.lib.types.ABSInteger.fromString("2"), abs.backend.java.lib.types.ABSInteger.fromString("1")), new Nil__Constructor())))), new Cons__Constructor(new Pair__Constructor(abs.backend.java.lib.types.ABSInteger.fromString("2"), map__Function.<abs.backend.java.lib.types.ABSInteger,abs.backend.java.lib.types.ABSInteger>apply(new Cons__Constructor(new Pair__Constructor(abs.backend.java.lib.types.ABSInteger.fromString("2"), abs.backend.java.lib.types.ABSInteger.fromString("2")), new Cons__Constructor(new Pair__Constructor(abs.backend.java.lib.types.ABSInteger.fromString("1"), abs.backend.java.lib.types.ABSInteger.fromString("2")), new Nil__Constructor())))), new Cons__Constructor(new Pair__Constructor(abs.backend.java.lib.types.ABSInteger.fromString("3"), map__Function.apply(new Nil__Constructor())), new Cons__Constructor(new Pair__Constructor(abs.backend.java.lib.types.ABSInteger.fromString("4"), map__Function.<abs.backend.java.lib.types.ABSInteger,abs.backend.java.lib.types.ABSInteger>apply(new Cons__Constructor(new Pair__Constructor(abs.backend.java.lib.types.ABSInteger.fromString("2"), abs.backend.java.lib.types.ABSInteger.fromString("1")), new Cons__Constructor(new Pair__Constructor(abs.backend.java.lib.types.ABSInteger.fromString("1"), abs.backend.java.lib.types.ABSInteger.fromString("3")), new Cons__Constructor(new Pair__Constructor(abs.backend.java.lib.types.ABSInteger.fromString("3"), abs.backend.java.lib.types.ABSInteger.fromString("10")), new Nil__Constructor()))))), new Nil__Constructor())))));
      Pair start = new Pair__Constructor(new Start__Constructor(), set__Function.<State>apply(new Cons__Constructor(new WaitToBoot__Constructor(), new Nil__Constructor())));
      Pair waitToBoot = new Pair__Constructor(new WaitToBoot__Constructor(), set__Function.<State>apply(new Cons__Constructor(new Booting__Constructor(), new Cons__Constructor(new End__Constructor(), new Nil__Constructor()))));
      Pair booting = new Pair__Constructor(new Booting__Constructor(), set__Function.<State>apply(new Cons__Constructor(new WaitToBoot__Constructor(), new Cons__Constructor(new WaitToReplicate__Constructor(), new Cons__Constructor(new End__Constructor(), new Nil__Constructor())))));
      Pair waitToReplicate = new Pair__Constructor(new WaitToReplicate__Constructor(), set__Function.<State>apply(new Cons__Constructor(new WaitToBoot__Constructor(), new Cons__Constructor(new WorkOnReplicate__Constructor(), new Cons__Constructor(new End__Constructor(), new Nil__Constructor())))));
      Pair workOnReplicate = new Pair__Constructor(new WorkOnReplicate__Constructor(), set__Function.<State>apply(new Cons__Constructor(new WaitToBoot__Constructor(), new Cons__Constructor(new WaitToReplicate__Constructor(), new Cons__Constructor(new End__Constructor(), new Nil__Constructor())))));
      Map machine = map__Function.<State,Set>apply(new Cons__Constructor(start, new Cons__Constructor(waitToBoot, new Cons__Constructor(booting, new Cons__Constructor(waitToReplicate, new Cons__Constructor(workOnReplicate, new Nil__Constructor()))))));
      ServerDataBase db = null;
      abs.backend.java.lib.runtime.ABSFut acc = null;
      SyncServerAcceptor acceptor = null;
      SyncServer syncserver = null;
      ClientConnector syncclient1 = null;
      ClientConnector syncclient2 = null;
      ClientConnector syncclient3 = null;
      ClientConnector syncclient4 = null;
      ClientConnector syncclient5 = null;
      ClientConnector syncclient6 = null;
      Tester tester1 = null;
      Tester tester2 = null;
      Tester tester3 = null;
      Tester tester4 = null;
      Tester tester5 = null;
      Tester tester6 = null;
      syncclient1 = new SyncClientImpl(machine);
      syncclient2 = new SyncClientImpl(machine);
      syncclient3 = new SyncClientImpl(machine);
      syncclient4 = new SyncClientImpl(machine);
      syncclient5 = new SyncClientImpl(machine);
      syncclient6 = new SyncClientImpl(machine);
      db = new DataBaseImpl(items);
      syncserver = new SyncServerImpl(db);
      tester1 = new TesterImpl(syncserver, syncclient1);
      tester2 = new TesterImpl(syncserver, syncclient2);
      tester3 = new TesterImpl(syncserver, syncclient3);
      tester4 = new TesterImpl(syncserver, syncclient4);
      tester5 = new TesterImpl(syncserver, syncclient5);
      tester6 = new TesterImpl(syncserver, syncclient6);
      acc = abs.backend.java.lib.runtime.ABSRuntime.asyncCall(new abs.backend.java.lib.runtime.Task<SyncServer>(this,syncserver) { protected ABSValue[] getArgs() { return new ABSValue[] {  }; }     public abs.backend.java.lib.runtime.Task<?> init() { return this; } public java.lang.String methodName() { return "getAcceptor"; } public Object execute() { return target.getAcceptor(); }}
     .init());
      abs.backend.java.lib.runtime.ABSRuntime.await(new abs.backend.java.lib.runtime.ABSFutureGuard(acc));
      acceptor = (SyncServerAcceptor)acc.get();
      syncclient1.setAcceptor(acceptor);
      syncclient2.setAcceptor(acceptor);
      syncclient3.setAcceptor(acceptor);
      syncclient4.setAcceptor(acceptor);
      syncclient5.setAcceptor(acceptor);
      syncclient6.setAcceptor(acceptor);
   }
}
abstract class Unit extends ABSDataType {
}
class Unit__Constructor extends Unit {
   public Unit__Constructor() {
   }
   protected ABSValue[] getArgs() { return new ABSValue[] {};}
   public java.lang.String getConstructorName() { return "Unit" ;} 
   public ABSBool eq(ABSValue o) {
      if (! (o instanceof Unit__Constructor)) return ABSBool.FALSE;
      Unit__Constructor other = (Unit__Constructor) o;
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
class True__Constructor extends Bool {
   public True__Constructor() {
   }
   protected ABSValue[] getArgs() { return new ABSValue[] {};}
   public java.lang.String getConstructorName() { return "True" ;} 
   public ABSBool eq(ABSValue o) {
      if (! (o instanceof True__Constructor)) return ABSBool.FALSE;
      True__Constructor other = (True__Constructor) o;
      return ABSBool.TRUE;
   }
   public boolean match(abs.backend.java.lib.expr.PatternConstructor c, abs.backend.java.lib.expr.PatternBinding b) {
       if (!c.constructorName.equals("True")) return false;
      return true;
   }
}
class False__Constructor extends Bool {
   public False__Constructor() {
   }
   protected ABSValue[] getArgs() { return new ABSValue[] {};}
   public java.lang.String getConstructorName() { return "False" ;} 
   public ABSBool eq(ABSValue o) {
      if (! (o instanceof False__Constructor)) return ABSBool.FALSE;
      False__Constructor other = (False__Constructor) o;
      return ABSBool.TRUE;
   }
   public boolean match(abs.backend.java.lib.expr.PatternConstructor c, abs.backend.java.lib.expr.PatternBinding b) {
       if (!c.constructorName.equals("False")) return false;
      return true;
   }
}
abstract class Fut<A extends abs.backend.java.lib.types.ABSValue> extends ABSDataType {
}
final class and__Function implements ABSFunction {
   private and__Function() { }
   public static abs.backend.java.lib.types.ABSBool apply(abs.backend.java.lib.types.ABSBool a, abs.backend.java.lib.types.ABSBool b) {
      return new Case() {
      public abs.backend.java.lib.types.ABSBool of(final abs.backend.java.lib.types.ABSBool b, final abs.backend.java.lib.types.ABSBool a, final abs.backend.java.lib.types.ABSBool __ABS_value) { 
final abs.backend.java.lib.expr.PatternBinding __ABS_binding0 = new abs.backend.java.lib.expr.PatternConstructor("True").match(__ABS_value);
if (__ABS_binding0 != null) return new Object() {
  public abs.backend.java.lib.types.ABSBool execute() { return b; }}.execute();
final abs.backend.java.lib.expr.PatternBinding __ABS_binding1 = new abs.backend.java.lib.expr.AnyPattern().match(__ABS_value);
if (__ABS_binding1 != null) return new Object() {
  public abs.backend.java.lib.types.ABSBool execute() { return abs.backend.java.lib.types.ABSBool.fromBoolean(false); }}.execute();
throw new abs.backend.java.lib.expr.UnmatchedCaseException("/home/jan/svn/hats/CaseStudies/models/fredhopper/replication/abs/lang/abslang.abs:11:32:  value "+__ABS_value+" did not match any pattern.");
}}.of(b, a, a);
   }
}
final class not__Function implements ABSFunction {
   private not__Function() { }
   public static abs.backend.java.lib.types.ABSBool apply(abs.backend.java.lib.types.ABSBool a) {
      return new Case() {
      public abs.backend.java.lib.types.ABSBool of(final abs.backend.java.lib.types.ABSBool a, final abs.backend.java.lib.types.ABSBool __ABS_value) { 
final abs.backend.java.lib.expr.PatternBinding __ABS_binding0 = new abs.backend.java.lib.expr.PatternConstructor("True").match(__ABS_value);
if (__ABS_binding0 != null) return new Object() {
  public abs.backend.java.lib.types.ABSBool execute() { return abs.backend.java.lib.types.ABSBool.fromBoolean(false); }}.execute();
final abs.backend.java.lib.expr.PatternBinding __ABS_binding1 = new abs.backend.java.lib.expr.PatternConstructor("False").match(__ABS_value);
if (__ABS_binding1 != null) return new Object() {
  public abs.backend.java.lib.types.ABSBool execute() { return abs.backend.java.lib.types.ABSBool.fromBoolean(true); }}.execute();
throw new abs.backend.java.lib.expr.UnmatchedCaseException("/home/jan/svn/hats/CaseStudies/models/fredhopper/replication/abs/lang/abslang.abs:12:24:  value "+__ABS_value+" did not match any pattern.");
}}.of(a, a);
   }
}
final class max__Function implements ABSFunction {
   private max__Function() { }
   public static abs.backend.java.lib.types.ABSInteger apply(abs.backend.java.lib.types.ABSInteger a, abs.backend.java.lib.types.ABSInteger b) {
      return new Case() {
      public abs.backend.java.lib.types.ABSInteger of(final abs.backend.java.lib.types.ABSInteger b, final abs.backend.java.lib.types.ABSInteger a, final abs.backend.java.lib.types.ABSBool __ABS_value) { 
final abs.backend.java.lib.expr.PatternBinding __ABS_binding0 = new abs.backend.java.lib.expr.PatternConstructor("True").match(__ABS_value);
if (__ABS_binding0 != null) return new Object() {
  public abs.backend.java.lib.types.ABSInteger execute() { return a; }}.execute();
final abs.backend.java.lib.expr.PatternBinding __ABS_binding1 = new abs.backend.java.lib.expr.PatternConstructor("False").match(__ABS_value);
if (__ABS_binding1 != null) return new Object() {
  public abs.backend.java.lib.types.ABSInteger execute() { return b; }}.execute();
throw new abs.backend.java.lib.expr.UnmatchedCaseException("/home/jan/svn/hats/CaseStudies/models/fredhopper/replication/abs/lang/abslang.abs:14:29:  value "+__ABS_value+" did not match any pattern.");
}}.of(b, a, a.gt(b));
   }
}
final class abs__Function implements ABSFunction {
   private abs__Function() { }
   public static abs.backend.java.lib.types.ABSInteger apply(abs.backend.java.lib.types.ABSInteger x) {
      return new Case() {
      public abs.backend.java.lib.types.ABSInteger of(final abs.backend.java.lib.types.ABSInteger x, final abs.backend.java.lib.types.ABSBool __ABS_value) { 
final abs.backend.java.lib.expr.PatternBinding __ABS_binding0 = new abs.backend.java.lib.expr.PatternConstructor("True").match(__ABS_value);
if (__ABS_binding0 != null) return new Object() {
  public abs.backend.java.lib.types.ABSInteger execute() { return x; }}.execute();
final abs.backend.java.lib.expr.PatternBinding __ABS_binding1 = new abs.backend.java.lib.expr.PatternConstructor("False").match(__ABS_value);
if (__ABS_binding1 != null) return new Object() {
  public abs.backend.java.lib.types.ABSInteger execute() { return x.negate(); }}.execute();
throw new abs.backend.java.lib.expr.UnmatchedCaseException("/home/jan/svn/hats/CaseStudies/models/fredhopper/replication/abs/lang/abslang.abs:15:22:  value "+__ABS_value+" did not match any pattern.");
}}.of(x, x.gt(abs.backend.java.lib.types.ABSInteger.fromString("0")));
   }
}
abstract class Maybe<A extends abs.backend.java.lib.types.ABSValue> extends ABSDataType {
}
class Nothing__Constructor<A extends abs.backend.java.lib.types.ABSValue> extends Maybe<A> {
   public Nothing__Constructor() {
   }
   protected ABSValue[] getArgs() { return new ABSValue[] {};}
   public java.lang.String getConstructorName() { return "Nothing" ;} 
   public ABSBool eq(ABSValue o) {
      if (! (o instanceof Nothing__Constructor)) return ABSBool.FALSE;
      Nothing__Constructor other = (Nothing__Constructor) o;
      return ABSBool.TRUE;
   }
   public boolean match(abs.backend.java.lib.expr.PatternConstructor c, abs.backend.java.lib.expr.PatternBinding b) {
       if (!c.constructorName.equals("Nothing")) return false;
      return true;
   }
}
class Just__Constructor<A extends abs.backend.java.lib.types.ABSValue> extends Maybe<A> {
   public final A arg0;
   public Just__Constructor(   final A arg0) {
this.arg0 = arg0;
   }
   protected ABSValue[] getArgs() { return new ABSValue[] {this.arg0};}
   public java.lang.String getConstructorName() { return "Just" ;} 
   public ABSBool eq(ABSValue o) {
      if (! (o instanceof Just__Constructor)) return ABSBool.FALSE;
      Just__Constructor other = (Just__Constructor) o;
if (!this.arg0.eq(other.arg0).toBoolean()) return ABSBool.FALSE;
      return ABSBool.TRUE;
   }
   public boolean match(abs.backend.java.lib.expr.PatternConstructor c, abs.backend.java.lib.expr.PatternBinding b) {
       if (!c.constructorName.equals("Just")) return false;
       if (!c.subpattern[0].match(this.arg0,b)) return false;
      return true;
   }
}
final class fromJust__Function implements ABSFunction {
   private fromJust__Function() { }
   public static <A extends abs.backend.java.lib.types.ABSValue>A apply(Maybe a) {
      return new Case() {
      public A of(final Maybe a, final Maybe __ABS_value) { 
final abs.backend.java.lib.expr.PatternBinding __ABS_binding0 = new abs.backend.java.lib.expr.PatternConstructor("Just",new abs.backend.java.lib.expr.PatternVariable("j")).match(__ABS_value);
if (__ABS_binding0 != null) return new Object() {
  public A execute(final A j) { return j; }}.execute((A) __ABS_binding0.getBinding(0));
throw new abs.backend.java.lib.expr.UnmatchedCaseException("/home/jan/svn/hats/CaseStudies/models/fredhopper/replication/abs/lang/abslang.abs:19:33:  value "+__ABS_value+" did not match any pattern.");
}}.of(a, a);
   }
}
final class isJust__Function implements ABSFunction {
   private isJust__Function() { }
   public static <A extends abs.backend.java.lib.types.ABSValue>abs.backend.java.lib.types.ABSBool apply(Maybe a) {
      return new Case() {
      public abs.backend.java.lib.types.ABSBool of(final Maybe a, final Maybe __ABS_value) { 
final abs.backend.java.lib.expr.PatternBinding __ABS_binding0 = new abs.backend.java.lib.expr.PatternConstructor("Just",new abs.backend.java.lib.expr.PatternVariable("j")).match(__ABS_value);
if (__ABS_binding0 != null) return new Object() {
  public abs.backend.java.lib.types.ABSBool execute(final A j) { return abs.backend.java.lib.types.ABSBool.fromBoolean(true); }}.execute((A) __ABS_binding0.getBinding(0));
final abs.backend.java.lib.expr.PatternBinding __ABS_binding1 = new abs.backend.java.lib.expr.PatternConstructor("Nothing").match(__ABS_value);
if (__ABS_binding1 != null) return new Object() {
  public abs.backend.java.lib.types.ABSBool execute() { return abs.backend.java.lib.types.ABSBool.fromBoolean(false); }}.execute();
throw new abs.backend.java.lib.expr.UnmatchedCaseException("/home/jan/svn/hats/CaseStudies/models/fredhopper/replication/abs/lang/abslang.abs:20:34:  value "+__ABS_value+" did not match any pattern.");
}}.of(a, a);
   }
}
abstract class Either<A extends abs.backend.java.lib.types.ABSValue,B extends abs.backend.java.lib.types.ABSValue> extends ABSDataType {
}
class Left__Constructor<A extends abs.backend.java.lib.types.ABSValue,B extends abs.backend.java.lib.types.ABSValue> extends Either<A,B> {
   public final A arg0;
   public Left__Constructor(   final A arg0) {
this.arg0 = arg0;
   }
   protected ABSValue[] getArgs() { return new ABSValue[] {this.arg0};}
   public java.lang.String getConstructorName() { return "Left" ;} 
   public ABSBool eq(ABSValue o) {
      if (! (o instanceof Left__Constructor)) return ABSBool.FALSE;
      Left__Constructor other = (Left__Constructor) o;
if (!this.arg0.eq(other.arg0).toBoolean()) return ABSBool.FALSE;
      return ABSBool.TRUE;
   }
   public boolean match(abs.backend.java.lib.expr.PatternConstructor c, abs.backend.java.lib.expr.PatternBinding b) {
       if (!c.constructorName.equals("Left")) return false;
       if (!c.subpattern[0].match(this.arg0,b)) return false;
      return true;
   }
}
class Right__Constructor<A extends abs.backend.java.lib.types.ABSValue,B extends abs.backend.java.lib.types.ABSValue> extends Either<A,B> {
   public final B arg0;
   public Right__Constructor(   final B arg0) {
this.arg0 = arg0;
   }
   protected ABSValue[] getArgs() { return new ABSValue[] {this.arg0};}
   public java.lang.String getConstructorName() { return "Right" ;} 
   public ABSBool eq(ABSValue o) {
      if (! (o instanceof Right__Constructor)) return ABSBool.FALSE;
      Right__Constructor other = (Right__Constructor) o;
if (!this.arg0.eq(other.arg0).toBoolean()) return ABSBool.FALSE;
      return ABSBool.TRUE;
   }
   public boolean match(abs.backend.java.lib.expr.PatternConstructor c, abs.backend.java.lib.expr.PatternBinding b) {
       if (!c.constructorName.equals("Right")) return false;
       if (!c.subpattern[0].match(this.arg0,b)) return false;
      return true;
   }
}
final class left__Function implements ABSFunction {
   private left__Function() { }
   public static <A extends abs.backend.java.lib.types.ABSValue,B extends abs.backend.java.lib.types.ABSValue>A apply(Either val) {
      return new Case() {
      public A of(final Either val, final Either __ABS_value) { 
final abs.backend.java.lib.expr.PatternBinding __ABS_binding0 = new abs.backend.java.lib.expr.PatternConstructor("Left",new abs.backend.java.lib.expr.PatternVariable("x")).match(__ABS_value);
if (__ABS_binding0 != null) return new Object() {
  public A execute(final A x) { return x; }}.execute((A) __ABS_binding0.getBinding(0));
throw new abs.backend.java.lib.expr.UnmatchedCaseException("/home/jan/svn/hats/CaseStudies/models/fredhopper/replication/abs/lang/abslang.abs:24:37:  value "+__ABS_value+" did not match any pattern.");
}}.of(val, val);
   }
}
final class right__Function implements ABSFunction {
   private right__Function() { }
   public static <A extends abs.backend.java.lib.types.ABSValue,B extends abs.backend.java.lib.types.ABSValue>B apply(Either val) {
      return new Case() {
      public B of(final Either val, final Either __ABS_value) { 
final abs.backend.java.lib.expr.PatternBinding __ABS_binding0 = new abs.backend.java.lib.expr.PatternConstructor("Right",new abs.backend.java.lib.expr.PatternVariable("x")).match(__ABS_value);
if (__ABS_binding0 != null) return new Object() {
  public B execute(final B x) { return x; }}.execute((B) __ABS_binding0.getBinding(0));
throw new abs.backend.java.lib.expr.UnmatchedCaseException("/home/jan/svn/hats/CaseStudies/models/fredhopper/replication/abs/lang/abslang.abs:25:38:  value "+__ABS_value+" did not match any pattern.");
}}.of(val, val);
   }
}
final class isLeft__Function implements ABSFunction {
   private isLeft__Function() { }
   public static <A extends abs.backend.java.lib.types.ABSValue,B extends abs.backend.java.lib.types.ABSValue>abs.backend.java.lib.types.ABSBool apply(Either val) {
      return new Case() {
      public abs.backend.java.lib.types.ABSBool of(final Either val, final Either __ABS_value) { 
final abs.backend.java.lib.expr.PatternBinding __ABS_binding0 = new abs.backend.java.lib.expr.PatternConstructor("Left",new abs.backend.java.lib.expr.PatternVariable("x")).match(__ABS_value);
if (__ABS_binding0 != null) return new Object() {
  public abs.backend.java.lib.types.ABSBool execute(final A x) { return abs.backend.java.lib.types.ABSBool.fromBoolean(true); }}.execute((A) __ABS_binding0.getBinding(0));
final abs.backend.java.lib.expr.PatternBinding __ABS_binding1 = new abs.backend.java.lib.expr.AnyPattern().match(__ABS_value);
if (__ABS_binding1 != null) return new Object() {
  public abs.backend.java.lib.types.ABSBool execute() { return abs.backend.java.lib.types.ABSBool.fromBoolean(false); }}.execute();
throw new abs.backend.java.lib.expr.UnmatchedCaseException("/home/jan/svn/hats/CaseStudies/models/fredhopper/replication/abs/lang/abslang.abs:26:42:  value "+__ABS_value+" did not match any pattern.");
}}.of(val, val);
   }
}
final class isRight__Function implements ABSFunction {
   private isRight__Function() { }
   public static <A extends abs.backend.java.lib.types.ABSValue,B extends abs.backend.java.lib.types.ABSValue>abs.backend.java.lib.types.ABSBool apply(Either val) {
      return isLeft__Function.<A,B>apply(val).negate();
   }
}
abstract class Pair<A extends abs.backend.java.lib.types.ABSValue,B extends abs.backend.java.lib.types.ABSValue> extends ABSDataType {
}
class Pair__Constructor<A extends abs.backend.java.lib.types.ABSValue,B extends abs.backend.java.lib.types.ABSValue> extends Pair<A,B> {
   public final A arg0;
   public final B arg1;
   public Pair__Constructor(   final A arg0,   final B arg1) {
this.arg0 = arg0;
this.arg1 = arg1;
   }
   protected ABSValue[] getArgs() { return new ABSValue[] {this.arg0,this.arg1};}
   public java.lang.String getConstructorName() { return "Pair" ;} 
   public ABSBool eq(ABSValue o) {
      if (! (o instanceof Pair__Constructor)) return ABSBool.FALSE;
      Pair__Constructor other = (Pair__Constructor) o;
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
final class fst__Function implements ABSFunction {
   private fst__Function() { }
   public static <A extends abs.backend.java.lib.types.ABSValue,B extends abs.backend.java.lib.types.ABSValue>A apply(Pair p) {
      return new Case() {
      public A of(final Pair p, final Pair __ABS_value) { 
final abs.backend.java.lib.expr.PatternBinding __ABS_binding0 = new abs.backend.java.lib.expr.PatternConstructor("Pair",new abs.backend.java.lib.expr.PatternVariable("s"),new abs.backend.java.lib.expr.PatternVariable("f")).match(__ABS_value);
if (__ABS_binding0 != null) return new Object() {
  public A execute(final A s,final B f) { return s; }}.execute((A) __ABS_binding0.getBinding(0),(B) __ABS_binding0.getBinding(1));
throw new abs.backend.java.lib.expr.UnmatchedCaseException("/home/jan/svn/hats/CaseStudies/models/fredhopper/replication/abs/lang/abslang.abs:31:33:  value "+__ABS_value+" did not match any pattern.");
}}.of(p, p);
   }
}
final class snd__Function implements ABSFunction {
   private snd__Function() { }
   public static <A extends abs.backend.java.lib.types.ABSValue,B extends abs.backend.java.lib.types.ABSValue>B apply(Pair p) {
      return new Case() {
      public B of(final Pair p, final Pair __ABS_value) { 
final abs.backend.java.lib.expr.PatternBinding __ABS_binding0 = new abs.backend.java.lib.expr.PatternConstructor("Pair",new abs.backend.java.lib.expr.PatternVariable("s"),new abs.backend.java.lib.expr.PatternVariable("f")).match(__ABS_value);
if (__ABS_binding0 != null) return new Object() {
  public B execute(final A s,final B f) { return f; }}.execute((A) __ABS_binding0.getBinding(0),(B) __ABS_binding0.getBinding(1));
throw new abs.backend.java.lib.expr.UnmatchedCaseException("/home/jan/svn/hats/CaseStudies/models/fredhopper/replication/abs/lang/abslang.abs:32:33:  value "+__ABS_value+" did not match any pattern.");
}}.of(p, p);
   }
}
abstract class Triple<A extends abs.backend.java.lib.types.ABSValue,B extends abs.backend.java.lib.types.ABSValue,C extends abs.backend.java.lib.types.ABSValue> extends ABSDataType {
}
class Triple__Constructor<A extends abs.backend.java.lib.types.ABSValue,B extends abs.backend.java.lib.types.ABSValue,C extends abs.backend.java.lib.types.ABSValue> extends Triple<A,B,C> {
   public final A arg0;
   public final B arg1;
   public final C arg2;
   public Triple__Constructor(   final A arg0,   final B arg1,   final C arg2) {
this.arg0 = arg0;
this.arg1 = arg1;
this.arg2 = arg2;
   }
   protected ABSValue[] getArgs() { return new ABSValue[] {this.arg0,this.arg1,this.arg2};}
   public java.lang.String getConstructorName() { return "Triple" ;} 
   public ABSBool eq(ABSValue o) {
      if (! (o instanceof Triple__Constructor)) return ABSBool.FALSE;
      Triple__Constructor other = (Triple__Constructor) o;
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
final class fstT__Function implements ABSFunction {
   private fstT__Function() { }
   public static <A extends abs.backend.java.lib.types.ABSValue,B extends abs.backend.java.lib.types.ABSValue,C extends abs.backend.java.lib.types.ABSValue>A apply(Triple p) {
      return new Case() {
      public A of(final Triple p, final Triple __ABS_value) { 
final abs.backend.java.lib.expr.PatternBinding __ABS_binding0 = new abs.backend.java.lib.expr.PatternConstructor("Triple",new abs.backend.java.lib.expr.PatternVariable("s"),new abs.backend.java.lib.expr.PatternVariable("f"),new abs.backend.java.lib.expr.PatternVariable("g")).match(__ABS_value);
if (__ABS_binding0 != null) return new Object() {
  public A execute(final A s,final B f,final C g) { return s; }}.execute((A) __ABS_binding0.getBinding(0),(B) __ABS_binding0.getBinding(1),(C) __ABS_binding0.getBinding(2));
throw new abs.backend.java.lib.expr.UnmatchedCaseException("/home/jan/svn/hats/CaseStudies/models/fredhopper/replication/abs/lang/abslang.abs:36:42:  value "+__ABS_value+" did not match any pattern.");
}}.of(p, p);
   }
}
final class sndT__Function implements ABSFunction {
   private sndT__Function() { }
   public static <A extends abs.backend.java.lib.types.ABSValue,B extends abs.backend.java.lib.types.ABSValue,C extends abs.backend.java.lib.types.ABSValue>B apply(Triple p) {
      return new Case() {
      public B of(final Triple p, final Triple __ABS_value) { 
final abs.backend.java.lib.expr.PatternBinding __ABS_binding0 = new abs.backend.java.lib.expr.PatternConstructor("Triple",new abs.backend.java.lib.expr.PatternVariable("s"),new abs.backend.java.lib.expr.PatternVariable("f"),new abs.backend.java.lib.expr.PatternVariable("g")).match(__ABS_value);
if (__ABS_binding0 != null) return new Object() {
  public B execute(final A s,final B f,final C g) { return f; }}.execute((A) __ABS_binding0.getBinding(0),(B) __ABS_binding0.getBinding(1),(C) __ABS_binding0.getBinding(2));
throw new abs.backend.java.lib.expr.UnmatchedCaseException("/home/jan/svn/hats/CaseStudies/models/fredhopper/replication/abs/lang/abslang.abs:37:42:  value "+__ABS_value+" did not match any pattern.");
}}.of(p, p);
   }
}
final class trd__Function implements ABSFunction {
   private trd__Function() { }
   public static <A extends abs.backend.java.lib.types.ABSValue,B extends abs.backend.java.lib.types.ABSValue,C extends abs.backend.java.lib.types.ABSValue>C apply(Triple p) {
      return new Case() {
      public C of(final Triple p, final Triple __ABS_value) { 
final abs.backend.java.lib.expr.PatternBinding __ABS_binding0 = new abs.backend.java.lib.expr.PatternConstructor("Triple",new abs.backend.java.lib.expr.PatternVariable("s"),new abs.backend.java.lib.expr.PatternVariable("f"),new abs.backend.java.lib.expr.PatternVariable("g")).match(__ABS_value);
if (__ABS_binding0 != null) return new Object() {
  public C execute(final A s,final B f,final C g) { return g; }}.execute((A) __ABS_binding0.getBinding(0),(B) __ABS_binding0.getBinding(1),(C) __ABS_binding0.getBinding(2));
throw new abs.backend.java.lib.expr.UnmatchedCaseException("/home/jan/svn/hats/CaseStudies/models/fredhopper/replication/abs/lang/abslang.abs:38:41:  value "+__ABS_value+" did not match any pattern.");
}}.of(p, p);
   }
}
abstract class Set<A extends abs.backend.java.lib.types.ABSValue> extends ABSDataType {
}
class EmptySet__Constructor<A extends abs.backend.java.lib.types.ABSValue> extends Set<A> {
   public EmptySet__Constructor() {
   }
   protected ABSValue[] getArgs() { return new ABSValue[] {};}
   public java.lang.String getConstructorName() { return "EmptySet" ;} 
   public ABSBool eq(ABSValue o) {
      if (! (o instanceof EmptySet__Constructor)) return ABSBool.FALSE;
      EmptySet__Constructor other = (EmptySet__Constructor) o;
      return ABSBool.TRUE;
   }
   public boolean match(abs.backend.java.lib.expr.PatternConstructor c, abs.backend.java.lib.expr.PatternBinding b) {
       if (!c.constructorName.equals("EmptySet")) return false;
      return true;
   }
}
class Insert__Constructor<A extends abs.backend.java.lib.types.ABSValue> extends Set<A> {
   public final A arg0;
   public final Set arg1;
   public Insert__Constructor(   final A arg0,   final Set arg1) {
this.arg0 = arg0;
this.arg1 = arg1;
   }
   protected ABSValue[] getArgs() { return new ABSValue[] {this.arg0,this.arg1};}
   public java.lang.String getConstructorName() { return "Insert" ;} 
   public ABSBool eq(ABSValue o) {
      if (! (o instanceof Insert__Constructor)) return ABSBool.FALSE;
      Insert__Constructor other = (Insert__Constructor) o;
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
final class set__Function implements ABSFunction {
   private set__Function() { }
   public static <A extends abs.backend.java.lib.types.ABSValue>Set apply(List l) {
      return new Case() {
      public Set of(final List l, final List __ABS_value) { 
final abs.backend.java.lib.expr.PatternBinding __ABS_binding0 = new abs.backend.java.lib.expr.PatternConstructor("Nil").match(__ABS_value);
if (__ABS_binding0 != null) return new Object() {
  public Set execute() { return new EmptySet__Constructor(); }}.execute();
final abs.backend.java.lib.expr.PatternBinding __ABS_binding1 = new abs.backend.java.lib.expr.PatternConstructor("Cons",new abs.backend.java.lib.expr.PatternVariable("x"),new abs.backend.java.lib.expr.PatternVariable("xs")).match(__ABS_value);
if (__ABS_binding1 != null) return new Object() {
  public Set execute(final A x,final List xs) { return new Insert__Constructor(x, set__Function.<A>apply(xs)); }}.execute((A) __ABS_binding1.getBinding(0),(List) __ABS_binding1.getBinding(1));
throw new abs.backend.java.lib.expr.UnmatchedCaseException("/home/jan/svn/hats/CaseStudies/models/fredhopper/replication/abs/lang/abslang.abs:45:32:  value "+__ABS_value+" did not match any pattern.");
}}.of(l, l);
   }
}
final class contains__Function implements ABSFunction {
   private contains__Function() { }
   public static <A extends abs.backend.java.lib.types.ABSValue>abs.backend.java.lib.types.ABSBool apply(Set ss, A e) {
      return new Case() {
      public abs.backend.java.lib.types.ABSBool of(final A e, final Set ss, final Set __ABS_value) { 
final abs.backend.java.lib.expr.PatternBinding __ABS_binding0 = new abs.backend.java.lib.expr.PatternConstructor("EmptySet").match(__ABS_value);
if (__ABS_binding0 != null) return new Object() {
  public abs.backend.java.lib.types.ABSBool execute() { return abs.backend.java.lib.types.ABSBool.fromBoolean(false); }}.execute();
final abs.backend.java.lib.expr.PatternBinding __ABS_binding1 = new abs.backend.java.lib.expr.PatternConstructor("Insert",new abs.backend.java.lib.expr.PatternValue(e),new abs.backend.java.lib.expr.AnyPattern()).match(__ABS_value);
if (__ABS_binding1 != null) return new Object() {
  public abs.backend.java.lib.types.ABSBool execute() { return abs.backend.java.lib.types.ABSBool.fromBoolean(true); }}.execute();
final abs.backend.java.lib.expr.PatternBinding __ABS_binding2 = new abs.backend.java.lib.expr.PatternConstructor("Insert",new abs.backend.java.lib.expr.AnyPattern(),new abs.backend.java.lib.expr.PatternVariable("xs")).match(__ABS_value);
if (__ABS_binding2 != null) return new Object() {
  public abs.backend.java.lib.types.ABSBool execute(final Set xs) { return contains__Function.<A>apply(xs, e); }}.execute((Set) __ABS_binding2.getBinding(0));
throw new abs.backend.java.lib.expr.UnmatchedCaseException("/home/jan/svn/hats/CaseStudies/models/fredhopper/replication/abs/lang/abslang.abs:51:3:  value "+__ABS_value+" did not match any pattern.");
}}.of(e, ss, ss);
   }
}
final class emptySet__Function implements ABSFunction {
   private emptySet__Function() { }
   public static <A extends abs.backend.java.lib.types.ABSValue>abs.backend.java.lib.types.ABSBool apply(Set xs) {
      return xs.eq(new EmptySet__Constructor());
   }
}
final class size__Function implements ABSFunction {
   private size__Function() { }
   public static <A extends abs.backend.java.lib.types.ABSValue>abs.backend.java.lib.types.ABSInteger apply(Set xs) {
      return new Case() {
      public abs.backend.java.lib.types.ABSInteger of(final Set xs, final Set __ABS_value) { 
final abs.backend.java.lib.expr.PatternBinding __ABS_binding0 = new abs.backend.java.lib.expr.PatternConstructor("EmptySet").match(__ABS_value);
if (__ABS_binding0 != null) return new Object() {
  public abs.backend.java.lib.types.ABSInteger execute() { return abs.backend.java.lib.types.ABSInteger.fromString("0"); }}.execute();
final abs.backend.java.lib.expr.PatternBinding __ABS_binding1 = new abs.backend.java.lib.expr.PatternConstructor("Insert",new abs.backend.java.lib.expr.PatternVariable("s"),new abs.backend.java.lib.expr.PatternVariable("ss")).match(__ABS_value);
if (__ABS_binding1 != null) return new Object() {
  public abs.backend.java.lib.types.ABSInteger execute(final A s,final Set ss) { return abs.backend.java.lib.types.ABSInteger.fromString("1").add(size__Function.<A>apply(ss)); }}.execute((A) __ABS_binding1.getBinding(0),(Set) __ABS_binding1.getBinding(1));
throw new abs.backend.java.lib.expr.UnmatchedCaseException("/home/jan/svn/hats/CaseStudies/models/fredhopper/replication/abs/lang/abslang.abs:65:30:  value "+__ABS_value+" did not match any pattern.");
}}.of(xs, xs);
   }
}
final class insertElement__Function implements ABSFunction {
   private insertElement__Function() { }
   public static <A extends abs.backend.java.lib.types.ABSValue>Set apply(Set xs, A e) {
      return new Case() {
      public Set of(final A e, final Set xs, final abs.backend.java.lib.types.ABSBool __ABS_value) { 
final abs.backend.java.lib.expr.PatternBinding __ABS_binding0 = new abs.backend.java.lib.expr.PatternConstructor("True").match(__ABS_value);
if (__ABS_binding0 != null) return new Object() {
  public Set execute() { return xs; }}.execute();
final abs.backend.java.lib.expr.PatternBinding __ABS_binding1 = new abs.backend.java.lib.expr.PatternConstructor("False").match(__ABS_value);
if (__ABS_binding1 != null) return new Object() {
  public Set execute() { return new Insert__Constructor(e, xs); }}.execute();
throw new abs.backend.java.lib.expr.UnmatchedCaseException("/home/jan/svn/hats/CaseStudies/models/fredhopper/replication/abs/lang/abslang.abs:72:3:  value "+__ABS_value+" did not match any pattern.");
}}.of(e, xs, contains__Function.<A>apply(xs, e));
   }
}
final class remove__Function implements ABSFunction {
   private remove__Function() { }
   public static <A extends abs.backend.java.lib.types.ABSValue>Set apply(Set xs, A e) {
      return new Case() {
      public Set of(final A e, final Set xs, final Set __ABS_value) { 
final abs.backend.java.lib.expr.PatternBinding __ABS_binding0 = new abs.backend.java.lib.expr.PatternConstructor("EmptySet").match(__ABS_value);
if (__ABS_binding0 != null) return new Object() {
  public Set execute() { return new EmptySet__Constructor(); }}.execute();
final abs.backend.java.lib.expr.PatternBinding __ABS_binding1 = new abs.backend.java.lib.expr.PatternConstructor("Insert",new abs.backend.java.lib.expr.PatternValue(e),new abs.backend.java.lib.expr.PatternVariable("ss")).match(__ABS_value);
if (__ABS_binding1 != null) return new Object() {
  public Set execute(final Set ss) { return ss; }}.execute((Set) __ABS_binding1.getBinding(0));
final abs.backend.java.lib.expr.PatternBinding __ABS_binding2 = new abs.backend.java.lib.expr.PatternConstructor("Insert",new abs.backend.java.lib.expr.PatternVariable("s"),new abs.backend.java.lib.expr.PatternVariable("ss")).match(__ABS_value);
if (__ABS_binding2 != null) return new Object() {
  public Set execute(final A s,final Set ss) { return new Insert__Constructor(s, remove__Function.<A>apply(ss, e)); }}.execute((A) __ABS_binding2.getBinding(0),(Set) __ABS_binding2.getBinding(1));
throw new abs.backend.java.lib.expr.UnmatchedCaseException("/home/jan/svn/hats/CaseStudies/models/fredhopper/replication/abs/lang/abslang.abs:81:3:  value "+__ABS_value+" did not match any pattern.");
}}.of(e, xs, xs);
   }
}
final class hasNext__Function implements ABSFunction {
   private hasNext__Function() { }
   public static <A extends abs.backend.java.lib.types.ABSValue>abs.backend.java.lib.types.ABSBool apply(Set s) {
      return emptySet__Function.<A>apply(s).negate();
   }
}
final class next__Function implements ABSFunction {
   private next__Function() { }
   public static <A extends abs.backend.java.lib.types.ABSValue>Pair apply(Set s) {
      return new Case() {
      public Pair of(final Set s, final Set __ABS_value) { 
final abs.backend.java.lib.expr.PatternBinding __ABS_binding0 = new abs.backend.java.lib.expr.PatternConstructor("Insert",new abs.backend.java.lib.expr.PatternVariable("e"),new abs.backend.java.lib.expr.PatternVariable("set2")).match(__ABS_value);
if (__ABS_binding0 != null) return new Object() {
  public Pair execute(final A e,final Set set2) { return new Pair__Constructor(set2, e); }}.execute((A) __ABS_binding0.getBinding(0),(Set) __ABS_binding0.getBinding(1));
throw new abs.backend.java.lib.expr.UnmatchedCaseException("/home/jan/svn/hats/CaseStudies/models/fredhopper/replication/abs/lang/abslang.abs:91:41:  value "+__ABS_value+" did not match any pattern.");
}}.of(s, s);
   }
}
abstract class List<A extends abs.backend.java.lib.types.ABSValue> extends ABSDataType {
}
class Nil__Constructor<A extends abs.backend.java.lib.types.ABSValue> extends List<A> {
   public Nil__Constructor() {
   }
   protected ABSValue[] getArgs() { return new ABSValue[] {};}
   public java.lang.String getConstructorName() { return "Nil" ;} 
   public ABSBool eq(ABSValue o) {
      if (! (o instanceof Nil__Constructor)) return ABSBool.FALSE;
      Nil__Constructor other = (Nil__Constructor) o;
      return ABSBool.TRUE;
   }
   public boolean match(abs.backend.java.lib.expr.PatternConstructor c, abs.backend.java.lib.expr.PatternBinding b) {
       if (!c.constructorName.equals("Nil")) return false;
      return true;
   }
}
class Cons__Constructor<A extends abs.backend.java.lib.types.ABSValue> extends List<A> {
   public final A arg0;
   public final List arg1;
   public Cons__Constructor(   final A arg0,   final List arg1) {
this.arg0 = arg0;
this.arg1 = arg1;
   }
   protected ABSValue[] getArgs() { return new ABSValue[] {this.arg0,this.arg1};}
   public java.lang.String getConstructorName() { return "Cons" ;} 
   public ABSBool eq(ABSValue o) {
      if (! (o instanceof Cons__Constructor)) return ABSBool.FALSE;
      Cons__Constructor other = (Cons__Constructor) o;
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
final class list__Function implements ABSFunction {
   private list__Function() { }
   public static <A extends abs.backend.java.lib.types.ABSValue>List apply(List l) {
      return l;
   }
}
final class length__Function implements ABSFunction {
   private length__Function() { }
   public static <A extends abs.backend.java.lib.types.ABSValue>abs.backend.java.lib.types.ABSInteger apply(List list) {
      return new Case() {
      public abs.backend.java.lib.types.ABSInteger of(final List list, final List __ABS_value) { 
final abs.backend.java.lib.expr.PatternBinding __ABS_binding0 = new abs.backend.java.lib.expr.PatternConstructor("Nil").match(__ABS_value);
if (__ABS_binding0 != null) return new Object() {
  public abs.backend.java.lib.types.ABSInteger execute() { return abs.backend.java.lib.types.ABSInteger.fromString("0"); }}.execute();
final abs.backend.java.lib.expr.PatternBinding __ABS_binding1 = new abs.backend.java.lib.expr.PatternConstructor("Cons",new abs.backend.java.lib.expr.PatternVariable("p"),new abs.backend.java.lib.expr.PatternVariable("l")).match(__ABS_value);
if (__ABS_binding1 != null) return new Object() {
  public abs.backend.java.lib.types.ABSInteger execute(final A p,final List l) { return abs.backend.java.lib.types.ABSInteger.fromString("1").add(length__Function.<A>apply(l)); }}.execute((A) __ABS_binding1.getBinding(0),(List) __ABS_binding1.getBinding(1));
throw new abs.backend.java.lib.expr.UnmatchedCaseException("/home/jan/svn/hats/CaseStudies/models/fredhopper/replication/abs/lang/abslang.abs:101:35:  value "+__ABS_value+" did not match any pattern.");
}}.of(list, list);
   }
}
final class isEmpty__Function implements ABSFunction {
   private isEmpty__Function() { }
   public static <A extends abs.backend.java.lib.types.ABSValue>abs.backend.java.lib.types.ABSBool apply(List list) {
      return list.eq(new Nil__Constructor());
   }
}
final class head__Function implements ABSFunction {
   private head__Function() { }
   public static <A extends abs.backend.java.lib.types.ABSValue>A apply(List list) {
      return new Case() {
      public A of(final List list, final List __ABS_value) { 
final abs.backend.java.lib.expr.PatternBinding __ABS_binding0 = new abs.backend.java.lib.expr.PatternConstructor("Cons",new abs.backend.java.lib.expr.PatternVariable("p"),new abs.backend.java.lib.expr.PatternVariable("l")).match(__ABS_value);
if (__ABS_binding0 != null) return new Object() {
  public A execute(final A p,final List l) { return p; }}.execute((A) __ABS_binding0.getBinding(0),(List) __ABS_binding0.getBinding(1));
throw new abs.backend.java.lib.expr.UnmatchedCaseException("/home/jan/svn/hats/CaseStudies/models/fredhopper/replication/abs/lang/abslang.abs:109:31:  value "+__ABS_value+" did not match any pattern.");
}}.of(list, list);
   }
}
final class tail__Function implements ABSFunction {
   private tail__Function() { }
   public static <A extends abs.backend.java.lib.types.ABSValue>List apply(List list) {
      return new Case() {
      public List of(final List list, final List __ABS_value) { 
final abs.backend.java.lib.expr.PatternBinding __ABS_binding0 = new abs.backend.java.lib.expr.PatternConstructor("Cons",new abs.backend.java.lib.expr.PatternVariable("p"),new abs.backend.java.lib.expr.PatternVariable("l")).match(__ABS_value);
if (__ABS_binding0 != null) return new Object() {
  public List execute(final A p,final List l) { return l; }}.execute((A) __ABS_binding0.getBinding(0),(List) __ABS_binding0.getBinding(1));
throw new abs.backend.java.lib.expr.UnmatchedCaseException("/home/jan/svn/hats/CaseStudies/models/fredhopper/replication/abs/lang/abslang.abs:114:37:  value "+__ABS_value+" did not match any pattern.");
}}.of(list, list);
   }
}
final class nth__Function implements ABSFunction {
   private nth__Function() { }
   public static <A extends abs.backend.java.lib.types.ABSValue>A apply(List list, abs.backend.java.lib.types.ABSInteger n) {
      return new Case() {
      public A of(final abs.backend.java.lib.types.ABSInteger n, final List list, final abs.backend.java.lib.types.ABSInteger __ABS_value) { 
final abs.backend.java.lib.expr.PatternBinding __ABS_binding0 = new abs.backend.java.lib.expr.PatternValue(abs.backend.java.lib.types.ABSInteger.fromString("0")).match(__ABS_value);
if (__ABS_binding0 != null) return new Object() {
  public A execute() { return head__Function.<A>apply(list); }}.execute();
final abs.backend.java.lib.expr.PatternBinding __ABS_binding1 = new abs.backend.java.lib.expr.AnyPattern().match(__ABS_value);
if (__ABS_binding1 != null) return new Object() {
  public A execute() { return nth__Function.<A>apply(tail__Function.<A>apply(list), n.subtract(abs.backend.java.lib.types.ABSInteger.fromString("1"))); }}.execute();
throw new abs.backend.java.lib.expr.UnmatchedCaseException("/home/jan/svn/hats/CaseStudies/models/fredhopper/replication/abs/lang/abslang.abs:119:3:  value "+__ABS_value+" did not match any pattern.");
}}.of(n, list, n);
   }
}
final class concatenate__Function implements ABSFunction {
   private concatenate__Function() { }
   public static <A extends abs.backend.java.lib.types.ABSValue>List apply(List list1, List list2) {
      return new Case() {
      public List of(final List list2, final List list1, final List __ABS_value) { 
final abs.backend.java.lib.expr.PatternBinding __ABS_binding0 = new abs.backend.java.lib.expr.PatternConstructor("Nil").match(__ABS_value);
if (__ABS_binding0 != null) return new Object() {
  public List execute() { return list2; }}.execute();
final abs.backend.java.lib.expr.PatternBinding __ABS_binding1 = new abs.backend.java.lib.expr.PatternConstructor("Cons",new abs.backend.java.lib.expr.PatternVariable("head"),new abs.backend.java.lib.expr.PatternVariable("tail")).match(__ABS_value);
if (__ABS_binding1 != null) return new Object() {
  public List execute(final A head,final List tail) { return new Cons__Constructor(head, concatenate__Function.<A>apply(tail, list2)); }}.execute((A) __ABS_binding1.getBinding(0),(List) __ABS_binding1.getBinding(1));
throw new abs.backend.java.lib.expr.UnmatchedCaseException("/home/jan/svn/hats/CaseStudies/models/fredhopper/replication/abs/lang/abslang.abs:125:3:  value "+__ABS_value+" did not match any pattern.");
}}.of(list2, list1, list1);
   }
}
final class appendright__Function implements ABSFunction {
   private appendright__Function() { }
   public static <A extends abs.backend.java.lib.types.ABSValue>List apply(List list, A p) {
      return concatenate__Function.<A>apply(list, new Cons__Constructor(p, new Nil__Constructor()));
   }
}
final class reverse__Function implements ABSFunction {
   private reverse__Function() { }
   public static <A extends abs.backend.java.lib.types.ABSValue>List apply(List list) {
      return new Case() {
      public List of(final List list, final List __ABS_value) { 
final abs.backend.java.lib.expr.PatternBinding __ABS_binding0 = new abs.backend.java.lib.expr.PatternConstructor("Cons",new abs.backend.java.lib.expr.PatternVariable("hd"),new abs.backend.java.lib.expr.PatternVariable("tl")).match(__ABS_value);
if (__ABS_binding0 != null) return new Object() {
  public List execute(final A hd,final List tl) { return appendright__Function.<A>apply(reverse__Function.<A>apply(tl), hd); }}.execute((A) __ABS_binding0.getBinding(0),(List) __ABS_binding0.getBinding(1));
final abs.backend.java.lib.expr.PatternBinding __ABS_binding1 = new abs.backend.java.lib.expr.PatternConstructor("Nil").match(__ABS_value);
if (__ABS_binding1 != null) return new Object() {
  public List execute() { return new Nil__Constructor(); }}.execute();
throw new abs.backend.java.lib.expr.UnmatchedCaseException("/home/jan/svn/hats/CaseStudies/models/fredhopper/replication/abs/lang/abslang.abs:134:3:  value "+__ABS_value+" did not match any pattern.");
}}.of(list, list);
   }
}
final class copy__Function implements ABSFunction {
   private copy__Function() { }
   public static <A extends abs.backend.java.lib.types.ABSValue>List apply(A p, abs.backend.java.lib.types.ABSInteger n) {
      return new Case() {
      public List of(final A p, final abs.backend.java.lib.types.ABSInteger n, final abs.backend.java.lib.types.ABSInteger __ABS_value) { 
final abs.backend.java.lib.expr.PatternBinding __ABS_binding0 = new abs.backend.java.lib.expr.PatternValue(abs.backend.java.lib.types.ABSInteger.fromString("0")).match(__ABS_value);
if (__ABS_binding0 != null) return new Object() {
  public List execute() { return new Nil__Constructor(); }}.execute();
final abs.backend.java.lib.expr.PatternBinding __ABS_binding1 = new abs.backend.java.lib.expr.PatternVariable("m").match(__ABS_value);
if (__ABS_binding1 != null) return new Object() {
  public List execute(final abs.backend.java.lib.types.ABSInteger m) { return new Cons__Constructor(p, copy__Function.<A>apply(p, m.subtract(abs.backend.java.lib.types.ABSInteger.fromString("1")))); }}.execute((abs.backend.java.lib.types.ABSInteger) __ABS_binding1.getBinding(0));
throw new abs.backend.java.lib.expr.UnmatchedCaseException("/home/jan/svn/hats/CaseStudies/models/fredhopper/replication/abs/lang/abslang.abs:138:35:  value "+__ABS_value+" did not match any pattern.");
}}.of(p, n, n);
   }
}
abstract class Map<A extends abs.backend.java.lib.types.ABSValue,B extends abs.backend.java.lib.types.ABSValue> extends ABSDataType {
}
class EmptyMap__Constructor<A extends abs.backend.java.lib.types.ABSValue,B extends abs.backend.java.lib.types.ABSValue> extends Map<A,B> {
   public EmptyMap__Constructor() {
   }
   protected ABSValue[] getArgs() { return new ABSValue[] {};}
   public java.lang.String getConstructorName() { return "EmptyMap" ;} 
   public ABSBool eq(ABSValue o) {
      if (! (o instanceof EmptyMap__Constructor)) return ABSBool.FALSE;
      EmptyMap__Constructor other = (EmptyMap__Constructor) o;
      return ABSBool.TRUE;
   }
   public boolean match(abs.backend.java.lib.expr.PatternConstructor c, abs.backend.java.lib.expr.PatternBinding b) {
       if (!c.constructorName.equals("EmptyMap")) return false;
      return true;
   }
}
class InsertAssoc__Constructor<A extends abs.backend.java.lib.types.ABSValue,B extends abs.backend.java.lib.types.ABSValue> extends Map<A,B> {
   public final Pair arg0;
   public final Map arg1;
   public InsertAssoc__Constructor(   final Pair arg0,   final Map arg1) {
this.arg0 = arg0;
this.arg1 = arg1;
   }
   protected ABSValue[] getArgs() { return new ABSValue[] {this.arg0,this.arg1};}
   public java.lang.String getConstructorName() { return "InsertAssoc" ;} 
   public ABSBool eq(ABSValue o) {
      if (! (o instanceof InsertAssoc__Constructor)) return ABSBool.FALSE;
      InsertAssoc__Constructor other = (InsertAssoc__Constructor) o;
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
final class map__Function implements ABSFunction {
   private map__Function() { }
   public static <A extends abs.backend.java.lib.types.ABSValue,B extends abs.backend.java.lib.types.ABSValue>Map apply(List l) {
      return new Case() {
      public Map of(final List l, final List __ABS_value) { 
final abs.backend.java.lib.expr.PatternBinding __ABS_binding0 = new abs.backend.java.lib.expr.PatternConstructor("Nil").match(__ABS_value);
if (__ABS_binding0 != null) return new Object() {
  public Map execute() { return new EmptyMap__Constructor(); }}.execute();
final abs.backend.java.lib.expr.PatternBinding __ABS_binding1 = new abs.backend.java.lib.expr.PatternConstructor("Cons",new abs.backend.java.lib.expr.PatternVariable("hd"),new abs.backend.java.lib.expr.PatternVariable("tl")).match(__ABS_value);
if (__ABS_binding1 != null) return new Object() {
  public Map execute(final Pair hd,final List tl) { return new InsertAssoc__Constructor(hd, map__Function.<A,B>apply(tl)); }}.execute((Pair) __ABS_binding1.getBinding(0),(List) __ABS_binding1.getBinding(1));
throw new abs.backend.java.lib.expr.UnmatchedCaseException("/home/jan/svn/hats/CaseStudies/models/fredhopper/replication/abs/lang/abslang.abs:145:3:  value "+__ABS_value+" did not match any pattern.");
}}.of(l, l);
   }
}
final class keys__Function implements ABSFunction {
   private keys__Function() { }
   public static <A extends abs.backend.java.lib.types.ABSValue,B extends abs.backend.java.lib.types.ABSValue>Set apply(Map map) {
      return new Case() {
      public Set of(final Map map, final Map __ABS_value) { 
final abs.backend.java.lib.expr.PatternBinding __ABS_binding0 = new abs.backend.java.lib.expr.PatternConstructor("EmptyMap").match(__ABS_value);
if (__ABS_binding0 != null) return new Object() {
  public Set execute() { return new EmptySet__Constructor(); }}.execute();
final abs.backend.java.lib.expr.PatternBinding __ABS_binding1 = new abs.backend.java.lib.expr.PatternConstructor("InsertAssoc",new abs.backend.java.lib.expr.PatternConstructor("Pair",new abs.backend.java.lib.expr.PatternVariable("a"),new abs.backend.java.lib.expr.AnyPattern()),new abs.backend.java.lib.expr.PatternVariable("tail")).match(__ABS_value);
if (__ABS_binding1 != null) return new Object() {
  public Set execute(final A a,final Map tail) { return new Insert__Constructor(a, keys__Function.<A,B>apply(tail)); }}.execute((A) __ABS_binding1.getBinding(0),(Map) __ABS_binding1.getBinding(1));
throw new abs.backend.java.lib.expr.UnmatchedCaseException("/home/jan/svn/hats/CaseStudies/models/fredhopper/replication/abs/lang/abslang.abs:150:3:  value "+__ABS_value+" did not match any pattern.");
}}.of(map, map);
   }
}
final class lookup__Function implements ABSFunction {
   private lookup__Function() { }
   public static <A extends abs.backend.java.lib.types.ABSValue,B extends abs.backend.java.lib.types.ABSValue>B apply(Map ms, A k) {
      return new Case() {
      public B of(final Map ms, final A k, final Map __ABS_value) { 
final abs.backend.java.lib.expr.PatternBinding __ABS_binding0 = new abs.backend.java.lib.expr.PatternConstructor("InsertAssoc",new abs.backend.java.lib.expr.PatternConstructor("Pair",new abs.backend.java.lib.expr.PatternValue(k),new abs.backend.java.lib.expr.PatternVariable("y")),new abs.backend.java.lib.expr.AnyPattern()).match(__ABS_value);
if (__ABS_binding0 != null) return new Object() {
  public B execute(final B y) { return y; }}.execute((B) __ABS_binding0.getBinding(0));
final abs.backend.java.lib.expr.PatternBinding __ABS_binding1 = new abs.backend.java.lib.expr.PatternConstructor("InsertAssoc",new abs.backend.java.lib.expr.AnyPattern(),new abs.backend.java.lib.expr.PatternVariable("tm")).match(__ABS_value);
if (__ABS_binding1 != null) return new Object() {
  public B execute(final Map tm) { return lookup__Function.<A,B>apply(tm, k); }}.execute((Map) __ABS_binding1.getBinding(0));
throw new abs.backend.java.lib.expr.UnmatchedCaseException("/home/jan/svn/hats/CaseStudies/models/fredhopper/replication/abs/lang/abslang.abs:155:3:  value "+__ABS_value+" did not match any pattern.");
}}.of(ms, k, ms);
   }
}
final class lookupDefault__Function implements ABSFunction {
   private lookupDefault__Function() { }
   public static <A extends abs.backend.java.lib.types.ABSValue,B extends abs.backend.java.lib.types.ABSValue>B apply(Map ms, A k, B d) {
      return new Case() {
      public B of(final B d, final Map ms, final A k, final Map __ABS_value) { 
final abs.backend.java.lib.expr.PatternBinding __ABS_binding0 = new abs.backend.java.lib.expr.PatternConstructor("InsertAssoc",new abs.backend.java.lib.expr.PatternConstructor("Pair",new abs.backend.java.lib.expr.PatternValue(k),new abs.backend.java.lib.expr.PatternVariable("y")),new abs.backend.java.lib.expr.AnyPattern()).match(__ABS_value);
if (__ABS_binding0 != null) return new Object() {
  public B execute(final B y) { return y; }}.execute((B) __ABS_binding0.getBinding(0));
final abs.backend.java.lib.expr.PatternBinding __ABS_binding1 = new abs.backend.java.lib.expr.PatternConstructor("InsertAssoc",new abs.backend.java.lib.expr.AnyPattern(),new abs.backend.java.lib.expr.PatternVariable("tm")).match(__ABS_value);
if (__ABS_binding1 != null) return new Object() {
  public B execute(final Map tm) { return lookupDefault__Function.<A,B>apply(tm, k, d); }}.execute((Map) __ABS_binding1.getBinding(0));
final abs.backend.java.lib.expr.PatternBinding __ABS_binding2 = new abs.backend.java.lib.expr.PatternConstructor("EmptyMap").match(__ABS_value);
if (__ABS_binding2 != null) return new Object() {
  public B execute() { return d; }}.execute();
throw new abs.backend.java.lib.expr.UnmatchedCaseException("/home/jan/svn/hats/CaseStudies/models/fredhopper/replication/abs/lang/abslang.abs:164:3:  value "+__ABS_value+" did not match any pattern.");
}}.of(d, ms, k, ms);
   }
}
final class insert__Function implements ABSFunction {
   private insert__Function() { }
   public static <A extends abs.backend.java.lib.types.ABSValue,B extends abs.backend.java.lib.types.ABSValue>Map apply(Map map, Pair p) {
      return new InsertAssoc__Constructor(p, map);
   }
}
final class put__Function implements ABSFunction {
   private put__Function() { }
   public static <A extends abs.backend.java.lib.types.ABSValue,B extends abs.backend.java.lib.types.ABSValue>Map apply(Map ms, A k, B v) {
      return new Case() {
      public Map of(final B v, final Map ms, final A k, final Map __ABS_value) { 
final abs.backend.java.lib.expr.PatternBinding __ABS_binding0 = new abs.backend.java.lib.expr.PatternConstructor("EmptyMap").match(__ABS_value);
if (__ABS_binding0 != null) return new Object() {
  public Map execute() { return new InsertAssoc__Constructor(new Pair__Constructor(k, v), new EmptyMap__Constructor()); }}.execute();
final abs.backend.java.lib.expr.PatternBinding __ABS_binding1 = new abs.backend.java.lib.expr.PatternConstructor("InsertAssoc",new abs.backend.java.lib.expr.PatternConstructor("Pair",new abs.backend.java.lib.expr.PatternValue(k),new abs.backend.java.lib.expr.AnyPattern()),new abs.backend.java.lib.expr.PatternVariable("ts")).match(__ABS_value);
if (__ABS_binding1 != null) return new Object() {
  public Map execute(final Map ts) { return new InsertAssoc__Constructor(new Pair__Constructor(k, v), ts); }}.execute((Map) __ABS_binding1.getBinding(0));
final abs.backend.java.lib.expr.PatternBinding __ABS_binding2 = new abs.backend.java.lib.expr.PatternConstructor("InsertAssoc",new abs.backend.java.lib.expr.PatternVariable("p"),new abs.backend.java.lib.expr.PatternVariable("ts")).match(__ABS_value);
if (__ABS_binding2 != null) return new Object() {
  public Map execute(final Pair p,final Map ts) { return new InsertAssoc__Constructor(p, put__Function.<A,B>apply(ts, k, v)); }}.execute((Pair) __ABS_binding2.getBinding(0),(Map) __ABS_binding2.getBinding(1));
throw new abs.backend.java.lib.expr.UnmatchedCaseException("/home/jan/svn/hats/CaseStudies/models/fredhopper/replication/abs/lang/abslang.abs:179:3:  value "+__ABS_value+" did not match any pattern.");
}}.of(v, ms, k, ms);
   }
}
final class intToString__Function implements ABSFunction {
   private intToString__Function() { }
   public static abs.backend.java.lib.types.ABSString apply(abs.backend.java.lib.types.ABSInteger n) {
      return new Case() {
      public abs.backend.java.lib.types.ABSString of(final abs.backend.java.lib.types.ABSInteger n, final abs.backend.java.lib.types.ABSBool __ABS_value) { 
final abs.backend.java.lib.expr.PatternBinding __ABS_binding0 = new abs.backend.java.lib.expr.PatternConstructor("True").match(__ABS_value);
if (__ABS_binding0 != null) return new Object() {
  public abs.backend.java.lib.types.ABSString execute() { return abs.backend.java.lib.types.ABSString.fromString("-").add(intToStringPos__Function.apply(n.negate())); }}.execute();
final abs.backend.java.lib.expr.PatternBinding __ABS_binding1 = new abs.backend.java.lib.expr.PatternConstructor("False").match(__ABS_value);
if (__ABS_binding1 != null) return new Object() {
  public abs.backend.java.lib.types.ABSString execute() { return intToStringPos__Function.apply(n); }}.execute();
throw new abs.backend.java.lib.expr.UnmatchedCaseException("/home/jan/svn/hats/CaseStudies/models/fredhopper/replication/abs/lang/abslang.abs:189:3:  value "+__ABS_value+" did not match any pattern.");
}}.of(n, n.lt(abs.backend.java.lib.types.ABSInteger.fromString("0")));
   }
}
final class intToStringPos__Function implements ABSFunction {
   private intToStringPos__Function() { }
   public static abs.backend.java.lib.types.ABSString apply(abs.backend.java.lib.types.ABSInteger n) {
      return new Let() { public abs.backend.java.lib.types.ABSString in(final abs.backend.java.lib.types.ABSInteger n, final abs.backend.java.lib.types.ABSInteger div) { return new Let() { public abs.backend.java.lib.types.ABSString in(final abs.backend.java.lib.types.ABSInteger div, final abs.backend.java.lib.types.ABSInteger n, final abs.backend.java.lib.types.ABSInteger res) { return new Case() {
      public abs.backend.java.lib.types.ABSString of(final abs.backend.java.lib.types.ABSInteger res, final abs.backend.java.lib.types.ABSInteger div, final abs.backend.java.lib.types.ABSInteger n, final abs.backend.java.lib.types.ABSInteger __ABS_value) { 
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
  public abs.backend.java.lib.types.ABSString execute() { return intToStringPos__Function.apply(div).add(intToStringPos__Function.apply(res)); }}.execute();
throw new abs.backend.java.lib.expr.UnmatchedCaseException("/home/jan/svn/hats/CaseStudies/models/fredhopper/replication/abs/lang/abslang.abs:197:3:  value "+__ABS_value+" did not match any pattern.");
}}.of(res, div, n, n); }}.in(div, n, n.mod(abs.backend.java.lib.types.ABSInteger.fromString("10"))); }}.in(n, n.divide(abs.backend.java.lib.types.ABSInteger.fromString("10")));
   }
}
abstract class Time extends ABSDataType {
}
class Time__Constructor extends Time {
   public final abs.backend.java.lib.types.ABSInteger arg0;
   public Time__Constructor(   final abs.backend.java.lib.types.ABSInteger arg0) {
this.arg0 = arg0;
   }
   protected ABSValue[] getArgs() { return new ABSValue[] {this.arg0};}
   public java.lang.String getConstructorName() { return "Time" ;} 
   public ABSBool eq(ABSValue o) {
      if (! (o instanceof Time__Constructor)) return ABSBool.FALSE;
      Time__Constructor other = (Time__Constructor) o;
if (!this.arg0.eq(other.arg0).toBoolean()) return ABSBool.FALSE;
      return ABSBool.TRUE;
   }
   public boolean match(abs.backend.java.lib.expr.PatternConstructor c, abs.backend.java.lib.expr.PatternBinding b) {
       if (!c.constructorName.equals("Time")) return false;
       if (!c.subpattern[0].match(this.arg0,b)) return false;
      return true;
   }
}
final class now__Function implements ABSFunction {
   private now__Function() { }
   public static Time apply() {
      return new Time__Constructor(abs.backend.java.lib.runtime.ABSBuiltInFunctions.currentms());
   }
}
final class timeval__Function implements ABSFunction {
   private timeval__Function() { }
   public static abs.backend.java.lib.types.ABSInteger apply(Time t) {
      return new Case() {
      public abs.backend.java.lib.types.ABSInteger of(final Time t, final Time __ABS_value) { 
final abs.backend.java.lib.expr.PatternBinding __ABS_binding0 = new abs.backend.java.lib.expr.PatternConstructor("Time",new abs.backend.java.lib.expr.PatternVariable("v")).match(__ABS_value);
if (__ABS_binding0 != null) return new Object() {
  public abs.backend.java.lib.types.ABSInteger execute(final abs.backend.java.lib.types.ABSInteger v) { return v; }}.execute((abs.backend.java.lib.types.ABSInteger) __ABS_binding0.getBinding(0));
throw new abs.backend.java.lib.expr.UnmatchedCaseException("/home/jan/svn/hats/CaseStudies/models/fredhopper/replication/abs/lang/abslang.abs:223:27:  value "+__ABS_value+" did not match any pattern.");
}}.of(t, t);
   }
}
final class timeDifference__Function implements ABSFunction {
   private timeDifference__Function() { }
   public static abs.backend.java.lib.types.ABSInteger apply(Time t1, Time t2) {
      return abs__Function.apply(timeval__Function.apply(t2).subtract(timeval__Function.apply(t1)));
   }
}
interface Tester extends abs.backend.java.lib.types.ABSInterface {
}
// /home/jan/svn/hats/CaseStudies/models/fredhopper/replication/abs/Tests.abs:15:1: 
class TesterImpl extends abs.backend.java.lib.runtime.ABSObject implements abs.backend.java.lib.types.ABSClass, Tester {
   private Node expected;
   private Node actual;
// /home/jan/svn/hats/CaseStudies/models/fredhopper/replication/abs/Tests.abs:17:2: 
   private Set result = new EmptySet__Constructor();
   public TesterImpl(Node expected, Node actual) {
      this.expected = expected;
      this.actual = actual;
       getCOG().objectCreated(this);
   }
   protected ABSValue getFieldValue(java.lang.String __ABS_fieldName) throws java.lang.NoSuchFieldException {
   if ("expected".equals(__ABS_fieldName)) return expected;
   if ("actual".equals(__ABS_fieldName)) return actual;
   if ("result".equals(__ABS_fieldName)) return result;
       return super.getFieldValue(__ABS_fieldName);
   }
   public java.lang.String getClassName() { return "TesterImpl"; }
   public static TesterImpl __ABS_createNewCOG(Node expected, Node actual) {
       final abs.backend.java.lib.runtime.COG __ABS_cog = new abs.backend.java.lib.runtime.COG(TesterImpl.class);
       final abs.backend.java.lib.runtime.ABSThread __ABS_thread = abs.backend.java.lib.runtime.ABSRuntime.getCurrentThread();
       final abs.backend.java.lib.runtime.COG __ABS_oldCOG = abs.backend.java.lib.runtime.ABSRuntime.getCurrentCOG();
       __ABS_thread.setCOG(__ABS_cog);
       try { 
            TesterImpl __ABS_result = new TesterImpl(expected, actual);
          abs.backend.java.lib.runtime.ABSRuntime.cogCreated(__ABS_result);
          return __ABS_result;
       } finally {
           __ABS_thread.setCOG(__ABS_oldCOG);
       }
   }
// /home/jan/svn/hats/CaseStudies/models/fredhopper/replication/abs/Tests.abs:19:2: 
   public abs.backend.java.lib.types.ABSUnit run(){ __ABS_checkSameCOG(); 
 {
      abs.backend.java.lib.types.ABSBool shutdown = abs.backend.java.lib.types.ABSBool.fromBoolean(false);
      while (shutdown.negate().toBoolean()) {
         abs.backend.java.lib.runtime.ABSFut sd = null;
         sd = abs.backend.java.lib.runtime.ABSRuntime.asyncCall(new abs.backend.java.lib.runtime.Task<Node>(this,actual) { protected ABSValue[] getArgs() { return new ABSValue[] {  }; }     public abs.backend.java.lib.runtime.Task<?> init() { return this; } public java.lang.String methodName() { return "isShutdownRequested"; } public Object execute() { return target.isShutdownRequested(); }}
     .init());
         abs.backend.java.lib.runtime.ABSRuntime.await(new abs.backend.java.lib.runtime.ABSFutureGuard(sd));
         shutdown = (abs.backend.java.lib.types.ABSBool)sd.get();
      }
      abs.backend.java.lib.runtime.ABSRuntime.asyncCall(new abs.backend.java.lib.runtime.Task<TesterImpl>(this,this) { protected ABSValue[] getArgs() { return new ABSValue[] {  }; }     public abs.backend.java.lib.runtime.Task<?> init() { return this; } public java.lang.String methodName() { return "assertData"; } public Object execute() { return target.assertData(); }}
     .init());
   return ABSUnit.UNIT;
   }
}// /home/jan/svn/hats/CaseStudies/models/fredhopper/replication/abs/Tests.abs:31:2: 
   public abs.backend.java.lib.types.ABSUnit assertData(){ __ABS_checkSameCOG(); 
 {
      DataBase e = null;
      DataBase a = null;
      Map fse = new EmptyMap__Constructor();
      Set fide = new EmptySet__Constructor();
      Map fsa = new EmptyMap__Constructor();
      e = expected.getDataBase();
      a = actual.getDataBase();
      this.checkData(e, a, abs.backend.java.lib.types.ABSBool.fromBoolean(true));
      this.checkData(a, e, abs.backend.java.lib.types.ABSBool.fromBoolean(false));
   return ABSUnit.UNIT;
   }
}// /home/jan/svn/hats/CaseStudies/models/fredhopper/replication/abs/Tests.abs:43:2: 
   public abs.backend.java.lib.types.ABSUnit checkData(DataBase e, DataBase a, abs.backend.java.lib.types.ABSBool record){ __ABS_checkSameCOG(); 
 {
      Set fids = new EmptySet__Constructor();
      abs.backend.java.lib.types.ABSInteger es = abs.backend.java.lib.types.ABSInteger.fromString("1").negate();
      abs.backend.java.lib.types.ABSInteger as = abs.backend.java.lib.types.ABSInteger.fromString("1").negate();
      fids = e.listFiles();
      while (hasNext__Function.<abs.backend.java.lib.types.ABSInteger>apply(fids).toBoolean()) {
         abs.backend.java.lib.types.ABSInteger id = abs.backend.java.lib.types.ABSInteger.fromString("1").negate();
         Pair nd = next__Function.<abs.backend.java.lib.types.ABSInteger>apply(fids);
         fids = fst__Function.<Set,abs.backend.java.lib.types.ABSInteger>apply(nd);
         id = snd__Function.<Set,abs.backend.java.lib.types.ABSInteger>apply(nd);
         es = e.getLength(id);
         as = a.getLength(id);
         if (record.toBoolean()) {
            result = new Insert__Constructor(new Triple__Constructor(id, es, as), result);
         }
         if (es.eq(as).toBoolean()) throw new abs.backend.java.lib.runtime.ABSAssertException("Assertion failed");
      }
   return ABSUnit.UNIT;
   }
}}
// /home/jan/svn/hats/CaseStudies/models/fredhopper/replication/abs/Servers.abs:9:1: 
class SyncServerImpl extends abs.backend.java.lib.runtime.ABSObject implements abs.backend.java.lib.types.ABSClass, SyncServer {
   private ServerDataBase db;
// /home/jan/svn/hats/CaseStudies/models/fredhopper/replication/abs/Servers.abs:13:2: 
   private abs.backend.java.lib.types.ABSInteger count = abs.backend.java.lib.types.ABSInteger.fromString("0");
// /home/jan/svn/hats/CaseStudies/models/fredhopper/replication/abs/Servers.abs:15:2: 
   private abs.backend.java.lib.types.ABSInteger cps = abs.backend.java.lib.types.ABSInteger.fromString("0");
// /home/jan/svn/hats/CaseStudies/models/fredhopper/replication/abs/Servers.abs:16:2: 
   private Set items = new EmptySet__Constructor();
// /home/jan/svn/hats/CaseStudies/models/fredhopper/replication/abs/Servers.abs:17:2: 
   private abs.backend.java.lib.types.ABSBool shutDown = abs.backend.java.lib.types.ABSBool.fromBoolean(false);
// /home/jan/svn/hats/CaseStudies/models/fredhopper/replication/abs/Servers.abs:19:2: 
   private SyncServerClientCoordinator coordinator = null;
// /home/jan/svn/hats/CaseStudies/models/fredhopper/replication/abs/Servers.abs:20:2: 
   private SyncServerAcceptor acceptor = null;
   public SyncServerImpl(ServerDataBase db) {
      this.db = db;
       getCOG().objectCreated(this);
   }
   protected ABSValue getFieldValue(java.lang.String __ABS_fieldName) throws java.lang.NoSuchFieldException {
   if ("db".equals(__ABS_fieldName)) return db;
   if ("count".equals(__ABS_fieldName)) return count;
   if ("cps".equals(__ABS_fieldName)) return cps;
   if ("items".equals(__ABS_fieldName)) return items;
   if ("shutDown".equals(__ABS_fieldName)) return shutDown;
   if ("coordinator".equals(__ABS_fieldName)) return coordinator;
   if ("acceptor".equals(__ABS_fieldName)) return acceptor;
       return super.getFieldValue(__ABS_fieldName);
   }
   public java.lang.String getClassName() { return "SyncServerImpl"; }
   public static SyncServerImpl __ABS_createNewCOG(ServerDataBase db) {
       final abs.backend.java.lib.runtime.COG __ABS_cog = new abs.backend.java.lib.runtime.COG(SyncServerImpl.class);
       final abs.backend.java.lib.runtime.ABSThread __ABS_thread = abs.backend.java.lib.runtime.ABSRuntime.getCurrentThread();
       final abs.backend.java.lib.runtime.COG __ABS_oldCOG = abs.backend.java.lib.runtime.ABSRuntime.getCurrentCOG();
       __ABS_thread.setCOG(__ABS_cog);
       try { 
            SyncServerImpl __ABS_result = new SyncServerImpl(db);
          abs.backend.java.lib.runtime.ABSRuntime.cogCreated(__ABS_result);
          return __ABS_result;
       } finally {
           __ABS_thread.setCOG(__ABS_oldCOG);
       }
   }
// /home/jan/svn/hats/CaseStudies/models/fredhopper/replication/abs/Servers.abs:22:2: 
   public abs.backend.java.lib.types.ABSUnit run(){ __ABS_checkSameCOG(); 
 {
      coordinator = new SyncServerClientCoordinatorImpl(this);
      acceptor = new SyncServerAcceptorImpl(this);
      abs.backend.java.lib.runtime.ABSRuntime.asyncCall(new abs.backend.java.lib.runtime.Task<SyncServerClientCoordinator>(this,coordinator) { protected ABSValue[] getArgs() { return new ABSValue[] {  }; }     public abs.backend.java.lib.runtime.Task<?> init() { return this; } public java.lang.String methodName() { return "process"; } public Object execute() { return target.process(); }}
     .init());
   return ABSUnit.UNIT;
   }
}// /home/jan/svn/hats/CaseStudies/models/fredhopper/replication/abs/Servers.abs:32:2: 
   public DataBase getDataBase(){ __ABS_checkSameCOG(); 
 {
      return db;
   }
}// /home/jan/svn/hats/CaseStudies/models/fredhopper/replication/abs/Servers.abs:36:2: 
   public Set getItems(){ __ABS_checkSameCOG(); 
 {
      return items;
   }
}// /home/jan/svn/hats/CaseStudies/models/fredhopper/replication/abs/Servers.abs:43:2: 
   public abs.backend.java.lib.types.ABSUnit refreshSnapShot(){ __ABS_checkSameCOG(); 
 {
      abs.backend.java.lib.runtime.ABSFut ns = null;
      abs.backend.java.lib.runtime.ABSFut rs = null;
      abs.backend.java.lib.runtime.ABSFut fl = null;
      abs.backend.java.lib.runtime.ABSFut end = null;
      Set fids = new EmptySet__Constructor();
      Set replications = new EmptySet__Constructor();
      abs.backend.java.lib.types.ABSBool refresh = abs.backend.java.lib.types.ABSBool.fromBoolean(false);
      if (items.eq(new EmptySet__Constructor()).toBoolean()) {
         cps = cps.add(abs.backend.java.lib.types.ABSInteger.fromString("1"));
         rs = abs.backend.java.lib.runtime.ABSRuntime.asyncCall(new abs.backend.java.lib.runtime.Task<ServerDataBase>(this,db) { protected ABSValue[] getArgs() { return new ABSValue[] {  }; }     public abs.backend.java.lib.runtime.Task<?> init() { return this; } public java.lang.String methodName() { return "refresh"; } public Object execute() { return target.refresh(); }}
     .init());
         abs.backend.java.lib.runtime.ABSRuntime.await(new abs.backend.java.lib.runtime.ABSFutureGuard(rs));
         refresh = (abs.backend.java.lib.types.ABSBool)rs.get();
         if (refresh.toBoolean()) {
            ns = abs.backend.java.lib.runtime.ABSRuntime.asyncCall(new abs.backend.java.lib.runtime.Task<SyncServerImpl>(this,this) { protected ABSValue[] getArgs() { return new ABSValue[] {  }; }     public abs.backend.java.lib.runtime.Task<?> init() { return this; } public java.lang.String methodName() { return "getFileNames"; } public Object execute() { return target.getFileNames(); }}
     .init());
            abs.backend.java.lib.runtime.ABSRuntime.await(new abs.backend.java.lib.runtime.ABSFutureGuard(ns));
            fids = (Set)ns.get();
            while (hasNext__Function.<abs.backend.java.lib.types.ABSInteger>apply(fids).toBoolean()) {
               abs.backend.java.lib.types.ABSInteger flength = abs.backend.java.lib.types.ABSInteger.fromString("1").negate();
               abs.backend.java.lib.types.ABSInteger fid = abs.backend.java.lib.types.ABSInteger.fromString("1").negate();
               Pair nt = next__Function.<abs.backend.java.lib.types.ABSInteger>apply(fids);
               fids = fst__Function.<Set,abs.backend.java.lib.types.ABSInteger>apply(nt);
               fid = snd__Function.<Set,abs.backend.java.lib.types.ABSInteger>apply(nt);
               fl = abs.backend.java.lib.runtime.ABSRuntime.asyncCall(new abs.backend.java.lib.runtime.Task<SyncServerImpl>(this,this) {abs.backend.java.lib.types.ABSInteger arg0; protected ABSValue[] getArgs() { return new ABSValue[] { arg0 }; }     public abs.backend.java.lib.runtime.Task<?> init(abs.backend.java.lib.types.ABSInteger _arg0) {arg0 = _arg0; return this; } public java.lang.String methodName() { return "getFileLength"; } public Object execute() { return target.getFileLength(arg0); }}
     .init(fid));
               abs.backend.java.lib.runtime.ABSRuntime.await(new abs.backend.java.lib.runtime.ABSFutureGuard(fl));
               flength = (abs.backend.java.lib.types.ABSInteger)fl.get();
               replications = new Insert__Constructor(new Pair__Constructor(fid, flength), replications);
            }
            items = new Insert__Constructor(new Pair__Constructor(cps, replications), items);
         }
         else {
            end = abs.backend.java.lib.runtime.ABSRuntime.asyncCall(new abs.backend.java.lib.runtime.Task<SyncServerImpl>(this,this) { protected ABSValue[] getArgs() { return new ABSValue[] {  }; }     public abs.backend.java.lib.runtime.Task<?> init() { return this; } public java.lang.String methodName() { return "requestShutDown"; } public Object execute() { return target.requestShutDown(); }}
     .init());
            abs.backend.java.lib.runtime.ABSRuntime.await(new abs.backend.java.lib.runtime.ABSFutureGuard(end));
         }
      }
   return ABSUnit.UNIT;
   }
}// /home/jan/svn/hats/CaseStudies/models/fredhopper/replication/abs/Servers.abs:95:2: 
   public abs.backend.java.lib.types.ABSUnit clearSnapShot(){ __ABS_checkSameCOG(); 
 {
      items = new EmptySet__Constructor();
   return ABSUnit.UNIT;
   }
}// /home/jan/svn/hats/CaseStudies/models/fredhopper/replication/abs/Servers.abs:100:2: 
   public abs.backend.java.lib.types.ABSInteger getFileLength(abs.backend.java.lib.types.ABSInteger fid){ __ABS_checkSameCOG(); 
 {
      abs.backend.java.lib.runtime.ABSFut fl = null;
      fl = abs.backend.java.lib.runtime.ABSRuntime.asyncCall(new abs.backend.java.lib.runtime.Task<ServerDataBase>(this,db) {abs.backend.java.lib.types.ABSInteger arg0; protected ABSValue[] getArgs() { return new ABSValue[] { arg0 }; }     public abs.backend.java.lib.runtime.Task<?> init(abs.backend.java.lib.types.ABSInteger _arg0) {arg0 = _arg0; return this; } public java.lang.String methodName() { return "getLength"; } public Object execute() { return target.getLength(arg0); }}
     .init(fid));
      abs.backend.java.lib.runtime.ABSRuntime.await(new abs.backend.java.lib.runtime.ABSFutureGuard(fl));
      return (abs.backend.java.lib.types.ABSInteger)fl.get();
   }
}// /home/jan/svn/hats/CaseStudies/models/fredhopper/replication/abs/Servers.abs:109:2: 
   public Set getFileNames(){ __ABS_checkSameCOG(); 
 {
      abs.backend.java.lib.runtime.ABSFut fs = null;
      fs = abs.backend.java.lib.runtime.ABSRuntime.asyncCall(new abs.backend.java.lib.runtime.Task<ServerDataBase>(this,db) { protected ABSValue[] getArgs() { return new ABSValue[] {  }; }     public abs.backend.java.lib.runtime.Task<?> init() { return this; } public java.lang.String methodName() { return "listCheckPointFiles"; } public Object execute() { return target.listCheckPointFiles(); }}
     .init());
      abs.backend.java.lib.runtime.ABSRuntime.await(new abs.backend.java.lib.runtime.ABSFutureGuard(fs));
      return (Set)fs.get();
   }
}// /home/jan/svn/hats/CaseStudies/models/fredhopper/replication/abs/Servers.abs:116:2: 
   public abs.backend.java.lib.types.ABSBool isShutdownRequested(){ __ABS_checkSameCOG(); 
 {
      abs.backend.java.lib.runtime.ABSRuntime.suspend();
      return shutDown;
   }
}// /home/jan/svn/hats/CaseStudies/models/fredhopper/replication/abs/Servers.abs:123:2: 
   public abs.backend.java.lib.types.ABSUnit requestShutDown(){ __ABS_checkSameCOG(); 
 {
      this.shutDown = abs.backend.java.lib.types.ABSBool.fromBoolean(true);
   return ABSUnit.UNIT;
   }
}// /home/jan/svn/hats/CaseStudies/models/fredhopper/replication/abs/Servers.abs:127:2: 
   public SyncServerAcceptor getAcceptor(){ __ABS_checkSameCOG(); 
 {
      return acceptor;
   }
}// /home/jan/svn/hats/CaseStudies/models/fredhopper/replication/abs/Servers.abs:131:2: 
   public SyncServerClientCoordinator getCoordinator(){ __ABS_checkSameCOG(); 
 {
      return coordinator;
   }
}}
// /home/jan/svn/hats/CaseStudies/models/fredhopper/replication/abs/Servers.abs:139:1: 
class SyncServerClientCoordinatorImpl extends abs.backend.java.lib.runtime.ABSObject implements abs.backend.java.lib.types.ABSClass, SyncServerClientCoordinator {
   private SyncServer server;
// /home/jan/svn/hats/CaseStudies/models/fredhopper/replication/abs/Servers.abs:142:2: 
   private abs.backend.java.lib.types.ABSInteger count = abs.backend.java.lib.types.ABSInteger.fromString("0");
// /home/jan/svn/hats/CaseStudies/models/fredhopper/replication/abs/Servers.abs:145:2: 
   private abs.backend.java.lib.types.ABSBool internal = abs.backend.java.lib.types.ABSBool.fromBoolean(false);
// /home/jan/svn/hats/CaseStudies/models/fredhopper/replication/abs/Servers.abs:148:2: 
   private abs.backend.java.lib.types.ABSBool replicationSignal = abs.backend.java.lib.types.ABSBool.fromBoolean(true);
// /home/jan/svn/hats/CaseStudies/models/fredhopper/replication/abs/Servers.abs:150:2: 
   private SyncServerAcceptor acceptor = null;
// /home/jan/svn/hats/CaseStudies/models/fredhopper/replication/abs/Servers.abs:151:2: 
   private Set threads = new EmptySet__Constructor();
   public SyncServerClientCoordinatorImpl(SyncServer server) {
      this.server = server;
       getCOG().objectCreated(this);
   }
   protected ABSValue getFieldValue(java.lang.String __ABS_fieldName) throws java.lang.NoSuchFieldException {
   if ("server".equals(__ABS_fieldName)) return server;
   if ("count".equals(__ABS_fieldName)) return count;
   if ("internal".equals(__ABS_fieldName)) return internal;
   if ("replicationSignal".equals(__ABS_fieldName)) return replicationSignal;
   if ("acceptor".equals(__ABS_fieldName)) return acceptor;
   if ("threads".equals(__ABS_fieldName)) return threads;
       return super.getFieldValue(__ABS_fieldName);
   }
   public java.lang.String getClassName() { return "SyncServerClientCoordinatorImpl"; }
   public static SyncServerClientCoordinatorImpl __ABS_createNewCOG(SyncServer server) {
       final abs.backend.java.lib.runtime.COG __ABS_cog = new abs.backend.java.lib.runtime.COG(SyncServerClientCoordinatorImpl.class);
       final abs.backend.java.lib.runtime.ABSThread __ABS_thread = abs.backend.java.lib.runtime.ABSRuntime.getCurrentThread();
       final abs.backend.java.lib.runtime.COG __ABS_oldCOG = abs.backend.java.lib.runtime.ABSRuntime.getCurrentCOG();
       __ABS_thread.setCOG(__ABS_cog);
       try { 
            SyncServerClientCoordinatorImpl __ABS_result = new SyncServerClientCoordinatorImpl(server);
          abs.backend.java.lib.runtime.ABSRuntime.cogCreated(__ABS_result);
          return __ABS_result;
       } finally {
           __ABS_thread.setCOG(__ABS_oldCOG);
       }
   }
// /home/jan/svn/hats/CaseStudies/models/fredhopper/replication/abs/Servers.abs:153:2: 
   public abs.backend.java.lib.types.ABSUnit process(){ __ABS_checkSameCOG(); 
 {
      abs.backend.java.lib.runtime.ABSFut acc = null;
      abs.backend.java.lib.runtime.ABSFut ac = null;
      abs.backend.java.lib.runtime.ABSFut end = null;
      abs.backend.java.lib.types.ABSBool shutdown = abs.backend.java.lib.types.ABSBool.fromBoolean(false);
      abs.backend.java.lib.types.ABSBool accept = abs.backend.java.lib.types.ABSBool.fromBoolean(false);
      acc = abs.backend.java.lib.runtime.ABSRuntime.asyncCall(new abs.backend.java.lib.runtime.Task<SyncServer>(this,server) { protected ABSValue[] getArgs() { return new ABSValue[] {  }; }     public abs.backend.java.lib.runtime.Task<?> init() { return this; } public java.lang.String methodName() { return "getAcceptor"; } public Object execute() { return target.getAcceptor(); }}
     .init());
      abs.backend.java.lib.runtime.ABSRuntime.await(new abs.backend.java.lib.runtime.ABSFutureGuard(acc));
      acceptor = (SyncServerAcceptor)acc.get();
      shutdown = this.isServerShutingDown();
      while (shutdown.negate().toBoolean()) {
         ac = abs.backend.java.lib.runtime.ABSRuntime.asyncCall(new abs.backend.java.lib.runtime.Task<SyncServerAcceptor>(this,acceptor) { protected ABSValue[] getArgs() { return new ABSValue[] {  }; }     public abs.backend.java.lib.runtime.Task<?> init() { return this; } public java.lang.String methodName() { return "isAcceptingConnection"; } public Object execute() { return target.isAcceptingConnection(); }}
     .init());
         abs.backend.java.lib.runtime.ABSRuntime.await(new abs.backend.java.lib.runtime.ABSFutureGuard(ac));
         accept = (abs.backend.java.lib.types.ABSBool)ac.get();
         if (accept.toBoolean()) {
            if (emptySet__Function.<ConnectionThread>apply(threads).negate().and(internal).toBoolean()) {
               acceptor.suspendConnection();
               internal = abs.backend.java.lib.types.ABSBool.fromBoolean(false);
            }
            else {
               internal = abs.backend.java.lib.types.ABSBool.fromBoolean(true);
            }
         }
         else {
            if (emptySet__Function.<ConnectionThread>apply(threads).toBoolean()) {
               acceptor.resumingConnection();
            }
         }
         shutdown = this.isServerShutingDown();
      }
      abs.backend.java.lib.runtime.ABSRuntime.await(new abs.backend.java.lib.runtime.ABSExpGuard() { public ABSBool evaluateExp() { return threads.eq(new EmptySet__Constructor()); }});
      acceptor.resumingConnection();
   return ABSUnit.UNIT;
   }
}// /home/jan/svn/hats/CaseStudies/models/fredhopper/replication/abs/Servers.abs:196:2: 
   public abs.backend.java.lib.types.ABSBool isServerShutingDown(){ __ABS_checkSameCOG(); 
 {
      abs.backend.java.lib.runtime.ABSFut sd = null;
      sd = abs.backend.java.lib.runtime.ABSRuntime.asyncCall(new abs.backend.java.lib.runtime.Task<SyncServer>(this,server) { protected ABSValue[] getArgs() { return new ABSValue[] {  }; }     public abs.backend.java.lib.runtime.Task<?> init() { return this; } public java.lang.String methodName() { return "isShutdownRequested"; } public Object execute() { return target.isShutdownRequested(); }}
     .init());
      abs.backend.java.lib.runtime.ABSRuntime.await(new abs.backend.java.lib.runtime.ABSFutureGuard(sd));
      return (abs.backend.java.lib.types.ABSBool)sd.get();
   }
}// /home/jan/svn/hats/CaseStudies/models/fredhopper/replication/abs/Servers.abs:204:2: 
   public abs.backend.java.lib.types.ABSUnit startReplicationUpdate(ConnectionThread thread){ __ABS_checkSameCOG(); 
 {
      abs.backend.java.lib.runtime.ABSRuntime.await(new abs.backend.java.lib.runtime.ABSExpGuard() { public ABSBool evaluateExp() { return replicationSignal; }});
      threads = new Insert__Constructor(thread, threads);
      if (size__Function.<ConnectionThread>apply(threads).eq(abs.backend.java.lib.types.ABSInteger.fromString("1")).toBoolean()) {
         abs.backend.java.lib.runtime.ABSFut end = null;
         replicationSignal = abs.backend.java.lib.types.ABSBool.fromBoolean(false);
         end = abs.backend.java.lib.runtime.ABSRuntime.asyncCall(new abs.backend.java.lib.runtime.Task<SyncServer>(this,server) { protected ABSValue[] getArgs() { return new ABSValue[] {  }; }     public abs.backend.java.lib.runtime.Task<?> init() { return this; } public java.lang.String methodName() { return "refreshSnapShot"; } public Object execute() { return target.refreshSnapShot(); }}
     .init());
         abs.backend.java.lib.runtime.ABSRuntime.await(new abs.backend.java.lib.runtime.ABSFutureGuard(end));
         replicationSignal = abs.backend.java.lib.types.ABSBool.fromBoolean(true);
      }
   return ABSUnit.UNIT;
   }
}// /home/jan/svn/hats/CaseStudies/models/fredhopper/replication/abs/Servers.abs:217:2: 
   public abs.backend.java.lib.types.ABSUnit finishReplicationUpdate(ConnectionThread thread){ __ABS_checkSameCOG(); 
 {
      abs.backend.java.lib.runtime.ABSRuntime.await(new abs.backend.java.lib.runtime.ABSExpGuard() { public ABSBool evaluateExp() { return replicationSignal; }});
      if (contains__Function.<ConnectionThread>apply(threads, thread).toBoolean()) {
         if (size__Function.<ConnectionThread>apply(threads).eq(abs.backend.java.lib.types.ABSInteger.fromString("1")).toBoolean()) {
            abs.backend.java.lib.runtime.ABSFut end = null;
            replicationSignal = abs.backend.java.lib.types.ABSBool.fromBoolean(false);
            end = abs.backend.java.lib.runtime.ABSRuntime.asyncCall(new abs.backend.java.lib.runtime.Task<SyncServer>(this,server) { protected ABSValue[] getArgs() { return new ABSValue[] {  }; }     public abs.backend.java.lib.runtime.Task<?> init() { return this; } public java.lang.String methodName() { return "clearSnapShot"; } public Object execute() { return target.clearSnapShot(); }}
     .init());
            abs.backend.java.lib.runtime.ABSRuntime.await(new abs.backend.java.lib.runtime.ABSFutureGuard(end));
            replicationSignal = abs.backend.java.lib.types.ABSBool.fromBoolean(true);
         }
         threads = remove__Function.<ConnectionThread>apply(threads, thread);
      }
      replicationSignal = abs.backend.java.lib.types.ABSBool.fromBoolean(true);
   return ABSUnit.UNIT;
   }
}}
// /home/jan/svn/hats/CaseStudies/models/fredhopper/replication/abs/Servers.abs:233:1: 
class SyncServerAcceptorImpl extends abs.backend.java.lib.runtime.ABSObject implements abs.backend.java.lib.types.ABSClass, SyncServerAcceptor {
   private SyncServer server;
// /home/jan/svn/hats/CaseStudies/models/fredhopper/replication/abs/Servers.abs:238:2: 
   private abs.backend.java.lib.types.ABSBool accept = abs.backend.java.lib.types.ABSBool.fromBoolean(true);
// /home/jan/svn/hats/CaseStudies/models/fredhopper/replication/abs/Servers.abs:241:2: 
   private abs.backend.java.lib.types.ABSBool shutdown = abs.backend.java.lib.types.ABSBool.fromBoolean(false);
// /home/jan/svn/hats/CaseStudies/models/fredhopper/replication/abs/Servers.abs:243:2: 
   private abs.backend.java.lib.types.ABSInteger count = abs.backend.java.lib.types.ABSInteger.fromString("0");
   public SyncServerAcceptorImpl(SyncServer server) {
      this.server = server;
       getCOG().objectCreated(this);
   }
   protected ABSValue getFieldValue(java.lang.String __ABS_fieldName) throws java.lang.NoSuchFieldException {
   if ("server".equals(__ABS_fieldName)) return server;
   if ("accept".equals(__ABS_fieldName)) return accept;
   if ("shutdown".equals(__ABS_fieldName)) return shutdown;
   if ("count".equals(__ABS_fieldName)) return count;
       return super.getFieldValue(__ABS_fieldName);
   }
   public java.lang.String getClassName() { return "SyncServerAcceptorImpl"; }
   public static SyncServerAcceptorImpl __ABS_createNewCOG(SyncServer server) {
       final abs.backend.java.lib.runtime.COG __ABS_cog = new abs.backend.java.lib.runtime.COG(SyncServerAcceptorImpl.class);
       final abs.backend.java.lib.runtime.ABSThread __ABS_thread = abs.backend.java.lib.runtime.ABSRuntime.getCurrentThread();
       final abs.backend.java.lib.runtime.COG __ABS_oldCOG = abs.backend.java.lib.runtime.ABSRuntime.getCurrentCOG();
       __ABS_thread.setCOG(__ABS_cog);
       try { 
            SyncServerAcceptorImpl __ABS_result = new SyncServerAcceptorImpl(server);
          abs.backend.java.lib.runtime.ABSRuntime.cogCreated(__ABS_result);
          return __ABS_result;
       } finally {
           __ABS_thread.setCOG(__ABS_oldCOG);
       }
   }
// /home/jan/svn/hats/CaseStudies/models/fredhopper/replication/abs/Servers.abs:245:2: 
   public abs.backend.java.lib.types.ABSBool isServerShutingDown(){ __ABS_checkSameCOG(); 
 {
      abs.backend.java.lib.runtime.ABSFut ss = null;
      ss = abs.backend.java.lib.runtime.ABSRuntime.asyncCall(new abs.backend.java.lib.runtime.Task<SyncServer>(this,server) { protected ABSValue[] getArgs() { return new ABSValue[] {  }; }     public abs.backend.java.lib.runtime.Task<?> init() { return this; } public java.lang.String methodName() { return "isShutdownRequested"; } public Object execute() { return target.isShutdownRequested(); }}
     .init());
      abs.backend.java.lib.runtime.ABSRuntime.await(new abs.backend.java.lib.runtime.ABSFutureGuard(ss));
      return (abs.backend.java.lib.types.ABSBool)ss.get();
   }
}// /home/jan/svn/hats/CaseStudies/models/fredhopper/replication/abs/Servers.abs:253:2: 
   public ConnectionThread getConnection(ClientJob job){ __ABS_checkSameCOG(); 
 {
      ConnectionThread thread = null;
      shutdown = this.isServerShutingDown();
      if (shutdown.negate().toBoolean()) {
         abs.backend.java.lib.runtime.ABSRuntime.await(new abs.backend.java.lib.runtime.ABSExpGuard() { public ABSBool evaluateExp() { return accept; }});
         thread = new ConnectionThreadImpl(job, server);
      }
      return thread;
   }
}// /home/jan/svn/hats/CaseStudies/models/fredhopper/replication/abs/Servers.abs:263:2: 
   public abs.backend.java.lib.types.ABSBool isAcceptingConnection(){ __ABS_checkSameCOG(); 
 {
      return accept;
   }
}// /home/jan/svn/hats/CaseStudies/models/fredhopper/replication/abs/Servers.abs:267:2: 
   public abs.backend.java.lib.types.ABSUnit suspendConnection(){ __ABS_checkSameCOG(); 
 {
      accept = abs.backend.java.lib.types.ABSBool.fromBoolean(false);
   return ABSUnit.UNIT;
   }
}// /home/jan/svn/hats/CaseStudies/models/fredhopper/replication/abs/Servers.abs:271:2: 
   public abs.backend.java.lib.types.ABSUnit resumingConnection(){ __ABS_checkSameCOG(); 
 {
      accept = abs.backend.java.lib.types.ABSBool.fromBoolean(true);
   return ABSUnit.UNIT;
   }
}}
// /home/jan/svn/hats/CaseStudies/models/fredhopper/replication/abs/Servers.abs:276:1: 
class ConnectionThreadImpl extends abs.backend.java.lib.runtime.ABSObject implements abs.backend.java.lib.types.ABSClass, ConnectionThread {
   private ClientJob job;
   private SyncServer server;
// /home/jan/svn/hats/CaseStudies/models/fredhopper/replication/abs/Servers.abs:278:2: 
   private SyncServerClientCoordinator coord;
// /home/jan/svn/hats/CaseStudies/models/fredhopper/replication/abs/Servers.abs:279:2: 
   private Maybe cmd = new Nothing__Constructor();
   public ConnectionThreadImpl(ClientJob job, SyncServer server) {
      this.job = job;
      this.server = server;
       getCOG().objectCreated(this);
   }
   protected ABSValue getFieldValue(java.lang.String __ABS_fieldName) throws java.lang.NoSuchFieldException {
   if ("job".equals(__ABS_fieldName)) return job;
   if ("server".equals(__ABS_fieldName)) return server;
   if ("coord".equals(__ABS_fieldName)) return coord;
   if ("cmd".equals(__ABS_fieldName)) return cmd;
       return super.getFieldValue(__ABS_fieldName);
   }
   public java.lang.String getClassName() { return "ConnectionThreadImpl"; }
   public static ConnectionThreadImpl __ABS_createNewCOG(ClientJob job, SyncServer server) {
       final abs.backend.java.lib.runtime.COG __ABS_cog = new abs.backend.java.lib.runtime.COG(ConnectionThreadImpl.class);
       final abs.backend.java.lib.runtime.ABSThread __ABS_thread = abs.backend.java.lib.runtime.ABSRuntime.getCurrentThread();
       final abs.backend.java.lib.runtime.COG __ABS_oldCOG = abs.backend.java.lib.runtime.ABSRuntime.getCurrentCOG();
       __ABS_thread.setCOG(__ABS_cog);
       try { 
            ConnectionThreadImpl __ABS_result = new ConnectionThreadImpl(job, server);
          abs.backend.java.lib.runtime.ABSRuntime.cogCreated(__ABS_result);
          return __ABS_result;
       } finally {
           __ABS_thread.setCOG(__ABS_oldCOG);
       }
   }
// /home/jan/svn/hats/CaseStudies/models/fredhopper/replication/abs/Servers.abs:281:2: 
   public abs.backend.java.lib.types.ABSUnit run(){ __ABS_checkSameCOG(); 
 {
      abs.backend.java.lib.runtime.ABSFut c = null;
      abs.backend.java.lib.runtime.ABSFut rp = null;
      abs.backend.java.lib.runtime.ABSFut is = null;
      Set items = new EmptySet__Constructor();
      Set filesets = new EmptySet__Constructor();
      c = abs.backend.java.lib.runtime.ABSRuntime.asyncCall(new abs.backend.java.lib.runtime.Task<SyncServer>(this,server) { protected ABSValue[] getArgs() { return new ABSValue[] {  }; }     public abs.backend.java.lib.runtime.Task<?> init() { return this; } public java.lang.String methodName() { return "getCoordinator"; } public Object execute() { return target.getCoordinator(); }}
     .init());
      abs.backend.java.lib.runtime.ABSRuntime.await(new abs.backend.java.lib.runtime.ABSFutureGuard(c));
      coord = (SyncServerClientCoordinator)c.get();
      rp = abs.backend.java.lib.runtime.ABSRuntime.asyncCall(new abs.backend.java.lib.runtime.Task<SyncServerClientCoordinator>(this,coord) {ConnectionThreadImpl arg0; protected ABSValue[] getArgs() { return new ABSValue[] { arg0 }; }     public abs.backend.java.lib.runtime.Task<?> init(ConnectionThreadImpl _arg0) {arg0 = _arg0; return this; } public java.lang.String methodName() { return "startReplicationUpdate"; } public Object execute() { return target.startReplicationUpdate(arg0); }}
     .init(this));
      abs.backend.java.lib.runtime.ABSRuntime.await(new abs.backend.java.lib.runtime.ABSFutureGuard(rp));
      abs.backend.java.lib.runtime.ABSRuntime.await(new abs.backend.java.lib.runtime.ABSExpGuard() { public ABSBool evaluateExp() { return cmd.notEq(new Nothing__Constructor()); }});
      rp = abs.backend.java.lib.runtime.ABSRuntime.asyncCall(new abs.backend.java.lib.runtime.Task<ClientJob>(this,job) { protected ABSValue[] getArgs() { return new ABSValue[] {  }; }     public abs.backend.java.lib.runtime.Task<?> init() { return this; } public java.lang.String methodName() { return "receiveSchedule"; } public Object execute() { return target.receiveSchedule(); }}
     .init());
      abs.backend.java.lib.runtime.ABSRuntime.await(new abs.backend.java.lib.runtime.ABSFutureGuard(rp));
      if (cmd.notEq(new Just__Constructor(new ListSchedule__Constructor())).toBoolean()) {
         is = abs.backend.java.lib.runtime.ABSRuntime.asyncCall(new abs.backend.java.lib.runtime.Task<SyncServer>(this,server) { protected ABSValue[] getArgs() { return new ABSValue[] {  }; }     public abs.backend.java.lib.runtime.Task<?> init() { return this; } public java.lang.String methodName() { return "getItems"; } public Object execute() { return target.getItems(); }}
     .init());
         abs.backend.java.lib.runtime.ABSRuntime.await(new abs.backend.java.lib.runtime.ABSFutureGuard(is));
         items = (Set)is.get();
         filesets = this.registerItems(items);
         rp = abs.backend.java.lib.runtime.ABSRuntime.asyncCall(new abs.backend.java.lib.runtime.Task<ClientJob>(this,job) {Command arg0; protected ABSValue[] getArgs() { return new ABSValue[] { arg0 }; }     public abs.backend.java.lib.runtime.Task<?> init(Command _arg0) {arg0 = _arg0; return this; } public java.lang.String methodName() { return "command"; } public Object execute() { return target.command(arg0); }}
     .init(new StartSnapShot__Constructor()));
         abs.backend.java.lib.runtime.ABSRuntime.await(new abs.backend.java.lib.runtime.ABSFutureGuard(rp));
         while (hasNext__Function.<Set>apply(filesets).toBoolean()) {
            Set fileset = new EmptySet__Constructor();
            Pair nfs = next__Function.<Set>apply(filesets);
            filesets = fst__Function.<Set,Set>apply(nfs);
            fileset = snd__Function.<Set,Set>apply(nfs);
            this.transferItems(fileset);
         }
         rp = abs.backend.java.lib.runtime.ABSRuntime.asyncCall(new abs.backend.java.lib.runtime.Task<ClientJob>(this,job) {Command arg0; protected ABSValue[] getArgs() { return new ABSValue[] { arg0 }; }     public abs.backend.java.lib.runtime.Task<?> init(Command _arg0) {arg0 = _arg0; return this; } public java.lang.String methodName() { return "command"; } public Object execute() { return target.command(arg0); }}
     .init(new EndSnapShot__Constructor()));
         abs.backend.java.lib.runtime.ABSRuntime.await(new abs.backend.java.lib.runtime.ABSFutureGuard(rp));
      }
      rp = abs.backend.java.lib.runtime.ABSRuntime.asyncCall(new abs.backend.java.lib.runtime.Task<SyncServerClientCoordinator>(this,coord) {ConnectionThreadImpl arg0; protected ABSValue[] getArgs() { return new ABSValue[] { arg0 }; }     public abs.backend.java.lib.runtime.Task<?> init(ConnectionThreadImpl _arg0) {arg0 = _arg0; return this; } public java.lang.String methodName() { return "finishReplicationUpdate"; } public Object execute() { return target.finishReplicationUpdate(arg0); }}
     .init(this));
      abs.backend.java.lib.runtime.ABSRuntime.await(new abs.backend.java.lib.runtime.ABSFutureGuard(rp));
   return ABSUnit.UNIT;
   }
}// /home/jan/svn/hats/CaseStudies/models/fredhopper/replication/abs/Servers.abs:337:2: 
   public abs.backend.java.lib.types.ABSUnit command(Command c){ __ABS_checkSameCOG(); 
 {
      cmd = new Just__Constructor(c);
   return ABSUnit.UNIT;
   }
}// /home/jan/svn/hats/CaseStudies/models/fredhopper/replication/abs/Servers.abs:343:2: 
   public Set registerItems(Set items){ __ABS_checkSameCOG(); 
 {
      Set regs = new EmptySet__Constructor();
      while (hasNext__Function.<Pair>apply(items).toBoolean()) {
         abs.backend.java.lib.runtime.ABSFut b = null;
         abs.backend.java.lib.types.ABSBool register = abs.backend.java.lib.types.ABSBool.fromBoolean(false);
         abs.backend.java.lib.types.ABSInteger cp = abs.backend.java.lib.types.ABSInteger.fromString("1").negate();
         Maybe item = new Nothing__Constructor();
         Pair nis = next__Function.<Pair>apply(items);
         items = fst__Function.<Set,Pair>apply(nis);
         item = new Just__Constructor(snd__Function.<Set,Pair>apply(nis));
         cp = fst__Function.<abs.backend.java.lib.types.ABSInteger,Set>apply(fromJust__Function.<Pair>apply(item));
         b = abs.backend.java.lib.runtime.ABSRuntime.asyncCall(new abs.backend.java.lib.runtime.Task<ClientJob>(this,job) {abs.backend.java.lib.types.ABSInteger arg0; protected ABSValue[] getArgs() { return new ABSValue[] { arg0 }; }     public abs.backend.java.lib.runtime.Task<?> init(abs.backend.java.lib.types.ABSInteger _arg0) {arg0 = _arg0; return this; } public java.lang.String methodName() { return "registerReplicationItems"; } public Object execute() { return target.registerReplicationItems(arg0); }}
     .init(cp));
         abs.backend.java.lib.runtime.ABSRuntime.await(new abs.backend.java.lib.runtime.ABSFutureGuard(b));
         register = (abs.backend.java.lib.types.ABSBool)b.get();
         if (register.toBoolean()) {
            regs = new Insert__Constructor(snd__Function.<abs.backend.java.lib.types.ABSInteger,Set>apply(fromJust__Function.<Pair>apply(item)), regs);
         }
      }
      return regs;
   }
}// /home/jan/svn/hats/CaseStudies/models/fredhopper/replication/abs/Servers.abs:370:2: 
   public abs.backend.java.lib.types.ABSUnit transferItems(Set fileset){ __ABS_checkSameCOG(); 
 {
      abs.backend.java.lib.runtime.ABSFut rp = null;
      while (hasNext__Function.<Pair>apply(fileset).toBoolean()) {
         abs.backend.java.lib.runtime.ABSFut fs = null;
         Pair file = new Pair__Constructor(abs.backend.java.lib.types.ABSInteger.fromString("1").negate(), abs.backend.java.lib.types.ABSInteger.fromString("1").negate());
         abs.backend.java.lib.types.ABSInteger size = abs.backend.java.lib.types.ABSInteger.fromString("1").negate();
         abs.backend.java.lib.types.ABSInteger tsize = abs.backend.java.lib.types.ABSInteger.fromString("1").negate();
         Pair nf = next__Function.<Pair>apply(fileset);
         fileset = fst__Function.<Set,Pair>apply(nf);
         file = snd__Function.<Set,Pair>apply(nf);
         tsize = snd__Function.<abs.backend.java.lib.types.ABSInteger,abs.backend.java.lib.types.ABSInteger>apply(file);
         rp = abs.backend.java.lib.runtime.ABSRuntime.asyncCall(new abs.backend.java.lib.runtime.Task<ClientJob>(this,job) {Command arg0; protected ABSValue[] getArgs() { return new ABSValue[] { arg0 }; }     public abs.backend.java.lib.runtime.Task<?> init(Command _arg0) {arg0 = _arg0; return this; } public java.lang.String methodName() { return "command"; } public Object execute() { return target.command(arg0); }}
     .init(new AppendSearchFile__Constructor()));
         abs.backend.java.lib.runtime.ABSRuntime.await(new abs.backend.java.lib.runtime.ABSFutureGuard(rp));
         fs = abs.backend.java.lib.runtime.ABSRuntime.asyncCall(new abs.backend.java.lib.runtime.Task<ClientJob>(this,job) {abs.backend.java.lib.types.ABSInteger arg0; protected ABSValue[] getArgs() { return new ABSValue[] { arg0 }; }     public abs.backend.java.lib.runtime.Task<?> init(abs.backend.java.lib.types.ABSInteger _arg0) {arg0 = _arg0; return this; } public java.lang.String methodName() { return "processFile"; } public Object execute() { return target.processFile(arg0); }}
     .init(fst__Function.<abs.backend.java.lib.types.ABSInteger,abs.backend.java.lib.types.ABSInteger>apply(file)));
         abs.backend.java.lib.runtime.ABSRuntime.await(new abs.backend.java.lib.runtime.ABSFutureGuard(fs));
         size = (abs.backend.java.lib.types.ABSInteger)fs.get();
         if (size.gt(tsize).toBoolean()) {
            rp = abs.backend.java.lib.runtime.ABSRuntime.asyncCall(new abs.backend.java.lib.runtime.Task<ClientJob>(this,job) {Command arg0; protected ABSValue[] getArgs() { return new ABSValue[] { arg0 }; }     public abs.backend.java.lib.runtime.Task<?> init(Command _arg0) {arg0 = _arg0; return this; } public java.lang.String methodName() { return "command"; } public Object execute() { return target.command(arg0); }}
     .init(new OverwriteFile__Constructor()));
            abs.backend.java.lib.runtime.ABSRuntime.await(new abs.backend.java.lib.runtime.ABSFutureGuard(rp));
            rp = abs.backend.java.lib.runtime.ABSRuntime.asyncCall(new abs.backend.java.lib.runtime.Task<ClientJob>(this,job) {Pair arg0; protected ABSValue[] getArgs() { return new ABSValue[] { arg0 }; }     public abs.backend.java.lib.runtime.Task<?> init(Pair _arg0) {arg0 = _arg0; return this; } public java.lang.String methodName() { return "processContent"; } public Object execute() { return target.processContent(arg0); }}
     .init(file));
            abs.backend.java.lib.runtime.ABSRuntime.await(new abs.backend.java.lib.runtime.ABSFutureGuard(rp));
         }
         else {
            if (tsize.subtract(size).gt(abs.backend.java.lib.types.ABSInteger.fromString("0")).toBoolean()) {
               rp = abs.backend.java.lib.runtime.ABSRuntime.asyncCall(new abs.backend.java.lib.runtime.Task<ClientJob>(this,job) {Command arg0; protected ABSValue[] getArgs() { return new ABSValue[] { arg0 }; }     public abs.backend.java.lib.runtime.Task<?> init(Command _arg0) {arg0 = _arg0; return this; } public java.lang.String methodName() { return "command"; } public Object execute() { return target.command(arg0); }}
     .init(new ContinueFile__Constructor()));
               abs.backend.java.lib.runtime.ABSRuntime.await(new abs.backend.java.lib.runtime.ABSFutureGuard(rp));
               file = new Pair__Constructor(fst__Function.<abs.backend.java.lib.types.ABSInteger,abs.backend.java.lib.types.ABSInteger>apply(file), tsize.subtract(size));
               rp = abs.backend.java.lib.runtime.ABSRuntime.asyncCall(new abs.backend.java.lib.runtime.Task<ClientJob>(this,job) {Pair arg0; protected ABSValue[] getArgs() { return new ABSValue[] { arg0 }; }     public abs.backend.java.lib.runtime.Task<?> init(Pair _arg0) {arg0 = _arg0; return this; } public java.lang.String methodName() { return "processContent"; } public Object execute() { return target.processContent(arg0); }}
     .init(file));
               abs.backend.java.lib.runtime.ABSRuntime.await(new abs.backend.java.lib.runtime.ABSFutureGuard(rp));
            }
            else {
               rp = abs.backend.java.lib.runtime.ABSRuntime.asyncCall(new abs.backend.java.lib.runtime.Task<ClientJob>(this,job) {Command arg0; protected ABSValue[] getArgs() { return new ABSValue[] { arg0 }; }     public abs.backend.java.lib.runtime.Task<?> init(Command _arg0) {arg0 = _arg0; return this; } public java.lang.String methodName() { return "command"; } public Object execute() { return target.command(arg0); }}
     .init(new SkipFile__Constructor()));
               abs.backend.java.lib.runtime.ABSRuntime.await(new abs.backend.java.lib.runtime.ABSFutureGuard(rp));
            }
         }
      }
      rp = abs.backend.java.lib.runtime.ABSRuntime.asyncCall(new abs.backend.java.lib.runtime.Task<ClientJob>(this,job) {Command arg0; protected ABSValue[] getArgs() { return new ABSValue[] { arg0 }; }     public abs.backend.java.lib.runtime.Task<?> init(Command _arg0) {arg0 = _arg0; return this; } public java.lang.String methodName() { return "command"; } public Object execute() { return target.command(arg0); }}
     .init(new EndSearchFile__Constructor()));
      abs.backend.java.lib.runtime.ABSRuntime.await(new abs.backend.java.lib.runtime.ABSFutureGuard(rp));
   return ABSUnit.UNIT;
   }
}}
// /home/jan/svn/hats/CaseStudies/models/fredhopper/replication/abs/Clients.abs:11:1: 
class SyncClientImpl extends abs.backend.java.lib.runtime.ABSObject implements abs.backend.java.lib.types.ABSClass, SyncClient, ClientConnector {
   private Map machine;
// /home/jan/svn/hats/CaseStudies/models/fredhopper/replication/abs/Clients.abs:14:2: 
   private ServerAcceptor acceptor = null;
// /home/jan/svn/hats/CaseStudies/models/fredhopper/replication/abs/Clients.abs:15:2: 
   private State state = new Start__Constructor();
// /home/jan/svn/hats/CaseStudies/models/fredhopper/replication/abs/Clients.abs:16:2: 
   private ClientDataBase db = null;
// /home/jan/svn/hats/CaseStudies/models/fredhopper/replication/abs/Clients.abs:17:2: 
   private abs.backend.java.lib.types.ABSInteger jobCount = abs.backend.java.lib.types.ABSInteger.fromString("0");
// /home/jan/svn/hats/CaseStudies/models/fredhopper/replication/abs/Clients.abs:18:2: 
   private abs.backend.java.lib.types.ABSBool shutDown = abs.backend.java.lib.types.ABSBool.fromBoolean(false);
   public SyncClientImpl(Map machine) {
      this.machine = machine;
       getCOG().objectCreated(this);
   }
   protected ABSValue getFieldValue(java.lang.String __ABS_fieldName) throws java.lang.NoSuchFieldException {
   if ("machine".equals(__ABS_fieldName)) return machine;
   if ("acceptor".equals(__ABS_fieldName)) return acceptor;
   if ("state".equals(__ABS_fieldName)) return state;
   if ("db".equals(__ABS_fieldName)) return db;
   if ("jobCount".equals(__ABS_fieldName)) return jobCount;
   if ("shutDown".equals(__ABS_fieldName)) return shutDown;
       return super.getFieldValue(__ABS_fieldName);
   }
   public java.lang.String getClassName() { return "SyncClientImpl"; }
   public static SyncClientImpl __ABS_createNewCOG(Map machine) {
       final abs.backend.java.lib.runtime.COG __ABS_cog = new abs.backend.java.lib.runtime.COG(SyncClientImpl.class);
       final abs.backend.java.lib.runtime.ABSThread __ABS_thread = abs.backend.java.lib.runtime.ABSRuntime.getCurrentThread();
       final abs.backend.java.lib.runtime.COG __ABS_oldCOG = abs.backend.java.lib.runtime.ABSRuntime.getCurrentCOG();
       __ABS_thread.setCOG(__ABS_cog);
       try { 
            SyncClientImpl __ABS_result = new SyncClientImpl(machine);
          abs.backend.java.lib.runtime.ABSRuntime.cogCreated(__ABS_result);
          return __ABS_result;
       } finally {
           __ABS_thread.setCOG(__ABS_oldCOG);
       }
   }
// /home/jan/svn/hats/CaseStudies/models/fredhopper/replication/abs/Clients.abs:20:2: 
   public abs.backend.java.lib.types.ABSBool isShutdownRequested(){ __ABS_checkSameCOG(); 
 {
      return shutDown;
   }
}// /home/jan/svn/hats/CaseStudies/models/fredhopper/replication/abs/Clients.abs:24:2: 
   public abs.backend.java.lib.types.ABSUnit requestShutDown(){ __ABS_checkSameCOG(); 
 {
      this.shutDown = abs.backend.java.lib.types.ABSBool.fromBoolean(true);
   return ABSUnit.UNIT;
   }
}// /home/jan/svn/hats/CaseStudies/models/fredhopper/replication/abs/Clients.abs:28:2: 
   public abs.backend.java.lib.types.ABSUnit incrementJob(){ __ABS_checkSameCOG(); 
 {
      jobCount = jobCount.add(abs.backend.java.lib.types.ABSInteger.fromString("1"));
   return ABSUnit.UNIT;
   }
}// /home/jan/svn/hats/CaseStudies/models/fredhopper/replication/abs/Clients.abs:32:2: 
   public ServerAcceptor getAcceptor(){ __ABS_checkSameCOG(); 
 {
      return acceptor;
   }
}// /home/jan/svn/hats/CaseStudies/models/fredhopper/replication/abs/Clients.abs:36:2: 
   public abs.backend.java.lib.types.ABSUnit run(){ __ABS_checkSameCOG(); 
 {
      abs.backend.java.lib.runtime.ABSFut end = null;
      this.db = new DataBaseImpl(new EmptyMap__Constructor());
      this.becomesState(new WaitToBoot__Constructor());
      abs.backend.java.lib.runtime.ABSRuntime.await(new abs.backend.java.lib.runtime.ABSExpGuard() { public ABSBool evaluateExp() { return acceptor.notEq(null); }});
      new ClientJobImpl(this, new Boot__Constructor());
   return ABSUnit.UNIT;
   }
}// /home/jan/svn/hats/CaseStudies/models/fredhopper/replication/abs/Clients.abs:52:2: 
   public ClientDataBase getClientDataBase(){ __ABS_checkSameCOG(); 
 {
      return db;
   }
}// /home/jan/svn/hats/CaseStudies/models/fredhopper/replication/abs/Clients.abs:56:2: 
   public DataBase getDataBase(){ __ABS_checkSameCOG(); 
 {
      return db;
   }
}// /home/jan/svn/hats/CaseStudies/models/fredhopper/replication/abs/Clients.abs:60:2: 
   public abs.backend.java.lib.types.ABSUnit becomesState(State state){ __ABS_checkSameCOG(); 
 {
      if (contains__Function.<State>apply(lookup__Function.<State,Set>apply(machine, this.state), state).toBoolean()) throw new abs.backend.java.lib.runtime.ABSAssertException("Assertion failed");
      this.state = state;
   return ABSUnit.UNIT;
   }
}// /home/jan/svn/hats/CaseStudies/models/fredhopper/replication/abs/Clients.abs:65:2: 
   public abs.backend.java.lib.types.ABSUnit setAcceptor(ServerAcceptor acceptor){ __ABS_checkSameCOG(); 
 {
      this.acceptor = acceptor;
   return ABSUnit.UNIT;
   }
}}
// /home/jan/svn/hats/CaseStudies/models/fredhopper/replication/abs/Clients.abs:73:1: 
class ClientJobImpl extends abs.backend.java.lib.runtime.ABSObject implements abs.backend.java.lib.types.ABSClass, ClientJob {
   private SyncClient client;
   private JobType jt;
// /home/jan/svn/hats/CaseStudies/models/fredhopper/replication/abs/Clients.abs:76:2: 
   private Command command = new EndSnapShot__Constructor();
// /home/jan/svn/hats/CaseStudies/models/fredhopper/replication/abs/Clients.abs:77:2: 
   private abs.backend.java.lib.types.ABSBool hasSchedule = abs.backend.java.lib.types.ABSBool.fromBoolean(false);
// /home/jan/svn/hats/CaseStudies/models/fredhopper/replication/abs/Clients.abs:78:2: 
   private ConnectionThread thread = null;
// /home/jan/svn/hats/CaseStudies/models/fredhopper/replication/abs/Clients.abs:79:2: 
   private ClientDataBase db;
   public ClientJobImpl(SyncClient client, JobType jt) {
      this.client = client;
      this.jt = jt;
       getCOG().objectCreated(this);
   }
   protected ABSValue getFieldValue(java.lang.String __ABS_fieldName) throws java.lang.NoSuchFieldException {
   if ("client".equals(__ABS_fieldName)) return client;
   if ("jt".equals(__ABS_fieldName)) return jt;
   if ("command".equals(__ABS_fieldName)) return command;
   if ("hasSchedule".equals(__ABS_fieldName)) return hasSchedule;
   if ("thread".equals(__ABS_fieldName)) return thread;
   if ("db".equals(__ABS_fieldName)) return db;
       return super.getFieldValue(__ABS_fieldName);
   }
   public java.lang.String getClassName() { return "ClientJobImpl"; }
   public static ClientJobImpl __ABS_createNewCOG(SyncClient client, JobType jt) {
       final abs.backend.java.lib.runtime.COG __ABS_cog = new abs.backend.java.lib.runtime.COG(ClientJobImpl.class);
       final abs.backend.java.lib.runtime.ABSThread __ABS_thread = abs.backend.java.lib.runtime.ABSRuntime.getCurrentThread();
       final abs.backend.java.lib.runtime.COG __ABS_oldCOG = abs.backend.java.lib.runtime.ABSRuntime.getCurrentCOG();
       __ABS_thread.setCOG(__ABS_cog);
       try { 
            ClientJobImpl __ABS_result = new ClientJobImpl(client, jt);
          abs.backend.java.lib.runtime.ABSRuntime.cogCreated(__ABS_result);
          return __ABS_result;
       } finally {
           __ABS_thread.setCOG(__ABS_oldCOG);
       }
   }
// /home/jan/svn/hats/CaseStudies/models/fredhopper/replication/abs/Clients.abs:81:2: 
   public abs.backend.java.lib.types.ABSUnit run(){ __ABS_checkSameCOG(); 
 {
      abs.backend.java.lib.runtime.ABSFut ep = null;
      abs.backend.java.lib.runtime.ABSFut t = null;
      abs.backend.java.lib.runtime.ABSFut end = null;
      abs.backend.java.lib.types.ABSBool empty = abs.backend.java.lib.types.ABSBool.fromBoolean(false);
      abs.backend.java.lib.types.ABSBool continue__ = abs.backend.java.lib.types.ABSBool.fromBoolean(false);
      ServerAcceptor acceptor = null;
      acceptor = client.getAcceptor();
      db = client.getClientDataBase();
      t = abs.backend.java.lib.runtime.ABSRuntime.asyncCall(new abs.backend.java.lib.runtime.Task<ServerAcceptor>(this,acceptor) {ClientJobImpl arg0; protected ABSValue[] getArgs() { return new ABSValue[] { arg0 }; }     public abs.backend.java.lib.runtime.Task<?> init(ClientJobImpl _arg0) {arg0 = _arg0; return this; } public java.lang.String methodName() { return "getConnection"; } public Object execute() { return target.getConnection(arg0); }}
     .init(this));
      abs.backend.java.lib.runtime.ABSRuntime.await(new abs.backend.java.lib.runtime.ABSFutureGuard(t));
      thread = (ConnectionThread)t.get();
      if (thread.notEq(null).toBoolean()) {
         if (jt.eq(new Boot__Constructor()).toBoolean()) {
            client.becomesState(new Booting__Constructor());
            end = abs.backend.java.lib.runtime.ABSRuntime.asyncCall(new abs.backend.java.lib.runtime.Task<ConnectionThread>(this,thread) {Command arg0; protected ABSValue[] getArgs() { return new ABSValue[] { arg0 }; }     public abs.backend.java.lib.runtime.Task<?> init(Command _arg0) {arg0 = _arg0; return this; } public java.lang.String methodName() { return "command"; } public Object execute() { return target.command(arg0); }}
     .init(new ListSchedule__Constructor()));
            abs.backend.java.lib.runtime.ABSRuntime.await(new abs.backend.java.lib.runtime.ABSExpGuard() { public ABSBool evaluateExp() { return hasSchedule.eq(abs.backend.java.lib.types.ABSBool.fromBoolean(true)); }});
         }
         else {
            client.becomesState(new WorkOnReplicate__Constructor());
            end = abs.backend.java.lib.runtime.ABSRuntime.asyncCall(new abs.backend.java.lib.runtime.Task<ConnectionThread>(this,thread) {Command arg0; protected ABSValue[] getArgs() { return new ABSValue[] { arg0 }; }     public abs.backend.java.lib.runtime.Task<?> init(Command _arg0) {arg0 = _arg0; return this; } public java.lang.String methodName() { return "command"; } public Object execute() { return target.command(arg0); }}
     .init(new SearchSchedule__Constructor()));
            abs.backend.java.lib.runtime.ABSRuntime.await(new abs.backend.java.lib.runtime.ABSExpGuard() { public ABSBool evaluateExp() { return hasSchedule.eq(abs.backend.java.lib.types.ABSBool.fromBoolean(true)); }});
            abs.backend.java.lib.runtime.ABSRuntime.await(new abs.backend.java.lib.runtime.ABSExpGuard() { public ABSBool evaluateExp() { return command.eq(new StartSnapShot__Constructor()); }});
            abs.backend.java.lib.runtime.ABSRuntime.await(new abs.backend.java.lib.runtime.ABSExpGuard() { public ABSBool evaluateExp() { return command.eq(new EndSnapShot__Constructor()); }});
         }
         client.becomesState(new WaitToReplicate__Constructor());
         client.incrementJob();
         new ClientJobImpl(client, new Replication__Constructor());
      }
      else {
         this.shutDownClient();
      }
   return ABSUnit.UNIT;
   }
}// /home/jan/svn/hats/CaseStudies/models/fredhopper/replication/abs/Clients.abs:125:2: 
   public abs.backend.java.lib.types.ABSUnit shutDownClient(){ __ABS_checkSameCOG(); 
 {
      client.requestShutDown();
      client.becomesState(new End__Constructor());
   return ABSUnit.UNIT;
   }
}// /home/jan/svn/hats/CaseStudies/models/fredhopper/replication/abs/Clients.abs:130:2: 
   public abs.backend.java.lib.types.ABSBool registerReplicationItems(abs.backend.java.lib.types.ABSInteger checkpoint){ __ABS_checkSameCOG(); 
 {
      return db.prepareReplicationItem(checkpoint);
   }
}// /home/jan/svn/hats/CaseStudies/models/fredhopper/replication/abs/Clients.abs:138:2: 
   public abs.backend.java.lib.types.ABSInteger processFile(abs.backend.java.lib.types.ABSInteger id){ __ABS_checkSameCOG(); 
 {
      abs.backend.java.lib.types.ABSInteger result = abs.backend.java.lib.types.ABSInteger.fromString("0");
      Set fids = new EmptySet__Constructor();
      fids = db.listFiles();
      if (contains__Function.<abs.backend.java.lib.types.ABSInteger>apply(fids, id).toBoolean()) {
         abs.backend.java.lib.runtime.ABSFut size = null;
         size = abs.backend.java.lib.runtime.ABSRuntime.asyncCall(new abs.backend.java.lib.runtime.Task<ClientDataBase>(this,db) {abs.backend.java.lib.types.ABSInteger arg0; protected ABSValue[] getArgs() { return new ABSValue[] { arg0 }; }     public abs.backend.java.lib.runtime.Task<?> init(abs.backend.java.lib.types.ABSInteger _arg0) {arg0 = _arg0; return this; } public java.lang.String methodName() { return "getLength"; } public Object execute() { return target.getLength(arg0); }}
     .init(id));
         abs.backend.java.lib.runtime.ABSRuntime.await(new abs.backend.java.lib.runtime.ABSFutureGuard(size));
         result = (abs.backend.java.lib.types.ABSInteger)size.get();
      }
      return result;
   }
}// /home/jan/svn/hats/CaseStudies/models/fredhopper/replication/abs/Clients.abs:151:2: 
   public abs.backend.java.lib.types.ABSUnit overwrite(Pair file){ __ABS_checkSameCOG(); 
 {
      abs.backend.java.lib.runtime.ABSFut u = null;
      abs.backend.java.lib.types.ABSInteger id = fst__Function.<abs.backend.java.lib.types.ABSInteger,abs.backend.java.lib.types.ABSInteger>apply(file);
      abs.backend.java.lib.types.ABSInteger size = snd__Function.<abs.backend.java.lib.types.ABSInteger,abs.backend.java.lib.types.ABSInteger>apply(file);
      u = abs.backend.java.lib.runtime.ABSRuntime.asyncCall(new abs.backend.java.lib.runtime.Task<ClientDataBase>(this,db) {abs.backend.java.lib.types.ABSInteger arg0;abs.backend.java.lib.types.ABSInteger arg1; protected ABSValue[] getArgs() { return new ABSValue[] { arg0,arg1 }; }     public abs.backend.java.lib.runtime.Task<?> init(abs.backend.java.lib.types.ABSInteger _arg0,abs.backend.java.lib.types.ABSInteger _arg1) {arg0 = _arg0;arg1 = _arg1; return this; } public java.lang.String methodName() { return "updateFile"; } public Object execute() { return target.updateFile(arg0,arg1); }}
     .init(id, size));
      abs.backend.java.lib.runtime.ABSRuntime.await(new abs.backend.java.lib.runtime.ABSFutureGuard(u));
   return ABSUnit.UNIT;
   }
}// /home/jan/svn/hats/CaseStudies/models/fredhopper/replication/abs/Clients.abs:159:2: 
   public abs.backend.java.lib.types.ABSUnit continue__(Pair file){ __ABS_checkSameCOG(); 
 {
      abs.backend.java.lib.runtime.ABSFut u = null;
      abs.backend.java.lib.runtime.ABSFut s = null;
      abs.backend.java.lib.types.ABSInteger id = fst__Function.<abs.backend.java.lib.types.ABSInteger,abs.backend.java.lib.types.ABSInteger>apply(file);
      abs.backend.java.lib.types.ABSInteger size = snd__Function.<abs.backend.java.lib.types.ABSInteger,abs.backend.java.lib.types.ABSInteger>apply(file);
      abs.backend.java.lib.types.ABSInteger fsize = abs.backend.java.lib.types.ABSInteger.fromString("1").negate();
      s = abs.backend.java.lib.runtime.ABSRuntime.asyncCall(new abs.backend.java.lib.runtime.Task<ClientDataBase>(this,db) {abs.backend.java.lib.types.ABSInteger arg0; protected ABSValue[] getArgs() { return new ABSValue[] { arg0 }; }     public abs.backend.java.lib.runtime.Task<?> init(abs.backend.java.lib.types.ABSInteger _arg0) {arg0 = _arg0; return this; } public java.lang.String methodName() { return "getLength"; } public Object execute() { return target.getLength(arg0); }}
     .init(fst__Function.<abs.backend.java.lib.types.ABSInteger,abs.backend.java.lib.types.ABSInteger>apply(file)));
      abs.backend.java.lib.runtime.ABSRuntime.await(new abs.backend.java.lib.runtime.ABSFutureGuard(s));
      fsize = (abs.backend.java.lib.types.ABSInteger)s.get();
      size = size.add(fsize);
      u = abs.backend.java.lib.runtime.ABSRuntime.asyncCall(new abs.backend.java.lib.runtime.Task<ClientDataBase>(this,db) {abs.backend.java.lib.types.ABSInteger arg0;abs.backend.java.lib.types.ABSInteger arg1; protected ABSValue[] getArgs() { return new ABSValue[] { arg0,arg1 }; }     public abs.backend.java.lib.runtime.Task<?> init(abs.backend.java.lib.types.ABSInteger _arg0,abs.backend.java.lib.types.ABSInteger _arg1) {arg0 = _arg0;arg1 = _arg1; return this; } public java.lang.String methodName() { return "updateFile"; } public Object execute() { return target.updateFile(arg0,arg1); }}
     .init(id, size));
      abs.backend.java.lib.runtime.ABSRuntime.await(new abs.backend.java.lib.runtime.ABSFutureGuard(u));
   return ABSUnit.UNIT;
   }
}// /home/jan/svn/hats/CaseStudies/models/fredhopper/replication/abs/Clients.abs:176:2: 
   public abs.backend.java.lib.types.ABSUnit processContent(Pair file){ __ABS_checkSameCOG(); 
 {
      abs.backend.java.lib.runtime.ABSRuntime.await(new abs.backend.java.lib.runtime.ABSExpGuard() { public ABSBool evaluateExp() { return isAppendCommand__Function.apply(command); }});
      if (command.eq(new SkipFile__Constructor()).toBoolean()) {
         ;
      }
      else      if (command.eq(new OverwriteFile__Constructor()).toBoolean()) {
         this.overwrite(file);
      }
      else      if (command.eq(new ContinueFile__Constructor()).toBoolean()) {
         this.continue__(file);
      }
   return ABSUnit.UNIT;
   }
}// /home/jan/svn/hats/CaseStudies/models/fredhopper/replication/abs/Clients.abs:187:2: 
   public abs.backend.java.lib.types.ABSUnit command(Command c){ __ABS_checkSameCOG(); 
 {
      command = c;
   return ABSUnit.UNIT;
   }
}// /home/jan/svn/hats/CaseStudies/models/fredhopper/replication/abs/Clients.abs:189:2: 
   public abs.backend.java.lib.types.ABSUnit receiveSchedule(){ __ABS_checkSameCOG(); 
 {
      hasSchedule = abs.backend.java.lib.types.ABSBool.fromBoolean(true);
   return ABSUnit.UNIT;
   }
}}
abstract class State extends ABSDataType {
}
class Start__Constructor extends State {
   public Start__Constructor() {
   }
   protected ABSValue[] getArgs() { return new ABSValue[] {};}
   public java.lang.String getConstructorName() { return "Start" ;} 
   public ABSBool eq(ABSValue o) {
      if (! (o instanceof Start__Constructor)) return ABSBool.FALSE;
      Start__Constructor other = (Start__Constructor) o;
      return ABSBool.TRUE;
   }
   public boolean match(abs.backend.java.lib.expr.PatternConstructor c, abs.backend.java.lib.expr.PatternBinding b) {
       if (!c.constructorName.equals("Start")) return false;
      return true;
   }
}
class WaitToBoot__Constructor extends State {
   public WaitToBoot__Constructor() {
   }
   protected ABSValue[] getArgs() { return new ABSValue[] {};}
   public java.lang.String getConstructorName() { return "WaitToBoot" ;} 
   public ABSBool eq(ABSValue o) {
      if (! (o instanceof WaitToBoot__Constructor)) return ABSBool.FALSE;
      WaitToBoot__Constructor other = (WaitToBoot__Constructor) o;
      return ABSBool.TRUE;
   }
   public boolean match(abs.backend.java.lib.expr.PatternConstructor c, abs.backend.java.lib.expr.PatternBinding b) {
       if (!c.constructorName.equals("WaitToBoot")) return false;
      return true;
   }
}
class Booting__Constructor extends State {
   public Booting__Constructor() {
   }
   protected ABSValue[] getArgs() { return new ABSValue[] {};}
   public java.lang.String getConstructorName() { return "Booting" ;} 
   public ABSBool eq(ABSValue o) {
      if (! (o instanceof Booting__Constructor)) return ABSBool.FALSE;
      Booting__Constructor other = (Booting__Constructor) o;
      return ABSBool.TRUE;
   }
   public boolean match(abs.backend.java.lib.expr.PatternConstructor c, abs.backend.java.lib.expr.PatternBinding b) {
       if (!c.constructorName.equals("Booting")) return false;
      return true;
   }
}
class WaitToReplicate__Constructor extends State {
   public WaitToReplicate__Constructor() {
   }
   protected ABSValue[] getArgs() { return new ABSValue[] {};}
   public java.lang.String getConstructorName() { return "WaitToReplicate" ;} 
   public ABSBool eq(ABSValue o) {
      if (! (o instanceof WaitToReplicate__Constructor)) return ABSBool.FALSE;
      WaitToReplicate__Constructor other = (WaitToReplicate__Constructor) o;
      return ABSBool.TRUE;
   }
   public boolean match(abs.backend.java.lib.expr.PatternConstructor c, abs.backend.java.lib.expr.PatternBinding b) {
       if (!c.constructorName.equals("WaitToReplicate")) return false;
      return true;
   }
}
class WorkOnReplicate__Constructor extends State {
   public WorkOnReplicate__Constructor() {
   }
   protected ABSValue[] getArgs() { return new ABSValue[] {};}
   public java.lang.String getConstructorName() { return "WorkOnReplicate" ;} 
   public ABSBool eq(ABSValue o) {
      if (! (o instanceof WorkOnReplicate__Constructor)) return ABSBool.FALSE;
      WorkOnReplicate__Constructor other = (WorkOnReplicate__Constructor) o;
      return ABSBool.TRUE;
   }
   public boolean match(abs.backend.java.lib.expr.PatternConstructor c, abs.backend.java.lib.expr.PatternBinding b) {
       if (!c.constructorName.equals("WorkOnReplicate")) return false;
      return true;
   }
}
class End__Constructor extends State {
   public End__Constructor() {
   }
   protected ABSValue[] getArgs() { return new ABSValue[] {};}
   public java.lang.String getConstructorName() { return "End" ;} 
   public ABSBool eq(ABSValue o) {
      if (! (o instanceof End__Constructor)) return ABSBool.FALSE;
      End__Constructor other = (End__Constructor) o;
      return ABSBool.TRUE;
   }
   public boolean match(abs.backend.java.lib.expr.PatternConstructor c, abs.backend.java.lib.expr.PatternBinding b) {
       if (!c.constructorName.equals("End")) return false;
      return true;
   }
}
abstract class Command extends ABSDataType {
}
class StartSnapShot__Constructor extends Command {
   public StartSnapShot__Constructor() {
   }
   protected ABSValue[] getArgs() { return new ABSValue[] {};}
   public java.lang.String getConstructorName() { return "StartSnapShot" ;} 
   public ABSBool eq(ABSValue o) {
      if (! (o instanceof StartSnapShot__Constructor)) return ABSBool.FALSE;
      StartSnapShot__Constructor other = (StartSnapShot__Constructor) o;
      return ABSBool.TRUE;
   }
   public boolean match(abs.backend.java.lib.expr.PatternConstructor c, abs.backend.java.lib.expr.PatternBinding b) {
       if (!c.constructorName.equals("StartSnapShot")) return false;
      return true;
   }
}
class EndSnapShot__Constructor extends Command {
   public EndSnapShot__Constructor() {
   }
   protected ABSValue[] getArgs() { return new ABSValue[] {};}
   public java.lang.String getConstructorName() { return "EndSnapShot" ;} 
   public ABSBool eq(ABSValue o) {
      if (! (o instanceof EndSnapShot__Constructor)) return ABSBool.FALSE;
      EndSnapShot__Constructor other = (EndSnapShot__Constructor) o;
      return ABSBool.TRUE;
   }
   public boolean match(abs.backend.java.lib.expr.PatternConstructor c, abs.backend.java.lib.expr.PatternBinding b) {
       if (!c.constructorName.equals("EndSnapShot")) return false;
      return true;
   }
}
class ListSchedule__Constructor extends Command {
   public ListSchedule__Constructor() {
   }
   protected ABSValue[] getArgs() { return new ABSValue[] {};}
   public java.lang.String getConstructorName() { return "ListSchedule" ;} 
   public ABSBool eq(ABSValue o) {
      if (! (o instanceof ListSchedule__Constructor)) return ABSBool.FALSE;
      ListSchedule__Constructor other = (ListSchedule__Constructor) o;
      return ABSBool.TRUE;
   }
   public boolean match(abs.backend.java.lib.expr.PatternConstructor c, abs.backend.java.lib.expr.PatternBinding b) {
       if (!c.constructorName.equals("ListSchedule")) return false;
      return true;
   }
}
class SearchSchedule__Constructor extends Command {
   public SearchSchedule__Constructor() {
   }
   protected ABSValue[] getArgs() { return new ABSValue[] {};}
   public java.lang.String getConstructorName() { return "SearchSchedule" ;} 
   public ABSBool eq(ABSValue o) {
      if (! (o instanceof SearchSchedule__Constructor)) return ABSBool.FALSE;
      SearchSchedule__Constructor other = (SearchSchedule__Constructor) o;
      return ABSBool.TRUE;
   }
   public boolean match(abs.backend.java.lib.expr.PatternConstructor c, abs.backend.java.lib.expr.PatternBinding b) {
       if (!c.constructorName.equals("SearchSchedule")) return false;
      return true;
   }
}
class EndSearchFile__Constructor extends Command {
   public EndSearchFile__Constructor() {
   }
   protected ABSValue[] getArgs() { return new ABSValue[] {};}
   public java.lang.String getConstructorName() { return "EndSearchFile" ;} 
   public ABSBool eq(ABSValue o) {
      if (! (o instanceof EndSearchFile__Constructor)) return ABSBool.FALSE;
      EndSearchFile__Constructor other = (EndSearchFile__Constructor) o;
      return ABSBool.TRUE;
   }
   public boolean match(abs.backend.java.lib.expr.PatternConstructor c, abs.backend.java.lib.expr.PatternBinding b) {
       if (!c.constructorName.equals("EndSearchFile")) return false;
      return true;
   }
}
class AppendSearchFile__Constructor extends Command {
   public AppendSearchFile__Constructor() {
   }
   protected ABSValue[] getArgs() { return new ABSValue[] {};}
   public java.lang.String getConstructorName() { return "AppendSearchFile" ;} 
   public ABSBool eq(ABSValue o) {
      if (! (o instanceof AppendSearchFile__Constructor)) return ABSBool.FALSE;
      AppendSearchFile__Constructor other = (AppendSearchFile__Constructor) o;
      return ABSBool.TRUE;
   }
   public boolean match(abs.backend.java.lib.expr.PatternConstructor c, abs.backend.java.lib.expr.PatternBinding b) {
       if (!c.constructorName.equals("AppendSearchFile")) return false;
      return true;
   }
}
class SkipFile__Constructor extends Command {
   public SkipFile__Constructor() {
   }
   protected ABSValue[] getArgs() { return new ABSValue[] {};}
   public java.lang.String getConstructorName() { return "SkipFile" ;} 
   public ABSBool eq(ABSValue o) {
      if (! (o instanceof SkipFile__Constructor)) return ABSBool.FALSE;
      SkipFile__Constructor other = (SkipFile__Constructor) o;
      return ABSBool.TRUE;
   }
   public boolean match(abs.backend.java.lib.expr.PatternConstructor c, abs.backend.java.lib.expr.PatternBinding b) {
       if (!c.constructorName.equals("SkipFile")) return false;
      return true;
   }
}
class ContinueFile__Constructor extends Command {
   public ContinueFile__Constructor() {
   }
   protected ABSValue[] getArgs() { return new ABSValue[] {};}
   public java.lang.String getConstructorName() { return "ContinueFile" ;} 
   public ABSBool eq(ABSValue o) {
      if (! (o instanceof ContinueFile__Constructor)) return ABSBool.FALSE;
      ContinueFile__Constructor other = (ContinueFile__Constructor) o;
      return ABSBool.TRUE;
   }
   public boolean match(abs.backend.java.lib.expr.PatternConstructor c, abs.backend.java.lib.expr.PatternBinding b) {
       if (!c.constructorName.equals("ContinueFile")) return false;
      return true;
   }
}
class OverwriteFile__Constructor extends Command {
   public OverwriteFile__Constructor() {
   }
   protected ABSValue[] getArgs() { return new ABSValue[] {};}
   public java.lang.String getConstructorName() { return "OverwriteFile" ;} 
   public ABSBool eq(ABSValue o) {
      if (! (o instanceof OverwriteFile__Constructor)) return ABSBool.FALSE;
      OverwriteFile__Constructor other = (OverwriteFile__Constructor) o;
      return ABSBool.TRUE;
   }
   public boolean match(abs.backend.java.lib.expr.PatternConstructor c, abs.backend.java.lib.expr.PatternBinding b) {
       if (!c.constructorName.equals("OverwriteFile")) return false;
      return true;
   }
}
abstract class JobType extends ABSDataType {
}
class Replication__Constructor extends JobType {
   public Replication__Constructor() {
   }
   protected ABSValue[] getArgs() { return new ABSValue[] {};}
   public java.lang.String getConstructorName() { return "Replication" ;} 
   public ABSBool eq(ABSValue o) {
      if (! (o instanceof Replication__Constructor)) return ABSBool.FALSE;
      Replication__Constructor other = (Replication__Constructor) o;
      return ABSBool.TRUE;
   }
   public boolean match(abs.backend.java.lib.expr.PatternConstructor c, abs.backend.java.lib.expr.PatternBinding b) {
       if (!c.constructorName.equals("Replication")) return false;
      return true;
   }
}
class Boot__Constructor extends JobType {
   public Boot__Constructor() {
   }
   protected ABSValue[] getArgs() { return new ABSValue[] {};}
   public java.lang.String getConstructorName() { return "Boot" ;} 
   public ABSBool eq(ABSValue o) {
      if (! (o instanceof Boot__Constructor)) return ABSBool.FALSE;
      Boot__Constructor other = (Boot__Constructor) o;
      return ABSBool.TRUE;
   }
   public boolean match(abs.backend.java.lib.expr.PatternConstructor c, abs.backend.java.lib.expr.PatternBinding b) {
       if (!c.constructorName.equals("Boot")) return false;
      return true;
   }
}
interface Commandee extends abs.backend.java.lib.types.ABSInterface {
// /home/jan/svn/hats/CaseStudies/models/fredhopper/replication/abs/DataTypes.abs:38:2: 
   public abs.backend.java.lib.types.ABSUnit command(Command command);
}
final class fsts__Function implements ABSFunction {
   private fsts__Function() { }
   public static <A extends abs.backend.java.lib.types.ABSValue,B extends abs.backend.java.lib.types.ABSValue>Set apply(Set ps) {
      return new Case() {
      public Set of(final Set ps, final Set __ABS_value) { 
final abs.backend.java.lib.expr.PatternBinding __ABS_binding0 = new abs.backend.java.lib.expr.PatternConstructor("EmptySet").match(__ABS_value);
if (__ABS_binding0 != null) return new Object() {
  public Set execute() { return new EmptySet__Constructor(); }}.execute();
final abs.backend.java.lib.expr.PatternBinding __ABS_binding1 = new abs.backend.java.lib.expr.PatternConstructor("Insert",new abs.backend.java.lib.expr.PatternVariable("x"),new abs.backend.java.lib.expr.PatternVariable("xs")).match(__ABS_value);
if (__ABS_binding1 != null) return new Object() {
  public Set execute(final Pair x,final Set xs) { return new Insert__Constructor(fst__Function.<A,B>apply(x), fsts__Function.<A,B>apply(xs)); }}.execute((Pair) __ABS_binding1.getBinding(0),(Set) __ABS_binding1.getBinding(1));
throw new abs.backend.java.lib.expr.UnmatchedCaseException("/home/jan/svn/hats/CaseStudies/models/fredhopper/replication/abs/DataTypes.abs:43:3:  value "+__ABS_value+" did not match any pattern.");
}}.of(ps, ps);
   }
}
final class snds__Function implements ABSFunction {
   private snds__Function() { }
   public static <A extends abs.backend.java.lib.types.ABSValue,B extends abs.backend.java.lib.types.ABSValue>Set apply(Set ps) {
      return new Case() {
      public Set of(final Set ps, final Set __ABS_value) { 
final abs.backend.java.lib.expr.PatternBinding __ABS_binding0 = new abs.backend.java.lib.expr.PatternConstructor("EmptySet").match(__ABS_value);
if (__ABS_binding0 != null) return new Object() {
  public Set execute() { return new EmptySet__Constructor(); }}.execute();
final abs.backend.java.lib.expr.PatternBinding __ABS_binding1 = new abs.backend.java.lib.expr.PatternConstructor("Insert",new abs.backend.java.lib.expr.PatternVariable("x"),new abs.backend.java.lib.expr.PatternVariable("xs")).match(__ABS_value);
if (__ABS_binding1 != null) return new Object() {
  public Set execute(final Pair x,final Set xs) { return new Insert__Constructor(snd__Function.<A,B>apply(x), snds__Function.<A,B>apply(xs)); }}.execute((Pair) __ABS_binding1.getBinding(0),(Set) __ABS_binding1.getBinding(1));
throw new abs.backend.java.lib.expr.UnmatchedCaseException("/home/jan/svn/hats/CaseStudies/models/fredhopper/replication/abs/DataTypes.abs:49:3:  value "+__ABS_value+" did not match any pattern.");
}}.of(ps, ps);
   }
}
final class isAppendCommand__Function implements ABSFunction {
   private isAppendCommand__Function() { }
   public static abs.backend.java.lib.types.ABSBool apply(Command c) {
      return new Case() {
      public abs.backend.java.lib.types.ABSBool of(final Command c, final Command __ABS_value) { 
final abs.backend.java.lib.expr.PatternBinding __ABS_binding0 = new abs.backend.java.lib.expr.PatternConstructor("SkipFile").match(__ABS_value);
if (__ABS_binding0 != null) return new Object() {
  public abs.backend.java.lib.types.ABSBool execute() { return abs.backend.java.lib.types.ABSBool.fromBoolean(true); }}.execute();
final abs.backend.java.lib.expr.PatternBinding __ABS_binding1 = new abs.backend.java.lib.expr.PatternConstructor("ContinueFile").match(__ABS_value);
if (__ABS_binding1 != null) return new Object() {
  public abs.backend.java.lib.types.ABSBool execute() { return abs.backend.java.lib.types.ABSBool.fromBoolean(true); }}.execute();
final abs.backend.java.lib.expr.PatternBinding __ABS_binding2 = new abs.backend.java.lib.expr.PatternConstructor("OverwriteFile").match(__ABS_value);
if (__ABS_binding2 != null) return new Object() {
  public abs.backend.java.lib.types.ABSBool execute() { return abs.backend.java.lib.types.ABSBool.fromBoolean(true); }}.execute();
final abs.backend.java.lib.expr.PatternBinding __ABS_binding3 = new abs.backend.java.lib.expr.AnyPattern().match(__ABS_value);
if (__ABS_binding3 != null) return new Object() {
  public abs.backend.java.lib.types.ABSBool execute() { return abs.backend.java.lib.types.ABSBool.fromBoolean(false); }}.execute();
throw new abs.backend.java.lib.expr.UnmatchedCaseException("/home/jan/svn/hats/CaseStudies/models/fredhopper/replication/abs/DataTypes.abs:56:2:  value "+__ABS_value+" did not match any pattern.");
}}.of(c, c);
   }
}
interface Node extends abs.backend.java.lib.types.ABSInterface {
// /home/jan/svn/hats/CaseStudies/models/fredhopper/replication/abs/Interfaces.abs:11:2: 
   public DataBase getDataBase();
// /home/jan/svn/hats/CaseStudies/models/fredhopper/replication/abs/Interfaces.abs:12:2: 
   public abs.backend.java.lib.types.ABSBool isShutdownRequested();
// /home/jan/svn/hats/CaseStudies/models/fredhopper/replication/abs/Interfaces.abs:13:2: 
   public abs.backend.java.lib.types.ABSUnit requestShutDown();
}
interface Client extends abs.backend.java.lib.types.ABSInterface, Node {
// /home/jan/svn/hats/CaseStudies/models/fredhopper/replication/abs/Interfaces.abs:18:2: 
   public ClientDataBase getClientDataBase();
}
interface ClientConnector extends abs.backend.java.lib.types.ABSInterface, Client {
// /home/jan/svn/hats/CaseStudies/models/fredhopper/replication/abs/Interfaces.abs:22:2: 
   public abs.backend.java.lib.types.ABSUnit setAcceptor(ServerAcceptor acceptor);
}
interface SyncClient extends abs.backend.java.lib.types.ABSInterface, Client {
// /home/jan/svn/hats/CaseStudies/models/fredhopper/replication/abs/Interfaces.abs:28:2: 
   public ServerAcceptor getAcceptor();
// /home/jan/svn/hats/CaseStudies/models/fredhopper/replication/abs/Interfaces.abs:29:2: 
   public abs.backend.java.lib.types.ABSUnit becomesState(State state);
// /home/jan/svn/hats/CaseStudies/models/fredhopper/replication/abs/Interfaces.abs:30:2: 
   public abs.backend.java.lib.types.ABSUnit incrementJob();
}
interface ClientJob extends abs.backend.java.lib.types.ABSInterface, Commandee {
// /home/jan/svn/hats/CaseStudies/models/fredhopper/replication/abs/Interfaces.abs:37:2: 
   public abs.backend.java.lib.types.ABSBool registerReplicationItems(abs.backend.java.lib.types.ABSInteger checkpoint);
// /home/jan/svn/hats/CaseStudies/models/fredhopper/replication/abs/Interfaces.abs:43:2: 
   public abs.backend.java.lib.types.ABSInteger processFile(abs.backend.java.lib.types.ABSInteger id);
// /home/jan/svn/hats/CaseStudies/models/fredhopper/replication/abs/Interfaces.abs:44:2: 
   public abs.backend.java.lib.types.ABSUnit processContent(Pair file);
// /home/jan/svn/hats/CaseStudies/models/fredhopper/replication/abs/Interfaces.abs:45:2: 
   public abs.backend.java.lib.types.ABSUnit receiveSchedule();
}
interface SyncServerClientCoordinator extends abs.backend.java.lib.types.ABSInterface {
// /home/jan/svn/hats/CaseStudies/models/fredhopper/replication/abs/Interfaces.abs:52:2: 
   public abs.backend.java.lib.types.ABSUnit process();
// /home/jan/svn/hats/CaseStudies/models/fredhopper/replication/abs/Interfaces.abs:53:2: 
   public abs.backend.java.lib.types.ABSUnit startReplicationUpdate(ConnectionThread worker);
// /home/jan/svn/hats/CaseStudies/models/fredhopper/replication/abs/Interfaces.abs:54:2: 
   public abs.backend.java.lib.types.ABSUnit finishReplicationUpdate(ConnectionThread worker);
}
interface ServerAcceptor extends abs.backend.java.lib.types.ABSInterface {
// /home/jan/svn/hats/CaseStudies/models/fredhopper/replication/abs/Interfaces.abs:60:2: 
   public ConnectionThread getConnection(ClientJob job);
}
interface SyncServerAcceptor extends abs.backend.java.lib.types.ABSInterface, ServerAcceptor {
// /home/jan/svn/hats/CaseStudies/models/fredhopper/replication/abs/Interfaces.abs:64:2: 
   public abs.backend.java.lib.types.ABSBool isAcceptingConnection();
// /home/jan/svn/hats/CaseStudies/models/fredhopper/replication/abs/Interfaces.abs:65:2: 
   public abs.backend.java.lib.types.ABSUnit suspendConnection();
// /home/jan/svn/hats/CaseStudies/models/fredhopper/replication/abs/Interfaces.abs:66:2: 
   public abs.backend.java.lib.types.ABSUnit resumingConnection();
}
interface ConnectionThread extends abs.backend.java.lib.types.ABSInterface, Commandee {
}
interface SyncServer extends abs.backend.java.lib.types.ABSInterface, Node {
// /home/jan/svn/hats/CaseStudies/models/fredhopper/replication/abs/Interfaces.abs:78:2: 
   public abs.backend.java.lib.types.ABSUnit refreshSnapShot();
// /home/jan/svn/hats/CaseStudies/models/fredhopper/replication/abs/Interfaces.abs:79:2: 
   public abs.backend.java.lib.types.ABSUnit clearSnapShot();
// /home/jan/svn/hats/CaseStudies/models/fredhopper/replication/abs/Interfaces.abs:80:2: 
   public Set getItems();
// /home/jan/svn/hats/CaseStudies/models/fredhopper/replication/abs/Interfaces.abs:81:2: 
   public SyncServerAcceptor getAcceptor();
// /home/jan/svn/hats/CaseStudies/models/fredhopper/replication/abs/Interfaces.abs:82:2: 
   public SyncServerClientCoordinator getCoordinator();
}
interface DataBase extends abs.backend.java.lib.types.ABSInterface {
// /home/jan/svn/hats/CaseStudies/models/fredhopper/replication/abs/DataBases.abs:12:3: 
   public abs.backend.java.lib.types.ABSInteger getLength(abs.backend.java.lib.types.ABSInteger fId);
// /home/jan/svn/hats/CaseStudies/models/fredhopper/replication/abs/DataBases.abs:13:4: 
   public Set listFiles();
}
interface ServerDataBase extends abs.backend.java.lib.types.ABSInterface, DataBase {
// /home/jan/svn/hats/CaseStudies/models/fredhopper/replication/abs/DataBases.abs:23:4: 
   public abs.backend.java.lib.types.ABSBool refresh();
// /home/jan/svn/hats/CaseStudies/models/fredhopper/replication/abs/DataBases.abs:24:4: 
   public Set listCheckPointFiles();
}
interface ClientDataBase extends abs.backend.java.lib.types.ABSInterface, DataBase {
// /home/jan/svn/hats/CaseStudies/models/fredhopper/replication/abs/DataBases.abs:34:4: 
   public abs.backend.java.lib.types.ABSBool prepareReplicationItem(abs.backend.java.lib.types.ABSInteger cp);
// /home/jan/svn/hats/CaseStudies/models/fredhopper/replication/abs/DataBases.abs:35:4: 
   public abs.backend.java.lib.types.ABSUnit updateFile(abs.backend.java.lib.types.ABSInteger fId, abs.backend.java.lib.types.ABSInteger size);
}
// /home/jan/svn/hats/CaseStudies/models/fredhopper/replication/abs/DataBases.abs:43:1: 
class DataBaseImpl extends abs.backend.java.lib.runtime.ABSObject implements abs.backend.java.lib.types.ABSClass, ServerDataBase, ClientDataBase {
   private Map db;
// /home/jan/svn/hats/CaseStudies/models/fredhopper/replication/abs/DataBases.abs:46:2: 
   private abs.backend.java.lib.types.ABSInteger ccp = abs.backend.java.lib.types.ABSInteger.fromString("1").negate();
// /home/jan/svn/hats/CaseStudies/models/fredhopper/replication/abs/DataBases.abs:47:2: 
   private Set checkPoints = keys__Function.<abs.backend.java.lib.types.ABSInteger,Map>apply(db);
// /home/jan/svn/hats/CaseStudies/models/fredhopper/replication/abs/DataBases.abs:50:2: 
   private Map filestore = new EmptyMap__Constructor();
   public DataBaseImpl(Map db) {
      this.db = db;
       getCOG().objectCreated(this);
   }
   protected ABSValue getFieldValue(java.lang.String __ABS_fieldName) throws java.lang.NoSuchFieldException {
   if ("db".equals(__ABS_fieldName)) return db;
   if ("ccp".equals(__ABS_fieldName)) return ccp;
   if ("checkPoints".equals(__ABS_fieldName)) return checkPoints;
   if ("filestore".equals(__ABS_fieldName)) return filestore;
       return super.getFieldValue(__ABS_fieldName);
   }
   public java.lang.String getClassName() { return "DataBaseImpl"; }
   public static DataBaseImpl __ABS_createNewCOG(Map db) {
       final abs.backend.java.lib.runtime.COG __ABS_cog = new abs.backend.java.lib.runtime.COG(DataBaseImpl.class);
       final abs.backend.java.lib.runtime.ABSThread __ABS_thread = abs.backend.java.lib.runtime.ABSRuntime.getCurrentThread();
       final abs.backend.java.lib.runtime.COG __ABS_oldCOG = abs.backend.java.lib.runtime.ABSRuntime.getCurrentCOG();
       __ABS_thread.setCOG(__ABS_cog);
       try { 
            DataBaseImpl __ABS_result = new DataBaseImpl(db);
          abs.backend.java.lib.runtime.ABSRuntime.cogCreated(__ABS_result);
          return __ABS_result;
       } finally {
           __ABS_thread.setCOG(__ABS_oldCOG);
       }
   }
// /home/jan/svn/hats/CaseStudies/models/fredhopper/replication/abs/DataBases.abs:52:2: 
   public abs.backend.java.lib.types.ABSBool prepareReplicationItem(abs.backend.java.lib.types.ABSInteger p){ __ABS_checkSameCOG(); 
 {
      abs.backend.java.lib.types.ABSBool result = abs.backend.java.lib.types.ABSBool.fromBoolean(false);
      if (contains__Function.<abs.backend.java.lib.types.ABSInteger>apply(checkPoints, p).negate().toBoolean()) {
         checkPoints = new Insert__Constructor(p, checkPoints);
         ccp = p;
         db = new InsertAssoc__Constructor(new Pair__Constructor(p, new EmptyMap__Constructor()), db);
         result = abs.backend.java.lib.types.ABSBool.fromBoolean(true);
      }
      return result;
   }
}// /home/jan/svn/hats/CaseStudies/models/fredhopper/replication/abs/DataBases.abs:63:2: 
   public abs.backend.java.lib.types.ABSBool advancedCheckPoint(){ __ABS_checkSameCOG(); 
 {
      abs.backend.java.lib.types.ABSBool result = abs.backend.java.lib.types.ABSBool.fromBoolean(false);
      if (hasNext__Function.<abs.backend.java.lib.types.ABSInteger>apply(checkPoints).toBoolean()) {
         Pair nt = next__Function.<abs.backend.java.lib.types.ABSInteger>apply(checkPoints);
         checkPoints = fst__Function.<Set,abs.backend.java.lib.types.ABSInteger>apply(nt);
         ccp = snd__Function.<Set,abs.backend.java.lib.types.ABSInteger>apply(nt);
         result = abs.backend.java.lib.types.ABSBool.fromBoolean(true);
      }
      return result;
   }
}// /home/jan/svn/hats/CaseStudies/models/fredhopper/replication/abs/DataBases.abs:75:2: 
   public abs.backend.java.lib.types.ABSBool refresh(){ __ABS_checkSameCOG(); 
 {
      abs.backend.java.lib.types.ABSBool more = abs.backend.java.lib.types.ABSBool.fromBoolean(false);
      Map updates = new EmptyMap__Constructor();
      Set fids = new EmptySet__Constructor();
      more = this.advancedCheckPoint();
      if (more.toBoolean()) {
         updates = lookup__Function.<abs.backend.java.lib.types.ABSInteger,Map>apply(db, ccp);
         fids = keys__Function.<abs.backend.java.lib.types.ABSInteger,abs.backend.java.lib.types.ABSInteger>apply(updates);
         while (hasNext__Function.<abs.backend.java.lib.types.ABSInteger>apply(fids).toBoolean()) {
            abs.backend.java.lib.types.ABSInteger id = abs.backend.java.lib.types.ABSInteger.fromString("1").negate();
            abs.backend.java.lib.types.ABSInteger fs = abs.backend.java.lib.types.ABSInteger.fromString("1").negate();
            Pair nids = next__Function.<abs.backend.java.lib.types.ABSInteger>apply(fids);
            fids = fst__Function.<Set,abs.backend.java.lib.types.ABSInteger>apply(nids);
            id = snd__Function.<Set,abs.backend.java.lib.types.ABSInteger>apply(nids);
            fs = lookup__Function.<abs.backend.java.lib.types.ABSInteger,abs.backend.java.lib.types.ABSInteger>apply(updates, id);
            filestore = put__Function.<abs.backend.java.lib.types.ABSInteger,abs.backend.java.lib.types.ABSInteger>apply(filestore, id, fs);
         }
      }
      return more;
   }
}// /home/jan/svn/hats/CaseStudies/models/fredhopper/replication/abs/DataBases.abs:101:2: 
   public abs.backend.java.lib.types.ABSInteger getLength(abs.backend.java.lib.types.ABSInteger fId){ __ABS_checkSameCOG(); 
 {
      return lookupDefault__Function.<abs.backend.java.lib.types.ABSInteger,abs.backend.java.lib.types.ABSInteger>apply(filestore, fId, abs.backend.java.lib.types.ABSInteger.fromString("0"));
   }
}// /home/jan/svn/hats/CaseStudies/models/fredhopper/replication/abs/DataBases.abs:106:2: 
   public abs.backend.java.lib.types.ABSUnit storeFile(abs.backend.java.lib.types.ABSInteger fId, abs.backend.java.lib.types.ABSInteger size){ __ABS_checkSameCOG(); 
 {
      filestore = put__Function.<abs.backend.java.lib.types.ABSInteger,abs.backend.java.lib.types.ABSInteger>apply(filestore, fId, size);
   return ABSUnit.UNIT;
   }
}// /home/jan/svn/hats/CaseStudies/models/fredhopper/replication/abs/DataBases.abs:110:2: 
   public Map checkPointFiles(){ __ABS_checkSameCOG(); 
 {
      return lookupDefault__Function.<abs.backend.java.lib.types.ABSInteger,Map>apply(db, ccp, new EmptyMap__Constructor());
   }
}// /home/jan/svn/hats/CaseStudies/models/fredhopper/replication/abs/DataBases.abs:115:2: 
   public abs.backend.java.lib.types.ABSUnit updateFile(abs.backend.java.lib.types.ABSInteger fId, abs.backend.java.lib.types.ABSInteger size){ __ABS_checkSameCOG(); 
 {
      Map checkPointFiles = new EmptyMap__Constructor();
      checkPointFiles = this.checkPointFiles();
      checkPointFiles = put__Function.<abs.backend.java.lib.types.ABSInteger,abs.backend.java.lib.types.ABSInteger>apply(checkPointFiles, fId, size);
      db = put__Function.<abs.backend.java.lib.types.ABSInteger,Map>apply(db, ccp, checkPointFiles);
      this.storeFile(fId, size);
   return ABSUnit.UNIT;
   }
}// /home/jan/svn/hats/CaseStudies/models/fredhopper/replication/abs/DataBases.abs:123:2: 
   public Set listCheckPointFiles(){ __ABS_checkSameCOG(); 
 {
      Map checkPointFiles = new EmptyMap__Constructor();
      checkPointFiles = this.checkPointFiles();
      return keys__Function.<abs.backend.java.lib.types.ABSInteger,abs.backend.java.lib.types.ABSInteger>apply(checkPointFiles);
   }
}// /home/jan/svn/hats/CaseStudies/models/fredhopper/replication/abs/DataBases.abs:129:2: 
   public Set listFiles(){ __ABS_checkSameCOG(); 
 {
      return keys__Function.<abs.backend.java.lib.types.ABSInteger,abs.backend.java.lib.types.ABSInteger>apply(filestore);
   }
}}
