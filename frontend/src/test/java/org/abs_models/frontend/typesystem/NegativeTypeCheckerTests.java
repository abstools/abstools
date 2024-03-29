/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved.
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.abs_models.frontend.typesystem;

import static org.junit.Assert.assertEquals;

import org.abs_models.frontend.FrontendTest;
import org.abs_models.frontend.analyser.ErrorMessage;
import org.abs_models.frontend.ast.Model;
import org.junit.Test;

public class NegativeTypeCheckerTests extends FrontendTest {

    // NEGATIVE TESTS

    @Test
    public void testNull() {
        assertTypeErrors("{ Unit u = null; } ");
    }

    @Test
    public void varDeclAssign() {
        assertTypeErrors("{ Unit u = True; } ");
    }

    @Test
    public void duplicateVarDecls() {
        assertTypeErrors("{ Unit u = Unit; Unit u = Unit; }");
    }

    @Test
    public void duplicateVarDeclsParams() {
        assertTypeErrors("class C { Unit m(Unit u) { Unit u = Unit; } }");
    }

    @Test
    public void varDelcaredLater() {
        assertTypeErrors("{ Unit u = f; Unit f = Unit; }");
    }

    @Test
    public void testUnresolvableType() {
        assertTypeErrors("{ I i; }");
    }

    @Test
    public void testUnresolvableType2() {
        assertTypeErrors("class C implements I { }");
    }

    @Test
    public void testUnresolvableType3() {
        assertTypeErrors("interface J extends I { }");
    }

    @Test
    public void missingInterface() {
        assertTypeErrors("class C implements I { } { I i; i = new local C(); }");
    }

    @Test
    public void parametricDataTypesOK() {
        assertTypeOK("interface I {} interface J extends I {} data Foo<A> = Bar(A); { J j; Foo<I> f = Bar(j); }");
    }

    @Test
    public void parametricDataTypeNoTypeArgs() {
        assertTypeErrors("data Foo<A> = Bar(A) | Nul; { Foo f = Nul; }");
    }


    @Test
    public void parametricDataTypeNoTypeArgs2() {
        assertTypeErrors("class Test { { Pair p = Pair(5,Pair(4,5)); } }");
    }

    @Test
    public void parametricDataTypeNoTypeArgs3() {
        assertTypeOK("type Euro = Int; type Cent = Int; type Money = Pair<Euro,Cent>;"
                + "def Money createMoney(Euro e, Cent c) = Pair(e,c); ");
    }

    @Test
    public void parametricDataTypeNoTypeArgs4() {
        assertTypeErrors("data Foo<A> = Bar(A) | Nul; type Foo2 = Foo; { Foo2 f = Nul; }");
    }

    @Test
    public void parametricDataTypeIndirect() {
        assertTypeErrors("data Foo<A> = Bar(A); type Foo2 = Foo<String>; { Foo2 f = Bar(2); }");
    }

    @Test
    public void parametricDataTypeShadowingParameter() {
        assertTypeErrors("data Foo<Int> = Bar(Int);");
    }

    @Test
    public void testClassError() {
        assertTypeErrors("interface I {} interface J{} class C implements J {} { I i; i = new local C(); }");
    }

    @Test
    public void testClassTypeUseError() {
        assertTypeErrors("class C {} { C c; c = new local C(); }");
    }

    @Test
    public void testClassDuplicateFields() {
        assertTypeErrors("class C { Bool b = True; Int b = 5; } ");
    }

    @Test
    public void testClassDuplicateFields2() {
        assertTypeErrors("class C(Int b) { Bool b = True; } ");
    }

    @Test
    public void testClassFieldAccessWithoutThis() {
        assertTypeOK("class C(Int b) { Int c = b; } ");
    }

    @Test
    public void testClassDuplicateMethods() {
        assertTypeErrors("class C { Unit m() {} Bool m() { return True;} } ");
    }

    @Test
    public void functionDuplicateParams() {
        assertTypeErrors("def Bool f(Bool b, Int b) = True; ");
    }

    @Test
    public void functionNestedErrors() {
        assertTypeErrors("def X f(Bool b) = True; ");
        assertTypeErrors("def Bool f(X b) = True; ");
        assertTypeErrors("def Bool f(Bool b) = Foo(); ");
    }

    @Test
    public void functionWrongReturnType() {
        assertTypeErrors("def Bool f() = 5; ");
    }

    @Test
    public void dataTypeDuplicateConstructors() {
        assertTypeErrors("data Foo = Bar | Bar ;");
    }

    @Test
    public void dataTypeNestedErrors() {
        assertTypeErrors("data Foo = Bar(X) ;");
    }

    @Test
    public void dataTypeDuplicateSelectorNames() {
        assertTypeErrors("data Foo = Bar(X x) | Baz(X x) ;");
    }

    @Test
    public void dataTypeSelectorNameLikeFunctionName() {
        assertTypeErrors("data Foo = Bar(X x); def Bool x() = True; ");
    }

    @Test
    public void interfaceDuplicateMethods() {
        assertTypeErrors("interface I { Unit m(); Unit m(); }");
    }

    @Test
    public void methodDuplicateParams() {
        assertTypeErrors("interface I { Unit m(Bool b, Int b); }");
    }

    @Test
    public void interfaceWrongExtend() {
        assertTypeErrors("interface I extends Bool {} ");
    }

    @Test
    public void interfaceCyclicExtend() {
        assertTypeErrors("interface I extends I {} ", ErrorMessage.CYCLIC_INHERITANCE);
        assertTypeErrors("interface I extends J {}  interface J extends I {}", ErrorMessage.CYCLIC_INHERITANCE);
    }

    @Test
    public void interfaceMethodOverride() {
        assertTypeErrors("interface I { Unit m(); } interface J extends I { Unit m(); }");
    }

    @Test
    public void interfaceNestedError() {
        assertTypeErrors("interface I { X m(); }");
    }

    @Test
    public void interfaceNestedParamError() {
        assertTypeErrors("interface I { Unit m(X x); }");
    }

    @Test
    public void interfaceDupParams() {
        assertTypeErrors("interface I { Unit m(Bool b, Int b); }");
    }

    @Test
    public void methodNotImplemented() {
        assertTypeErrors("interface I { Unit m(); }" + "class C implements I {  } ");
    }

    @Test
    public void methodOverrideWrongReturnType() {
        assertTypeErrors("interface I { Unit m(); }" + "class C implements I { Bool m() { return True; } } ");
    }

    @Test
    public void classMethodWrongNumParams() {
        assertTypeErrors("interface I { Unit m(); } class C implements I { Unit m(Bool b) { } }");
    }

    @Test
    public void classMethodWrongParamType() {
        assertTypeErrors("interface I { Unit m(Bool b); } class C implements I { Unit m(Int i) { } }");
    }

    @Test
    public void classDuplicateFields() {
        assertTypeErrors("class C { Bool f; Int f;}");
    }

    @Test
    public void classFieldError() {
        assertTypeErrors("class C { X f; }");
    }

    @Test
    public void classFieldAccess() {
        assertTypeErrors("class C { Bool f = True; Unit m(Int f) { this.f = 5; } }");
    }

    @Test
    public void classInitializerBlockError() {
        assertTypeErrors("class C { { X f; } }");
    }

    // https://github.com/abstools/abstools/issues/198
    @Test
    public void classInitializerInvalidMethodCall() {
        assertTypeErrors("class C { { i.m(); } }");
    }

    @Test
    public void classInitializerInvalidDurationStmt() {
        assertTypeErrors("class C { { duration(1, 1); } }");
    }

    @Test
    public void classInitializerInvalidGetExp() {
        assertTypeErrors("class C { { Fut<Int> f; f.get; } }");
    }

    @Test
    public void classParamsError() {
        assertTypeErrors("class C(I i) { }");
    }

    @Test
    public void negTestError() {
        Model m = assertParse(" { Bool b = !5; }");
        assertEquals(ErrorMessage.EXPECTED_TYPE, m.typeCheck().getFirstError().msg);
    }

    @Test
    public void plusError() {
        assertTypeErrors("{ Int i = 4 + True; }");
    }

    @Test
    public void plusError2() {
        assertTypeErrors("{ Int i = 4 + \"a\"; }");
    }

    @Test
    public void plusError3() {
        // https://github.com/abstools/abstools/issues/202
        assertTypeErrors("{ Int i = 4 + (1/2) + 4; }");
    }

    @Test
    public void getError() {
        assertTypeErrors("{ Bool f = True; f.get; }");
    }

    @Test
    public void orError() {
        assertTypeErrors("{ Bool b = True || 5; }");
    }

    @Test
    public void andError() {
        assertTypeErrors("{ Bool b = 5 && True; }");
    }

    @Test
    public void unknownVarError() {
        // crashed the compiler while trying to work with Unknown types
        assertTypeErrors("def Int s(List<Int> p) = when True then head(s) else head(s);");
    }

    @Test
    public void newError() {
        assertTypeErrors("interface I { } { I i; i = new local I(); }");
    }

    @Test
    public void newError2() {
        assertTypeErrors("interface I { } class C(Bool b) implements I { } { I i; i = new local C(); }");
    }

    @Test
    public void newError3() {
        assertTypeErrors("interface I { } class C(Bool b) implements I { } { I i; i = new local C(5); }");
    }

    @Test
    public void letError() {
        assertTypeErrors("{ Bool b = let (Bool x) = 5 in x; }");
    }

    @Test
    public void caseError() {
        assertTypeErrors("{ Bool x = True; Bool b = case x { True => False; False => 5; }; }");
    }

    @Test
    public void caseErrorNoDataType() {
        assertTypeErrors("interface I { } { I i; Bool b = case i { True => False; False => 5; }; }");
    }

    @Test
    public void caseBoundVarWrongType() {
        assertTypeErrors("def Bool f(Bool x) = let (Int y) = 5 in case x { y => True; };");
    }

    @Test
    public void caseErrorConstructorNotResolvable() {
        assertTypeErrors("{ Bool x = True; Bool b = case x { Foo => False; }; }");
    }

    @Test
    public void caseErrorDuplicateFunctionTypeParameter() {
        assertTypeErrors("def A f<A,A>(A a) = a;");
    }

    @Test
    public void parametricFunctionTypeShadowingParameter() {
        assertTypeErrors("def A f<A,Int>(A a) = a;");
    }

    @Test
    public void parametricFunctionUnboundType() {
        // https://github.com/abstools/abstools/issues/247
        assertTypeErrors("data Err = Err(Int) | NotUsed; def A bottom<A>(Err err) = case err {NotUsed => bottom(Err(5));};");
    }

    @Test
    public void caseErrorDuplicateDatatypeTypeParameter() {
        assertTypeErrors("data X<A, A>;");
    }

    @Test
    public void ticket188() {
        assertTypeErrors(
            "def Bool f(List<Bool> list) ="
           +"    case list {"
           +"       Insert(xyz,_) => xyz;" // Insert is not a List constructor
           +"   };");
    }

    @Test
    public void caseErrorConstructorWrongArgNum() {
        assertTypeErrors("{ Bool x = True; Bool b = case x { True(5) => False; }; }");
    }

    @Test
    public void caseErrorConstructorWrongArgType() {
        assertTypeErrors("data Foo = Bar(Bool);  { Foo x = Bar(True); Bool b = case x { Bar(5) => False; }; }");
    }

    @Test
    public void caseErrorConstructorExpWrongArgType() {
        assertTypeErrors("data Foo = Bar(Bool); { Foo x = Bar(5); }");
    }

    @Test
    public void fnAppMissingDef() {
        assertTypeErrors("def Bool f() = x(); ");
    }

    @Test
    public void fnAppWrongArgNum() {
        assertTypeErrors("def Bool f() = f(True); ");
    }

    @Test
    public void fnAppWrongArgType() {
        assertTypeErrors("def Bool f(Bool b) = f(5); ");
    }

    @Test
    public void fnAppListIncompatibleElement() {
        assertTypeErrors("{ Set<Int> s = set[4, \"hello\"]; }");
    }

    @Test
    public void fnAppListIncompatibleListType() {
        assertTypeErrors("{ Set<Int> s = set[4, 4/3]; }");
    }

    @Test
    public void fnTypecheckNoCrash() {
        assertTypeErrors("def List<String> map2list<A>(Map<String,A> map) =" + "case map {" + "EmptyMap => Nil ;"
                + "Insert(Pair(b,_), tail) => Cons(b, map2list(tail)) ;" + "};");
    }

    @Test
    public void listLiteralUnknownType() {
        assertTypeErrors("{ List<Int> x = list[nth(unknown(), 1), nth(unknown(), 2)]; }");
    }

    @Test
    public void missingConstructorNoSO() {
        /* Frob not visible, gave once an SO */
        assertTypeErrors("type Tuple = Map<Int, Int>;"
                +"def Bool tupleMatches(Tuple tuple, Tuple criterion) ="
                +"case criterion {"
                +"  Frob(pair, rest) =>"
                +"    lookup(tuple, fst(pair)) == Just(snd(pair)) &&"
                +"    tupleMatches(tuple, rest);"
                +"  EmptyMap => True;"
                +"};");
    }

    @Test
    public void methodReturnError() {
        assertTypeErrors("class C { Unit m() { return True; } }");
    }

    @Test
    public void methodMissingReturn() {
        assertTypeErrors("class C { Bool m() {  } }", ErrorMessage.METHOD_IMPL_MISSING_RETURN_STMT);
    }

    @Test
    public void methodReturnNotLastStmt() {
        assertTypeErrors("class C { Bool m() { if (True) return True; else return False; return True;} }");
    }

    @Test
    public void syncCallWrongTarget() {
        assertTypeErrors("class C { Unit m(Bool b) { b.m();  } }");
    }

    @Test
    public void syncCallNoMethodThis() {
        assertTypeErrors("class C { Unit m() { this.n(); } }");
    }

    @Test
    public void syncCallNoMethodIntf() {
        assertTypeErrors("interface I {} { I i; i.m(); }");
    }

    @Test
    public void syncCallWrongArgNum() {
        assertTypeErrors("interface I { Unit m(); } { I i; i.m(True); }");
    }

    @Test
    public void syncCallWrongArgType() {
        assertTypeErrors("interface I { Unit m(Int i); } { I i; i.m(True); }");
    }

    @Test
    public void testVarDeclInitNoSubtypeError() {
        assertTypeErrors("interface I {} interface J {} { J j; I i = j; }");
    }

    @Test
    public void testVarDeclInitMissingError() {
        assertTypeErrors("{ Bool b; }");
    }

    @Test
    public void testIfNoBool() {
        assertTypeErrors("{ if (5) { } else { } }");
    }

    @Test
    public void testWhileNoBool() {
        assertTypeErrors("{ while (5) { } }");
    }

    @Test
    public void testAwaitNoFut() {
        assertTypeErrors("{ Bool b = True; await b?; }");
    }

    @Test
    public void testGetNoFut() {
        assertTypeErrors("{ Either<Unit,Bool> e = Right(True); e.get; }", ErrorMessage.EXPECTED_FUT_TYPE);
    }

    @Test
    public void testAwaitNoBool() {
        assertTypeErrors("{ await 5; }");
    }

    @Test
    public void testAwaitAndError() {
        assertTypeErrors("{ await 5 && True; }");
    }

    @Test
    public void constructorTypeArgsError() {
        assertTypeErrors("data Foo<A> = Bar(A,A); { Foo<A> o = Bar(True,5); }");
    }

    @Test
    public void missingTypArg() {
        assertTypeErrors("def List<A> map2list<A>(Map<A,B> map) = Nil;");
    }

    @Test
    public void returnInMainBlock() {
        assertTypeErrors("{ return Nil; }");
        assertTypeErrors("{ return Unit; return Unit; }");
    }

    @Test
    public void returnInInitBlock() {
        assertTypeErrors("class C { { return Nil; } }");
        assertTypeErrors("class C { { return Unit; return Unit; } }");
    }

    @Test
    public void initCodeBlock() {
        assertTypeErrors("class C { { await True; } }");
        assertTypeErrors("class C { { Fut<Unit> f; f.get; } }");
        assertTypeErrors("class C { { this.m(); } Unit m() { }}");
        assertTypeErrors("import * from ABS.DC; class C { { [Cost: 5] skip; } }");
        assertTypeErrors("import * from ABS.DC; class C { { [DataSize: 5] skip; } }");
        assertTypeOK("class C { { this.m(); } [Atomic] Unit m() { }}");
    }

    @Test
    public void init_this() {
        assertTypeErrors("class C {} { this.f(); }");
    }

    @Test
    public void wrongNumberOfParams() {
        assertTypeErrors("module Bug;"
          + "def String f (String name) = name;"
          + "{ List<String> s = Cons(f(\"foo\", Nil)); }", ErrorMessage.WRONG_NUMBER_OF_ARGS);
    }

    @Test
    public void overloadingOfImplementedInterfacesInClass() {
        assertTypeErrors("module Test; interface A { Unit m(); } interface B { Unit m(Int x); } class C implements A, B { Unit m() { skip; }}");
    }

    @Test
    public void overloadingOfImplementedInterfacesInInterface() {
        assertTypeErrors("module Test; interface A { Unit m(); } interface B { Unit m(Int x); } interface I extends A, B { }");
    }

    @Test
    public void uninitializedVariable() {
        assertTypeErrors(" { Int x = x; }");
    }

    @Test
    public void wrongInitOrder() {
        assertTypeErrors(" class C { Int x = y + 1; Int y = 1; }");
    }

    @Test
    public void wrongInitOrder1() {
        assertTypeErrors(" class C { Int x = this.y + 1; Int y = 1; }");
    }

    @Test
    public void wrongInitOrder2() {
        assertTypeErrors(" class C { Int x = x + 1; }");
    }

    @Test
    public void rightInitOrder() {
        assertTypeOK(" class C { Int y = 1; Int x = y + 1; }");
    }


    @Test
    public void localVariableHiding() {
        assertTypeErrors(" class C { Unit foo() { Int x = 1; if (True) { Int x = 2; }} }");
    }

    @Test
    public void localVariableHiding2() {
        assertTypeErrors(" class C { Unit foo(Int x) { if (True) { Int x = 2; } } }");
    }


    @Test
    public void localVariableHiding3() {
        assertTypeErrors(" class C {{ Int x = 1; if (True) { Int x = 2; }}}");
    }

    @Test
    public void localVariableHidingOk() {
        assertTypeOK(" class C { Unit foo() { if (True) { Int x = 1; } if (True) { Int x = 2; }} }");
    }

    @Test
    public void localVariableHidingOk2() {
        assertTypeOK(" class C { Unit foo() { if (True) { Int x = 2; } Int x = 1; } }");
    }

    @Test
    public void localVariableHidingOk3() {
        assertTypeOK(" class C {  Int x = 1; Unit foo() { if (True) { Int x = 2; } } }");
    }

    @Test
    public void localVariableHidingOk4() {
        assertTypeOK(" class C {  Int x = 1; { Int x = 2; } }");
    }

    @Test
    public void typeRecursive1() throws Exception {
       assertTypeErrors("type Foo = Foo;", ErrorMessage.CIRCULAR_TYPESYN);
    }

    @Test
    public void typeRecursive2() throws Exception {
        // TODO: reactivate testing for the more specific error once our type
        // checking code is unified (we currently have two type-checking
        // passes and the second one would raise the specified error message)
        assertTypeErrors("type Foo = Bar; type Bar = Foo;" /*, ErrorMessage.CIRCULAR_TYPESYN*/);
    }

    @Test
    public void constNull1() throws Exception {
        assertTypeErrors("{ null!m(); }", ErrorMessage.NULL_NOT_HERE);
    }

    @Test
    public void constNull2() throws Exception {
        assertTypeErrors("{ null.m(); }", ErrorMessage.NULL_NOT_HERE);
    }

    @Test
    public void constructorPatternBorked1() {
        assertTypeErrors("interface I {} { List<I> ifs = Nil;   Unit u = case ifs { Cons(I(x), Nil) => Unit; }; }", ErrorMessage.CONSTRUCTOR_NOT_RESOLVABLE);
    }

    @Test
    public void schedulerInvalid1() {
        assertTypeErrors("module M; import * from ABS.Scheduler; [Scheduler: xx] class C { }");
    }

    @Test
    public void schedulerInvalid2() {
        assertTypeErrors("module M; import * from ABS.Scheduler; [Scheduler: foo(queue)] class C { }");
    }

    @Test
    public void schedulerInvalid3() {
        assertTypeErrors("module M; import * from ABS.Scheduler; def Process f(List<Process> queue) = head(queue); [Scheduler: f(x)] class C { }");
    }

    @Test
    public void schedulerInvalid4() {
        assertTypeErrors("module M; import * from ABS.Scheduler; def Process f(List<Process> queue) = head(queue); [Scheduler: f(queue, b)] class C { }");
    }

    @Test
    public void schedulerInvalid5() {
        assertTypeErrors("module M; import * from ABS.Scheduler; def Process f(List<Process> queue, String x) = head(queue); [Scheduler: f(queue, b)] class C { }");
    }

    @Test
    public void schedulerInvalid6() {
        assertTypeErrors("module M; import * from ABS.Scheduler; def Process f(List<Process> queue, String x) = head(queue); [Scheduler: f(queue, b)] class C { Int b = 0; }");
    }

    @Test
    public void annotationCostInvalid() {
        assertTypeErrors("module M; import * from ABS.DC; def Rat error(Rat x) = x; { [Cost: error(False)] skip; } ");
    }
    @Test
    public void annotationDCInvalid() {
        assertTypeErrors("module M; import * from ABS.DC; def DC error(Rat x) = null; class C {} { [DC: error(False)] new C(); } ");
    }
    @Test
    public void annotationDeadlineInvalid() {
        assertTypeErrors("module M; import * from ABS.DC; def Duration error(Rat x) = Duration(x); interface I { Unit m(); } class C implements I { Unit m() { skip; } } { I i = new C(); [Deadline: error(False)] i!m(); } ");
    }
    @Test
    public void annotationDataSizeInvalid() {
        assertTypeErrors("module M; import * from ABS.DC; def Rat error(Rat x) = x; interface I { Unit m(); } class C implements I { Unit m() { skip; } } { I i = new C(); [DataSize: error(False)] i!m(); } ");
    }

    @Test
    public void deltaWithoutUseClause() {
        // Can add new classes etc. without a `uses` clause only with qualified name
        assertTypeErrors("delta D1; adds interface I {}");
    }

    @Test
    public void declarationQualifiedName() {
        // Cannot use qualified names for declarations outside deltas
        assertTypeErrors("module M; interface M.I {}");
    }

    @Test
    public void foreachWithNoList() {
	assertTypeErrors("{ foreach(v, i in 5) { println(`$i$'th int value is $v$`);} }");
    }

    @Test
    public void sqlite3Wrong() {
	// wrong return value: must be a list
	assertTypeErrors("def Int query() = builtin(sqlite3, `test.db`, `select * from table`);");
	// wrong return value: list type must be a datatype that can be converted from sql values
	assertTypeErrors("def List<Object> query() = builtin(sqlite3, `test.db`, `select * from table`);");
	// wrong return value: list type must have 1 constructor
	assertTypeErrors("data Tuple; def List<Tuple> query() = builtin(sqlite3, `test.db`, `select * from table`);");
	// wrong return value: list type constructor must have > 0 arguments
	assertTypeErrors("def List<Unit> query() = builtin(sqlite3, `test.db`, `select * from table`);");
	// wrong arguments: too few arguments
	assertTypeErrors("def List<Int> query() = builtin(sqlite3);");
	assertTypeErrors("def List<Int> query() = builtin(sqlite3, `test.db`);");
	// wrong argument types
	assertTypeErrors("def List<Int> query() = builtin(sqlite3, Unit, 13);");
	// wrong arguments: wrong query parameter type
	assertTypeErrors("def List<Int> query() = builtin(sqlite3, `test.db`, `select * from table`, Unit);");
    }

    @Test
    public void bug323() {
        // https://github.com/abstools/abstools/issues/323
        assertTypeErrors("{ foreach (string in snd(entries)) skip; }");
    }
}
