package gen.ABS;

import gen.ABS.Absyn.*;
import java.util.Collections;
import java.util.List;
import java.util.ArrayList;

/** BNFC-Generated Fold Visitor */
public abstract class FoldVisitor<R,A> implements AllVisitor<R,A> {
    public abstract R leaf(A arg);
    public abstract R combine(R x, R y, A arg);

/* AnyIdent */
    public R visit(gen.ABS.Absyn.AnyIden p, A arg) {
      R r = leaf(arg);
      return r;
    }
    public R visit(gen.ABS.Absyn.AnyTyIden p, A arg) {
      R r = leaf(arg);
      return r;
    }

/* Program */
    public R visit(gen.ABS.Absyn.Prog p, A arg) {
      R r = leaf(arg);
      for (Module x : p.listmodule_) {
        r = combine(x.accept(this,arg), r, arg);
      }
      return r;
    }

/* Module */
    public R visit(gen.ABS.Absyn.Modul p, A arg) {
      R r = leaf(arg);
      r = combine(p.qualtype_.accept(this, arg), r, arg);
      for (Export x : p.listexport_) {
        r = combine(x.accept(this,arg), r, arg);
      }
      for (Import x : p.listimport_) {
        r = combine(x.accept(this,arg), r, arg);
      }
      for (Decl x : p.listdecl_) {
        r = combine(x.accept(this,arg), r, arg);
      }
      r = combine(p.maybeblock_.accept(this, arg), r, arg);
      return r;
    }

/* Export */
    public R visit(gen.ABS.Absyn.AnyExport p, A arg) {
      R r = leaf(arg);
      for (AnyIdent x : p.listanyident_) {
        r = combine(x.accept(this,arg), r, arg);
      }
      return r;
    }
    public R visit(gen.ABS.Absyn.AnyFromExport p, A arg) {
      R r = leaf(arg);
      for (AnyIdent x : p.listanyident_) {
        r = combine(x.accept(this,arg), r, arg);
      }
      r = combine(p.qualtype_.accept(this, arg), r, arg);
      return r;
    }
    public R visit(gen.ABS.Absyn.StarExport p, A arg) {
      R r = leaf(arg);
      return r;
    }
    public R visit(gen.ABS.Absyn.StarFromExport p, A arg) {
      R r = leaf(arg);
      r = combine(p.qualtype_.accept(this, arg), r, arg);
      return r;
    }

/* Import */
    public R visit(gen.ABS.Absyn.AnyImport p, A arg) {
      R r = leaf(arg);
      r = combine(p.importtype_.accept(this, arg), r, arg);
      r = combine(p.qualtype_.accept(this, arg), r, arg);
      r = combine(p.anyident_.accept(this, arg), r, arg);
      return r;
    }
    public R visit(gen.ABS.Absyn.AnyFromImport p, A arg) {
      R r = leaf(arg);
      r = combine(p.importtype_.accept(this, arg), r, arg);
      for (AnyIdent x : p.listanyident_) {
        r = combine(x.accept(this,arg), r, arg);
      }
      r = combine(p.qualtype_.accept(this, arg), r, arg);
      return r;
    }
    public R visit(gen.ABS.Absyn.StarFromImport p, A arg) {
      R r = leaf(arg);
      r = combine(p.importtype_.accept(this, arg), r, arg);
      r = combine(p.qualtype_.accept(this, arg), r, arg);
      return r;
    }

/* ImportType */
    public R visit(gen.ABS.Absyn.ForeignImport p, A arg) {
      R r = leaf(arg);
      return r;
    }
    public R visit(gen.ABS.Absyn.NormalImport p, A arg) {
      R r = leaf(arg);
      return r;
    }

/* Type */
    public R visit(gen.ABS.Absyn.TUnderscore p, A arg) {
      R r = leaf(arg);
      return r;
    }
    public R visit(gen.ABS.Absyn.TSimple p, A arg) {
      R r = leaf(arg);
      r = combine(p.qualtype_.accept(this, arg), r, arg);
      return r;
    }
    public R visit(gen.ABS.Absyn.TGen p, A arg) {
      R r = leaf(arg);
      r = combine(p.qualtype_.accept(this, arg), r, arg);
      for (Type x : p.listtype_) {
        r = combine(x.accept(this,arg), r, arg);
      }
      return r;
    }

/* QualType */
    public R visit(gen.ABS.Absyn.QType p, A arg) {
      R r = leaf(arg);
      for (QualTypeSegment x : p.listqualtypesegment_) {
        r = combine(x.accept(this,arg), r, arg);
      }
      return r;
    }

/* QualTypeSegment */
    public R visit(gen.ABS.Absyn.QTypeSegment p, A arg) {
      R r = leaf(arg);
      return r;
    }

/* Decl */
    public R visit(gen.ABS.Absyn.TypeDecl p, A arg) {
      R r = leaf(arg);
      r = combine(p.type_.accept(this, arg), r, arg);
      return r;
    }
    public R visit(gen.ABS.Absyn.ExceptionDecl p, A arg) {
      R r = leaf(arg);
      r = combine(p.constrident_.accept(this, arg), r, arg);
      return r;
    }
    public R visit(gen.ABS.Absyn.DataDecl p, A arg) {
      R r = leaf(arg);
      for (ConstrIdent x : p.listconstrident_) {
        r = combine(x.accept(this,arg), r, arg);
      }
      return r;
    }
    public R visit(gen.ABS.Absyn.DataParDecl p, A arg) {
      R r = leaf(arg);
      for (ConstrIdent x : p.listconstrident_) {
        r = combine(x.accept(this,arg), r, arg);
      }
      return r;
    }
    public R visit(gen.ABS.Absyn.FunDecl p, A arg) {
      R r = leaf(arg);
      r = combine(p.type_.accept(this, arg), r, arg);
      for (Param x : p.listparam_) {
        r = combine(x.accept(this,arg), r, arg);
      }
      r = combine(p.funbody_.accept(this, arg), r, arg);
      return r;
    }
    public R visit(gen.ABS.Absyn.FunParDecl p, A arg) {
      R r = leaf(arg);
      r = combine(p.type_.accept(this, arg), r, arg);
      for (Param x : p.listparam_) {
        r = combine(x.accept(this,arg), r, arg);
      }
      r = combine(p.funbody_.accept(this, arg), r, arg);
      return r;
    }
    public R visit(gen.ABS.Absyn.InterfDecl p, A arg) {
      R r = leaf(arg);
      for (MethSignat x : p.listmethsignat_) {
        r = combine(x.accept(this,arg), r, arg);
      }
      return r;
    }
    public R visit(gen.ABS.Absyn.ExtendsDecl p, A arg) {
      R r = leaf(arg);
      for (QualType x : p.listqualtype_) {
        r = combine(x.accept(this,arg), r, arg);
      }
      for (MethSignat x : p.listmethsignat_) {
        r = combine(x.accept(this,arg), r, arg);
      }
      return r;
    }
    public R visit(gen.ABS.Absyn.ClassDecl p, A arg) {
      R r = leaf(arg);
      for (ClassBody x : p.listclassbody_1) {
        r = combine(x.accept(this,arg), r, arg);
      }
      r = combine(p.maybeblock_.accept(this, arg), r, arg);
      for (ClassBody x : p.listclassbody_2) {
        r = combine(x.accept(this,arg), r, arg);
      }
      return r;
    }
    public R visit(gen.ABS.Absyn.ClassParamDecl p, A arg) {
      R r = leaf(arg);
      for (Param x : p.listparam_) {
        r = combine(x.accept(this,arg), r, arg);
      }
      for (ClassBody x : p.listclassbody_1) {
        r = combine(x.accept(this,arg), r, arg);
      }
      r = combine(p.maybeblock_.accept(this, arg), r, arg);
      for (ClassBody x : p.listclassbody_2) {
        r = combine(x.accept(this,arg), r, arg);
      }
      return r;
    }
    public R visit(gen.ABS.Absyn.ClassImplements p, A arg) {
      R r = leaf(arg);
      for (QualType x : p.listqualtype_) {
        r = combine(x.accept(this,arg), r, arg);
      }
      for (ClassBody x : p.listclassbody_1) {
        r = combine(x.accept(this,arg), r, arg);
      }
      r = combine(p.maybeblock_.accept(this, arg), r, arg);
      for (ClassBody x : p.listclassbody_2) {
        r = combine(x.accept(this,arg), r, arg);
      }
      return r;
    }
    public R visit(gen.ABS.Absyn.ClassParamImplements p, A arg) {
      R r = leaf(arg);
      for (Param x : p.listparam_) {
        r = combine(x.accept(this,arg), r, arg);
      }
      for (QualType x : p.listqualtype_) {
        r = combine(x.accept(this,arg), r, arg);
      }
      for (ClassBody x : p.listclassbody_1) {
        r = combine(x.accept(this,arg), r, arg);
      }
      r = combine(p.maybeblock_.accept(this, arg), r, arg);
      for (ClassBody x : p.listclassbody_2) {
        r = combine(x.accept(this,arg), r, arg);
      }
      return r;
    }

/* ConstrIdent */
    public R visit(gen.ABS.Absyn.SinglConstrIdent p, A arg) {
      R r = leaf(arg);
      return r;
    }
    public R visit(gen.ABS.Absyn.ParamConstrIdent p, A arg) {
      R r = leaf(arg);
      for (ConstrType x : p.listconstrtype_) {
        r = combine(x.accept(this,arg), r, arg);
      }
      return r;
    }

/* ConstrType */
    public R visit(gen.ABS.Absyn.EmptyConstrType p, A arg) {
      R r = leaf(arg);
      r = combine(p.type_.accept(this, arg), r, arg);
      return r;
    }
    public R visit(gen.ABS.Absyn.RecordConstrType p, A arg) {
      R r = leaf(arg);
      r = combine(p.type_.accept(this, arg), r, arg);
      return r;
    }

/* FunBody */
    public R visit(gen.ABS.Absyn.BuiltinFunBody p, A arg) {
      R r = leaf(arg);
      return r;
    }
    public R visit(gen.ABS.Absyn.NormalFunBody p, A arg) {
      R r = leaf(arg);
      r = combine(p.pureexp_.accept(this, arg), r, arg);
      return r;
    }

/* MethSignat */
    public R visit(gen.ABS.Absyn.MethSig p, A arg) {
      R r = leaf(arg);
      r = combine(p.type_.accept(this, arg), r, arg);
      for (Param x : p.listparam_) {
        r = combine(x.accept(this,arg), r, arg);
      }
      return r;
    }

/* ClassBody */
    public R visit(gen.ABS.Absyn.FieldClassBody p, A arg) {
      R r = leaf(arg);
      r = combine(p.type_.accept(this, arg), r, arg);
      return r;
    }
    public R visit(gen.ABS.Absyn.FieldAssignClassBody p, A arg) {
      R r = leaf(arg);
      r = combine(p.type_.accept(this, arg), r, arg);
      r = combine(p.pureexp_.accept(this, arg), r, arg);
      return r;
    }
    public R visit(gen.ABS.Absyn.MethClassBody p, A arg) {
      R r = leaf(arg);
      r = combine(p.type_.accept(this, arg), r, arg);
      for (Param x : p.listparam_) {
        r = combine(x.accept(this,arg), r, arg);
      }
      r = combine(p.block_.accept(this, arg), r, arg);
      return r;
    }

/* Block */
    public R visit(gen.ABS.Absyn.Bloc p, A arg) {
      R r = leaf(arg);
      for (Stm x : p.liststm_) {
        r = combine(x.accept(this,arg), r, arg);
      }
      return r;
    }

/* MaybeBlock */
    public R visit(gen.ABS.Absyn.JustBlock p, A arg) {
      R r = leaf(arg);
      r = combine(p.block_.accept(this, arg), r, arg);
      return r;
    }
    public R visit(gen.ABS.Absyn.NoBlock p, A arg) {
      R r = leaf(arg);
      return r;
    }

/* Param */
    public R visit(gen.ABS.Absyn.Par p, A arg) {
      R r = leaf(arg);
      r = combine(p.type_.accept(this, arg), r, arg);
      return r;
    }

/* Stm */
    public R visit(gen.ABS.Absyn.SExp p, A arg) {
      R r = leaf(arg);
      r = combine(p.exp_.accept(this, arg), r, arg);
      return r;
    }
    public R visit(gen.ABS.Absyn.SBlock p, A arg) {
      R r = leaf(arg);
      for (Stm x : p.liststm_) {
        r = combine(x.accept(this,arg), r, arg);
      }
      return r;
    }
    public R visit(gen.ABS.Absyn.SWhile p, A arg) {
      R r = leaf(arg);
      r = combine(p.pureexp_.accept(this, arg), r, arg);
      r = combine(p.stm_.accept(this, arg), r, arg);
      return r;
    }
    public R visit(gen.ABS.Absyn.SReturn p, A arg) {
      R r = leaf(arg);
      r = combine(p.exp_.accept(this, arg), r, arg);
      return r;
    }
    public R visit(gen.ABS.Absyn.SAss p, A arg) {
      R r = leaf(arg);
      r = combine(p.exp_.accept(this, arg), r, arg);
      return r;
    }
    public R visit(gen.ABS.Absyn.SFieldAss p, A arg) {
      R r = leaf(arg);
      r = combine(p.exp_.accept(this, arg), r, arg);
      return r;
    }
    public R visit(gen.ABS.Absyn.SDec p, A arg) {
      R r = leaf(arg);
      r = combine(p.type_.accept(this, arg), r, arg);
      return r;
    }
    public R visit(gen.ABS.Absyn.SDecAss p, A arg) {
      R r = leaf(arg);
      r = combine(p.type_.accept(this, arg), r, arg);
      r = combine(p.exp_.accept(this, arg), r, arg);
      return r;
    }
    public R visit(gen.ABS.Absyn.SIf p, A arg) {
      R r = leaf(arg);
      r = combine(p.pureexp_.accept(this, arg), r, arg);
      r = combine(p.stm_.accept(this, arg), r, arg);
      return r;
    }
    public R visit(gen.ABS.Absyn.SIfElse p, A arg) {
      R r = leaf(arg);
      r = combine(p.pureexp_.accept(this, arg), r, arg);
      r = combine(p.stm_1.accept(this, arg), r, arg);
      r = combine(p.stm_2.accept(this, arg), r, arg);
      return r;
    }
    public R visit(gen.ABS.Absyn.SSuspend p, A arg) {
      R r = leaf(arg);
      return r;
    }
    public R visit(gen.ABS.Absyn.SSkip p, A arg) {
      R r = leaf(arg);
      return r;
    }
    public R visit(gen.ABS.Absyn.SAssert p, A arg) {
      R r = leaf(arg);
      r = combine(p.pureexp_.accept(this, arg), r, arg);
      return r;
    }
    public R visit(gen.ABS.Absyn.SAwait p, A arg) {
      R r = leaf(arg);
      r = combine(p.guard_.accept(this, arg), r, arg);
      return r;
    }
    public R visit(gen.ABS.Absyn.SThrow p, A arg) {
      R r = leaf(arg);
      r = combine(p.pureexp_.accept(this, arg), r, arg);
      return r;
    }
    public R visit(gen.ABS.Absyn.STryCatchFinally p, A arg) {
      R r = leaf(arg);
      r = combine(p.stm_.accept(this, arg), r, arg);
      for (CatchBranch x : p.listcatchbranch_) {
        r = combine(x.accept(this,arg), r, arg);
      }
      r = combine(p.maybefinally_.accept(this, arg), r, arg);
      return r;
    }

/* CatchBranch */
    public R visit(gen.ABS.Absyn.CatchBranc p, A arg) {
      R r = leaf(arg);
      r = combine(p.pattern_.accept(this, arg), r, arg);
      r = combine(p.stm_.accept(this, arg), r, arg);
      return r;
    }

/* MaybeFinally */
    public R visit(gen.ABS.Absyn.JustFinally p, A arg) {
      R r = leaf(arg);
      r = combine(p.stm_.accept(this, arg), r, arg);
      return r;
    }
    public R visit(gen.ABS.Absyn.NoFinally p, A arg) {
      R r = leaf(arg);
      return r;
    }

/* Guard */
    public R visit(gen.ABS.Absyn.VarGuard p, A arg) {
      R r = leaf(arg);
      return r;
    }
    public R visit(gen.ABS.Absyn.FieldGuard p, A arg) {
      R r = leaf(arg);
      return r;
    }
    public R visit(gen.ABS.Absyn.ExpGuard p, A arg) {
      R r = leaf(arg);
      r = combine(p.pureexp_.accept(this, arg), r, arg);
      return r;
    }
    public R visit(gen.ABS.Absyn.AndGuard p, A arg) {
      R r = leaf(arg);
      r = combine(p.guard_1.accept(this, arg), r, arg);
      r = combine(p.guard_2.accept(this, arg), r, arg);
      return r;
    }

/* Exp */
    public R visit(gen.ABS.Absyn.ExpP p, A arg) {
      R r = leaf(arg);
      r = combine(p.pureexp_.accept(this, arg), r, arg);
      return r;
    }
    public R visit(gen.ABS.Absyn.ExpE p, A arg) {
      R r = leaf(arg);
      r = combine(p.effexp_.accept(this, arg), r, arg);
      return r;
    }

/* PureExp */
    public R visit(gen.ABS.Absyn.EOr p, A arg) {
      R r = leaf(arg);
      r = combine(p.pureexp_1.accept(this, arg), r, arg);
      r = combine(p.pureexp_2.accept(this, arg), r, arg);
      return r;
    }
    public R visit(gen.ABS.Absyn.Let p, A arg) {
      R r = leaf(arg);
      r = combine(p.param_.accept(this, arg), r, arg);
      r = combine(p.pureexp_1.accept(this, arg), r, arg);
      r = combine(p.pureexp_2.accept(this, arg), r, arg);
      return r;
    }
    public R visit(gen.ABS.Absyn.If p, A arg) {
      R r = leaf(arg);
      r = combine(p.pureexp_1.accept(this, arg), r, arg);
      r = combine(p.pureexp_2.accept(this, arg), r, arg);
      r = combine(p.pureexp_3.accept(this, arg), r, arg);
      return r;
    }
    public R visit(gen.ABS.Absyn.Case p, A arg) {
      R r = leaf(arg);
      r = combine(p.pureexp_.accept(this, arg), r, arg);
      for (CaseBranch x : p.listcasebranch_) {
        r = combine(x.accept(this,arg), r, arg);
      }
      return r;
    }
    public R visit(gen.ABS.Absyn.EAnd p, A arg) {
      R r = leaf(arg);
      r = combine(p.pureexp_1.accept(this, arg), r, arg);
      r = combine(p.pureexp_2.accept(this, arg), r, arg);
      return r;
    }
    public R visit(gen.ABS.Absyn.EEq p, A arg) {
      R r = leaf(arg);
      r = combine(p.pureexp_1.accept(this, arg), r, arg);
      r = combine(p.pureexp_2.accept(this, arg), r, arg);
      return r;
    }
    public R visit(gen.ABS.Absyn.ENeq p, A arg) {
      R r = leaf(arg);
      r = combine(p.pureexp_1.accept(this, arg), r, arg);
      r = combine(p.pureexp_2.accept(this, arg), r, arg);
      return r;
    }
    public R visit(gen.ABS.Absyn.ELt p, A arg) {
      R r = leaf(arg);
      r = combine(p.pureexp_1.accept(this, arg), r, arg);
      r = combine(p.pureexp_2.accept(this, arg), r, arg);
      return r;
    }
    public R visit(gen.ABS.Absyn.ELe p, A arg) {
      R r = leaf(arg);
      r = combine(p.pureexp_1.accept(this, arg), r, arg);
      r = combine(p.pureexp_2.accept(this, arg), r, arg);
      return r;
    }
    public R visit(gen.ABS.Absyn.EGt p, A arg) {
      R r = leaf(arg);
      r = combine(p.pureexp_1.accept(this, arg), r, arg);
      r = combine(p.pureexp_2.accept(this, arg), r, arg);
      return r;
    }
    public R visit(gen.ABS.Absyn.EGe p, A arg) {
      R r = leaf(arg);
      r = combine(p.pureexp_1.accept(this, arg), r, arg);
      r = combine(p.pureexp_2.accept(this, arg), r, arg);
      return r;
    }
    public R visit(gen.ABS.Absyn.EAdd p, A arg) {
      R r = leaf(arg);
      r = combine(p.pureexp_1.accept(this, arg), r, arg);
      r = combine(p.pureexp_2.accept(this, arg), r, arg);
      return r;
    }
    public R visit(gen.ABS.Absyn.ESub p, A arg) {
      R r = leaf(arg);
      r = combine(p.pureexp_1.accept(this, arg), r, arg);
      r = combine(p.pureexp_2.accept(this, arg), r, arg);
      return r;
    }
    public R visit(gen.ABS.Absyn.EMul p, A arg) {
      R r = leaf(arg);
      r = combine(p.pureexp_1.accept(this, arg), r, arg);
      r = combine(p.pureexp_2.accept(this, arg), r, arg);
      return r;
    }
    public R visit(gen.ABS.Absyn.EDiv p, A arg) {
      R r = leaf(arg);
      r = combine(p.pureexp_1.accept(this, arg), r, arg);
      r = combine(p.pureexp_2.accept(this, arg), r, arg);
      return r;
    }
    public R visit(gen.ABS.Absyn.EMod p, A arg) {
      R r = leaf(arg);
      r = combine(p.pureexp_1.accept(this, arg), r, arg);
      r = combine(p.pureexp_2.accept(this, arg), r, arg);
      return r;
    }
    public R visit(gen.ABS.Absyn.ELogNeg p, A arg) {
      R r = leaf(arg);
      r = combine(p.pureexp_.accept(this, arg), r, arg);
      return r;
    }
    public R visit(gen.ABS.Absyn.EIntNeg p, A arg) {
      R r = leaf(arg);
      r = combine(p.pureexp_.accept(this, arg), r, arg);
      return r;
    }
    public R visit(gen.ABS.Absyn.EFunCall p, A arg) {
      R r = leaf(arg);
      for (PureExp x : p.listpureexp_) {
        r = combine(x.accept(this,arg), r, arg);
      }
      return r;
    }
    public R visit(gen.ABS.Absyn.EQualFunCall p, A arg) {
      R r = leaf(arg);
      r = combine(p.qualtype_.accept(this, arg), r, arg);
      for (PureExp x : p.listpureexp_) {
        r = combine(x.accept(this,arg), r, arg);
      }
      return r;
    }
    public R visit(gen.ABS.Absyn.ENaryFunCall p, A arg) {
      R r = leaf(arg);
      for (PureExp x : p.listpureexp_) {
        r = combine(x.accept(this,arg), r, arg);
      }
      return r;
    }
    public R visit(gen.ABS.Absyn.ENaryQualFunCall p, A arg) {
      R r = leaf(arg);
      r = combine(p.qualtype_.accept(this, arg), r, arg);
      for (PureExp x : p.listpureexp_) {
        r = combine(x.accept(this,arg), r, arg);
      }
      return r;
    }
    public R visit(gen.ABS.Absyn.EVar p, A arg) {
      R r = leaf(arg);
      return r;
    }
    public R visit(gen.ABS.Absyn.EThis p, A arg) {
      R r = leaf(arg);
      return r;
    }
    public R visit(gen.ABS.Absyn.EQualVar p, A arg) {
      R r = leaf(arg);
      r = combine(p.qualtype_.accept(this, arg), r, arg);
      return r;
    }
    public R visit(gen.ABS.Absyn.ESinglConstr p, A arg) {
      R r = leaf(arg);
      r = combine(p.qualtype_.accept(this, arg), r, arg);
      return r;
    }
    public R visit(gen.ABS.Absyn.EParamConstr p, A arg) {
      R r = leaf(arg);
      r = combine(p.qualtype_.accept(this, arg), r, arg);
      for (PureExp x : p.listpureexp_) {
        r = combine(x.accept(this,arg), r, arg);
      }
      return r;
    }
    public R visit(gen.ABS.Absyn.ELit p, A arg) {
      R r = leaf(arg);
      r = combine(p.literal_.accept(this, arg), r, arg);
      return r;
    }

/* CaseBranch */
    public R visit(gen.ABS.Absyn.CaseBranc p, A arg) {
      R r = leaf(arg);
      r = combine(p.pattern_.accept(this, arg), r, arg);
      r = combine(p.pureexp_.accept(this, arg), r, arg);
      return r;
    }

/* Pattern */
    public R visit(gen.ABS.Absyn.PIdent p, A arg) {
      R r = leaf(arg);
      return r;
    }
    public R visit(gen.ABS.Absyn.PLit p, A arg) {
      R r = leaf(arg);
      r = combine(p.literal_.accept(this, arg), r, arg);
      return r;
    }
    public R visit(gen.ABS.Absyn.PSinglConstr p, A arg) {
      R r = leaf(arg);
      return r;
    }
    public R visit(gen.ABS.Absyn.PParamConstr p, A arg) {
      R r = leaf(arg);
      for (Pattern x : p.listpattern_) {
        r = combine(x.accept(this,arg), r, arg);
      }
      return r;
    }
    public R visit(gen.ABS.Absyn.PUnderscore p, A arg) {
      R r = leaf(arg);
      return r;
    }

/* Literal */
    public R visit(gen.ABS.Absyn.LNull p, A arg) {
      R r = leaf(arg);
      return r;
    }
    public R visit(gen.ABS.Absyn.LThis p, A arg) {
      R r = leaf(arg);
      return r;
    }
    public R visit(gen.ABS.Absyn.LThisDC p, A arg) {
      R r = leaf(arg);
      return r;
    }
    public R visit(gen.ABS.Absyn.LStr p, A arg) {
      R r = leaf(arg);
      return r;
    }
    public R visit(gen.ABS.Absyn.LInt p, A arg) {
      R r = leaf(arg);
      return r;
    }

/* EffExp */
    public R visit(gen.ABS.Absyn.New p, A arg) {
      R r = leaf(arg);
      r = combine(p.type_.accept(this, arg), r, arg);
      for (PureExp x : p.listpureexp_) {
        r = combine(x.accept(this,arg), r, arg);
      }
      return r;
    }
    public R visit(gen.ABS.Absyn.NewLocal p, A arg) {
      R r = leaf(arg);
      r = combine(p.type_.accept(this, arg), r, arg);
      for (PureExp x : p.listpureexp_) {
        r = combine(x.accept(this,arg), r, arg);
      }
      return r;
    }
    public R visit(gen.ABS.Absyn.SyncMethCall p, A arg) {
      R r = leaf(arg);
      r = combine(p.pureexp_.accept(this, arg), r, arg);
      for (PureExp x : p.listpureexp_) {
        r = combine(x.accept(this,arg), r, arg);
      }
      return r;
    }
    public R visit(gen.ABS.Absyn.ThisSyncMethCall p, A arg) {
      R r = leaf(arg);
      for (PureExp x : p.listpureexp_) {
        r = combine(x.accept(this,arg), r, arg);
      }
      return r;
    }
    public R visit(gen.ABS.Absyn.AsyncMethCall p, A arg) {
      R r = leaf(arg);
      r = combine(p.pureexp_.accept(this, arg), r, arg);
      for (PureExp x : p.listpureexp_) {
        r = combine(x.accept(this,arg), r, arg);
      }
      return r;
    }
    public R visit(gen.ABS.Absyn.ThisAsyncMethCall p, A arg) {
      R r = leaf(arg);
      for (PureExp x : p.listpureexp_) {
        r = combine(x.accept(this,arg), r, arg);
      }
      return r;
    }
    public R visit(gen.ABS.Absyn.Get p, A arg) {
      R r = leaf(arg);
      r = combine(p.pureexp_.accept(this, arg), r, arg);
      return r;
    }
    public R visit(gen.ABS.Absyn.Spawns p, A arg) {
      R r = leaf(arg);
      r = combine(p.pureexp_.accept(this, arg), r, arg);
      r = combine(p.type_.accept(this, arg), r, arg);
      for (PureExp x : p.listpureexp_) {
        r = combine(x.accept(this,arg), r, arg);
      }
      return r;
    }


}
