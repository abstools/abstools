package gen.ABS;
import gen.ABS.Absyn.*;
/** BNFC-Generated Abstract Visitor */
public class AbstractVisitor<R,A> implements AllVisitor<R,A> {
/* AnyIdent */
    public R visit(gen.ABS.Absyn.AnyIden p, A arg) { return visitDefault(p, arg); }
    public R visit(gen.ABS.Absyn.AnyTyIden p, A arg) { return visitDefault(p, arg); }
    public R visitDefault(gen.ABS.Absyn.AnyIdent p, A arg) {
      throw new IllegalArgumentException(this.getClass().getName() + ": " + p);
    }
/* Program */
    public R visit(gen.ABS.Absyn.Prog p, A arg) { return visitDefault(p, arg); }
    public R visitDefault(gen.ABS.Absyn.Program p, A arg) {
      throw new IllegalArgumentException(this.getClass().getName() + ": " + p);
    }
/* Module */
    public R visit(gen.ABS.Absyn.Modul p, A arg) { return visitDefault(p, arg); }
    public R visitDefault(gen.ABS.Absyn.Module p, A arg) {
      throw new IllegalArgumentException(this.getClass().getName() + ": " + p);
    }
/* Export */
    public R visit(gen.ABS.Absyn.AnyExport p, A arg) { return visitDefault(p, arg); }
    public R visit(gen.ABS.Absyn.AnyFromExport p, A arg) { return visitDefault(p, arg); }
    public R visit(gen.ABS.Absyn.StarExport p, A arg) { return visitDefault(p, arg); }
    public R visit(gen.ABS.Absyn.StarFromExport p, A arg) { return visitDefault(p, arg); }
    public R visitDefault(gen.ABS.Absyn.Export p, A arg) {
      throw new IllegalArgumentException(this.getClass().getName() + ": " + p);
    }
/* Import */
    public R visit(gen.ABS.Absyn.AnyImport p, A arg) { return visitDefault(p, arg); }
    public R visit(gen.ABS.Absyn.AnyFromImport p, A arg) { return visitDefault(p, arg); }
    public R visit(gen.ABS.Absyn.StarFromImport p, A arg) { return visitDefault(p, arg); }
    public R visitDefault(gen.ABS.Absyn.Import p, A arg) {
      throw new IllegalArgumentException(this.getClass().getName() + ": " + p);
    }
/* ImportType */
    public R visit(gen.ABS.Absyn.ForeignImport p, A arg) { return visitDefault(p, arg); }
    public R visit(gen.ABS.Absyn.NormalImport p, A arg) { return visitDefault(p, arg); }
    public R visitDefault(gen.ABS.Absyn.ImportType p, A arg) {
      throw new IllegalArgumentException(this.getClass().getName() + ": " + p);
    }
/* Type */
    public R visit(gen.ABS.Absyn.TUnderscore p, A arg) { return visitDefault(p, arg); }
    public R visit(gen.ABS.Absyn.TSimple p, A arg) { return visitDefault(p, arg); }
    public R visit(gen.ABS.Absyn.TGen p, A arg) { return visitDefault(p, arg); }
    public R visitDefault(gen.ABS.Absyn.Type p, A arg) {
      throw new IllegalArgumentException(this.getClass().getName() + ": " + p);
    }
/* QualType */
    public R visit(gen.ABS.Absyn.QType p, A arg) { return visitDefault(p, arg); }
    public R visitDefault(gen.ABS.Absyn.QualType p, A arg) {
      throw new IllegalArgumentException(this.getClass().getName() + ": " + p);
    }
/* QualTypeSegment */
    public R visit(gen.ABS.Absyn.QTypeSegment p, A arg) { return visitDefault(p, arg); }
    public R visitDefault(gen.ABS.Absyn.QualTypeSegment p, A arg) {
      throw new IllegalArgumentException(this.getClass().getName() + ": " + p);
    }
/* Decl */
    public R visit(gen.ABS.Absyn.TypeDecl p, A arg) { return visitDefault(p, arg); }
    public R visit(gen.ABS.Absyn.ExceptionDecl p, A arg) { return visitDefault(p, arg); }
    public R visit(gen.ABS.Absyn.DataDecl p, A arg) { return visitDefault(p, arg); }
    public R visit(gen.ABS.Absyn.DataParDecl p, A arg) { return visitDefault(p, arg); }
    public R visit(gen.ABS.Absyn.FunDecl p, A arg) { return visitDefault(p, arg); }
    public R visit(gen.ABS.Absyn.FunParDecl p, A arg) { return visitDefault(p, arg); }
    public R visit(gen.ABS.Absyn.InterfDecl p, A arg) { return visitDefault(p, arg); }
    public R visit(gen.ABS.Absyn.ExtendsDecl p, A arg) { return visitDefault(p, arg); }
    public R visit(gen.ABS.Absyn.ClassDecl p, A arg) { return visitDefault(p, arg); }
    public R visit(gen.ABS.Absyn.ClassParamDecl p, A arg) { return visitDefault(p, arg); }
    public R visit(gen.ABS.Absyn.ClassImplements p, A arg) { return visitDefault(p, arg); }
    public R visit(gen.ABS.Absyn.ClassParamImplements p, A arg) { return visitDefault(p, arg); }
    public R visitDefault(gen.ABS.Absyn.Decl p, A arg) {
      throw new IllegalArgumentException(this.getClass().getName() + ": " + p);
    }
/* ConstrIdent */
    public R visit(gen.ABS.Absyn.SinglConstrIdent p, A arg) { return visitDefault(p, arg); }
    public R visit(gen.ABS.Absyn.ParamConstrIdent p, A arg) { return visitDefault(p, arg); }
    public R visitDefault(gen.ABS.Absyn.ConstrIdent p, A arg) {
      throw new IllegalArgumentException(this.getClass().getName() + ": " + p);
    }
/* ConstrType */
    public R visit(gen.ABS.Absyn.EmptyConstrType p, A arg) { return visitDefault(p, arg); }
    public R visit(gen.ABS.Absyn.RecordConstrType p, A arg) { return visitDefault(p, arg); }
    public R visitDefault(gen.ABS.Absyn.ConstrType p, A arg) {
      throw new IllegalArgumentException(this.getClass().getName() + ": " + p);
    }
/* FunBody */
    public R visit(gen.ABS.Absyn.BuiltinFunBody p, A arg) { return visitDefault(p, arg); }
    public R visit(gen.ABS.Absyn.NormalFunBody p, A arg) { return visitDefault(p, arg); }
    public R visitDefault(gen.ABS.Absyn.FunBody p, A arg) {
      throw new IllegalArgumentException(this.getClass().getName() + ": " + p);
    }
/* MethSignat */
    public R visit(gen.ABS.Absyn.MethSig p, A arg) { return visitDefault(p, arg); }
    public R visitDefault(gen.ABS.Absyn.MethSignat p, A arg) {
      throw new IllegalArgumentException(this.getClass().getName() + ": " + p);
    }
/* ClassBody */
    public R visit(gen.ABS.Absyn.FieldClassBody p, A arg) { return visitDefault(p, arg); }
    public R visit(gen.ABS.Absyn.FieldAssignClassBody p, A arg) { return visitDefault(p, arg); }
    public R visit(gen.ABS.Absyn.MethClassBody p, A arg) { return visitDefault(p, arg); }
    public R visitDefault(gen.ABS.Absyn.ClassBody p, A arg) {
      throw new IllegalArgumentException(this.getClass().getName() + ": " + p);
    }
/* Block */
    public R visit(gen.ABS.Absyn.Bloc p, A arg) { return visitDefault(p, arg); }
    public R visitDefault(gen.ABS.Absyn.Block p, A arg) {
      throw new IllegalArgumentException(this.getClass().getName() + ": " + p);
    }
/* MaybeBlock */
    public R visit(gen.ABS.Absyn.JustBlock p, A arg) { return visitDefault(p, arg); }
    public R visit(gen.ABS.Absyn.NoBlock p, A arg) { return visitDefault(p, arg); }
    public R visitDefault(gen.ABS.Absyn.MaybeBlock p, A arg) {
      throw new IllegalArgumentException(this.getClass().getName() + ": " + p);
    }
/* Param */
    public R visit(gen.ABS.Absyn.Par p, A arg) { return visitDefault(p, arg); }
    public R visitDefault(gen.ABS.Absyn.Param p, A arg) {
      throw new IllegalArgumentException(this.getClass().getName() + ": " + p);
    }
/* Stm */
    public R visit(gen.ABS.Absyn.SExp p, A arg) { return visitDefault(p, arg); }
    public R visit(gen.ABS.Absyn.SBlock p, A arg) { return visitDefault(p, arg); }
    public R visit(gen.ABS.Absyn.SWhile p, A arg) { return visitDefault(p, arg); }
    public R visit(gen.ABS.Absyn.SReturn p, A arg) { return visitDefault(p, arg); }
    public R visit(gen.ABS.Absyn.SAss p, A arg) { return visitDefault(p, arg); }
    public R visit(gen.ABS.Absyn.SFieldAss p, A arg) { return visitDefault(p, arg); }
    public R visit(gen.ABS.Absyn.SDec p, A arg) { return visitDefault(p, arg); }
    public R visit(gen.ABS.Absyn.SDecAss p, A arg) { return visitDefault(p, arg); }
    public R visit(gen.ABS.Absyn.SIf p, A arg) { return visitDefault(p, arg); }
    public R visit(gen.ABS.Absyn.SIfElse p, A arg) { return visitDefault(p, arg); }
    public R visit(gen.ABS.Absyn.SSuspend p, A arg) { return visitDefault(p, arg); }
    public R visit(gen.ABS.Absyn.SSkip p, A arg) { return visitDefault(p, arg); }
    public R visit(gen.ABS.Absyn.SAssert p, A arg) { return visitDefault(p, arg); }
    public R visit(gen.ABS.Absyn.SAwait p, A arg) { return visitDefault(p, arg); }
    public R visit(gen.ABS.Absyn.SThrow p, A arg) { return visitDefault(p, arg); }
    public R visit(gen.ABS.Absyn.STryCatchFinally p, A arg) { return visitDefault(p, arg); }
    public R visitDefault(gen.ABS.Absyn.Stm p, A arg) {
      throw new IllegalArgumentException(this.getClass().getName() + ": " + p);
    }
/* CatchBranch */
    public R visit(gen.ABS.Absyn.CatchBranc p, A arg) { return visitDefault(p, arg); }
    public R visitDefault(gen.ABS.Absyn.CatchBranch p, A arg) {
      throw new IllegalArgumentException(this.getClass().getName() + ": " + p);
    }
/* MaybeFinally */
    public R visit(gen.ABS.Absyn.JustFinally p, A arg) { return visitDefault(p, arg); }
    public R visit(gen.ABS.Absyn.NoFinally p, A arg) { return visitDefault(p, arg); }
    public R visitDefault(gen.ABS.Absyn.MaybeFinally p, A arg) {
      throw new IllegalArgumentException(this.getClass().getName() + ": " + p);
    }
/* Guard */
    public R visit(gen.ABS.Absyn.VarGuard p, A arg) { return visitDefault(p, arg); }
    public R visit(gen.ABS.Absyn.FieldGuard p, A arg) { return visitDefault(p, arg); }
    public R visit(gen.ABS.Absyn.ExpGuard p, A arg) { return visitDefault(p, arg); }
    public R visit(gen.ABS.Absyn.AndGuard p, A arg) { return visitDefault(p, arg); }
    public R visitDefault(gen.ABS.Absyn.Guard p, A arg) {
      throw new IllegalArgumentException(this.getClass().getName() + ": " + p);
    }
/* Exp */
    public R visit(gen.ABS.Absyn.ExpP p, A arg) { return visitDefault(p, arg); }
    public R visit(gen.ABS.Absyn.ExpE p, A arg) { return visitDefault(p, arg); }
    public R visitDefault(gen.ABS.Absyn.Exp p, A arg) {
      throw new IllegalArgumentException(this.getClass().getName() + ": " + p);
    }
/* PureExp */
    public R visit(gen.ABS.Absyn.EOr p, A arg) { return visitDefault(p, arg); }

    public R visit(gen.ABS.Absyn.Let p, A arg) { return visitDefault(p, arg); }
    public R visit(gen.ABS.Absyn.If p, A arg) { return visitDefault(p, arg); }
    public R visit(gen.ABS.Absyn.Case p, A arg) { return visitDefault(p, arg); }
    public R visit(gen.ABS.Absyn.EAnd p, A arg) { return visitDefault(p, arg); }

    public R visit(gen.ABS.Absyn.EEq p, A arg) { return visitDefault(p, arg); }
    public R visit(gen.ABS.Absyn.ENeq p, A arg) { return visitDefault(p, arg); }

    public R visit(gen.ABS.Absyn.ELt p, A arg) { return visitDefault(p, arg); }
    public R visit(gen.ABS.Absyn.ELe p, A arg) { return visitDefault(p, arg); }
    public R visit(gen.ABS.Absyn.EGt p, A arg) { return visitDefault(p, arg); }
    public R visit(gen.ABS.Absyn.EGe p, A arg) { return visitDefault(p, arg); }

    public R visit(gen.ABS.Absyn.EAdd p, A arg) { return visitDefault(p, arg); }
    public R visit(gen.ABS.Absyn.ESub p, A arg) { return visitDefault(p, arg); }

    public R visit(gen.ABS.Absyn.EMul p, A arg) { return visitDefault(p, arg); }
    public R visit(gen.ABS.Absyn.EDiv p, A arg) { return visitDefault(p, arg); }
    public R visit(gen.ABS.Absyn.EMod p, A arg) { return visitDefault(p, arg); }

    public R visit(gen.ABS.Absyn.ELogNeg p, A arg) { return visitDefault(p, arg); }
    public R visit(gen.ABS.Absyn.EIntNeg p, A arg) { return visitDefault(p, arg); }

    public R visit(gen.ABS.Absyn.EFunCall p, A arg) { return visitDefault(p, arg); }
    public R visit(gen.ABS.Absyn.EQualFunCall p, A arg) { return visitDefault(p, arg); }
    public R visit(gen.ABS.Absyn.ENaryFunCall p, A arg) { return visitDefault(p, arg); }
    public R visit(gen.ABS.Absyn.ENaryQualFunCall p, A arg) { return visitDefault(p, arg); }
    public R visit(gen.ABS.Absyn.EVar p, A arg) { return visitDefault(p, arg); }
    public R visit(gen.ABS.Absyn.EThis p, A arg) { return visitDefault(p, arg); }
    public R visit(gen.ABS.Absyn.EQualVar p, A arg) { return visitDefault(p, arg); }
    public R visit(gen.ABS.Absyn.ESinglConstr p, A arg) { return visitDefault(p, arg); }
    public R visit(gen.ABS.Absyn.EParamConstr p, A arg) { return visitDefault(p, arg); }
    public R visit(gen.ABS.Absyn.ELit p, A arg) { return visitDefault(p, arg); }

    public R visitDefault(gen.ABS.Absyn.PureExp p, A arg) {
      throw new IllegalArgumentException(this.getClass().getName() + ": " + p);
    }
/* CaseBranch */
    public R visit(gen.ABS.Absyn.CaseBranc p, A arg) { return visitDefault(p, arg); }
    public R visitDefault(gen.ABS.Absyn.CaseBranch p, A arg) {
      throw new IllegalArgumentException(this.getClass().getName() + ": " + p);
    }
/* Pattern */
    public R visit(gen.ABS.Absyn.PIdent p, A arg) { return visitDefault(p, arg); }
    public R visit(gen.ABS.Absyn.PLit p, A arg) { return visitDefault(p, arg); }
    public R visit(gen.ABS.Absyn.PSinglConstr p, A arg) { return visitDefault(p, arg); }
    public R visit(gen.ABS.Absyn.PParamConstr p, A arg) { return visitDefault(p, arg); }
    public R visit(gen.ABS.Absyn.PUnderscore p, A arg) { return visitDefault(p, arg); }
    public R visitDefault(gen.ABS.Absyn.Pattern p, A arg) {
      throw new IllegalArgumentException(this.getClass().getName() + ": " + p);
    }
/* Literal */
    public R visit(gen.ABS.Absyn.LNull p, A arg) { return visitDefault(p, arg); }
    public R visit(gen.ABS.Absyn.LThis p, A arg) { return visitDefault(p, arg); }
    public R visit(gen.ABS.Absyn.LThisDC p, A arg) { return visitDefault(p, arg); }
    public R visit(gen.ABS.Absyn.LStr p, A arg) { return visitDefault(p, arg); }
    public R visit(gen.ABS.Absyn.LInt p, A arg) { return visitDefault(p, arg); }
    public R visitDefault(gen.ABS.Absyn.Literal p, A arg) {
      throw new IllegalArgumentException(this.getClass().getName() + ": " + p);
    }
/* EffExp */
    public R visit(gen.ABS.Absyn.New p, A arg) { return visitDefault(p, arg); }
    public R visit(gen.ABS.Absyn.NewLocal p, A arg) { return visitDefault(p, arg); }
    public R visit(gen.ABS.Absyn.SyncMethCall p, A arg) { return visitDefault(p, arg); }
    public R visit(gen.ABS.Absyn.ThisSyncMethCall p, A arg) { return visitDefault(p, arg); }
    public R visit(gen.ABS.Absyn.AsyncMethCall p, A arg) { return visitDefault(p, arg); }
    public R visit(gen.ABS.Absyn.ThisAsyncMethCall p, A arg) { return visitDefault(p, arg); }
    public R visit(gen.ABS.Absyn.Get p, A arg) { return visitDefault(p, arg); }
    public R visit(gen.ABS.Absyn.Spawns p, A arg) { return visitDefault(p, arg); }
    public R visitDefault(gen.ABS.Absyn.EffExp p, A arg) {
      throw new IllegalArgumentException(this.getClass().getName() + ": " + p);
    }

}
