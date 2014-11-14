package gen.ABS;
import gen.ABS.Absyn.*;
/** BNFC-Generated Composition Visitor
*/

public class ComposVisitor<A> implements
  gen.ABS.Absyn.AnyIdent.Visitor<gen.ABS.Absyn.AnyIdent,A>,
  gen.ABS.Absyn.Program.Visitor<gen.ABS.Absyn.Program,A>,
  gen.ABS.Absyn.Module.Visitor<gen.ABS.Absyn.Module,A>,
  gen.ABS.Absyn.Export.Visitor<gen.ABS.Absyn.Export,A>,
  gen.ABS.Absyn.Import.Visitor<gen.ABS.Absyn.Import,A>,
  gen.ABS.Absyn.ImportType.Visitor<gen.ABS.Absyn.ImportType,A>,
  gen.ABS.Absyn.Type.Visitor<gen.ABS.Absyn.Type,A>,
  gen.ABS.Absyn.QualType.Visitor<gen.ABS.Absyn.QualType,A>,
  gen.ABS.Absyn.QualTypeSegment.Visitor<gen.ABS.Absyn.QualTypeSegment,A>,
  gen.ABS.Absyn.Decl.Visitor<gen.ABS.Absyn.Decl,A>,
  gen.ABS.Absyn.ConstrIdent.Visitor<gen.ABS.Absyn.ConstrIdent,A>,
  gen.ABS.Absyn.ConstrType.Visitor<gen.ABS.Absyn.ConstrType,A>,
  gen.ABS.Absyn.FunBody.Visitor<gen.ABS.Absyn.FunBody,A>,
  gen.ABS.Absyn.MethSignat.Visitor<gen.ABS.Absyn.MethSignat,A>,
  gen.ABS.Absyn.ClassBody.Visitor<gen.ABS.Absyn.ClassBody,A>,
  gen.ABS.Absyn.Block.Visitor<gen.ABS.Absyn.Block,A>,
  gen.ABS.Absyn.MaybeBlock.Visitor<gen.ABS.Absyn.MaybeBlock,A>,
  gen.ABS.Absyn.Param.Visitor<gen.ABS.Absyn.Param,A>,
  gen.ABS.Absyn.Stm.Visitor<gen.ABS.Absyn.Stm,A>,
  gen.ABS.Absyn.CatchBranch.Visitor<gen.ABS.Absyn.CatchBranch,A>,
  gen.ABS.Absyn.MaybeFinally.Visitor<gen.ABS.Absyn.MaybeFinally,A>,
  gen.ABS.Absyn.Guard.Visitor<gen.ABS.Absyn.Guard,A>,
  gen.ABS.Absyn.Exp.Visitor<gen.ABS.Absyn.Exp,A>,
  gen.ABS.Absyn.PureExp.Visitor<gen.ABS.Absyn.PureExp,A>,
  gen.ABS.Absyn.CaseBranch.Visitor<gen.ABS.Absyn.CaseBranch,A>,
  gen.ABS.Absyn.Pattern.Visitor<gen.ABS.Absyn.Pattern,A>,
  gen.ABS.Absyn.Literal.Visitor<gen.ABS.Absyn.Literal,A>,
  gen.ABS.Absyn.EffExp.Visitor<gen.ABS.Absyn.EffExp,A>
{
/* AnyIdent */
    public AnyIdent visit(gen.ABS.Absyn.AnyIden p, A arg)
    {
      String ident_ = p.ident_;

      return new gen.ABS.Absyn.AnyIden(ident_);
    }
    public AnyIdent visit(gen.ABS.Absyn.AnyTyIden p, A arg)
    {
      String typeident_ = p.typeident_;

      return new gen.ABS.Absyn.AnyTyIden(typeident_);
    }

/* Program */
    public Program visit(gen.ABS.Absyn.Prog p, A arg)
    {
      ListModule listmodule_ = new ListModule();
      for (Module x : p.listmodule_) {
        listmodule_.add(x.accept(this,arg));
      }

      return new gen.ABS.Absyn.Prog(listmodule_);
    }

/* Module */
    public Module visit(gen.ABS.Absyn.Modul p, A arg)
    {
      QualType qualtype_ = p.qualtype_.accept(this, arg);
      ListExport listexport_ = new ListExport();
      for (Export x : p.listexport_) {
        listexport_.add(x.accept(this,arg));
      }
      ListImport listimport_ = new ListImport();
      for (Import x : p.listimport_) {
        listimport_.add(x.accept(this,arg));
      }
      ListDecl listdecl_ = new ListDecl();
      for (Decl x : p.listdecl_) {
        listdecl_.add(x.accept(this,arg));
      }
      MaybeBlock maybeblock_ = p.maybeblock_.accept(this, arg);

      return new gen.ABS.Absyn.Modul(qualtype_, listexport_, listimport_, listdecl_, maybeblock_);
    }

/* Export */
    public Export visit(gen.ABS.Absyn.AnyExport p, A arg)
    {
      ListAnyIdent listanyident_ = new ListAnyIdent();
      for (AnyIdent x : p.listanyident_) {
        listanyident_.add(x.accept(this,arg));
      }

      return new gen.ABS.Absyn.AnyExport(listanyident_);
    }
    public Export visit(gen.ABS.Absyn.AnyFromExport p, A arg)
    {
      ListAnyIdent listanyident_ = new ListAnyIdent();
      for (AnyIdent x : p.listanyident_) {
        listanyident_.add(x.accept(this,arg));
      }
      QualType qualtype_ = p.qualtype_.accept(this, arg);

      return new gen.ABS.Absyn.AnyFromExport(listanyident_, qualtype_);
    }
    public Export visit(gen.ABS.Absyn.StarExport p, A arg)
    {

      return new gen.ABS.Absyn.StarExport();
    }
    public Export visit(gen.ABS.Absyn.StarFromExport p, A arg)
    {
      QualType qualtype_ = p.qualtype_.accept(this, arg);

      return new gen.ABS.Absyn.StarFromExport(qualtype_);
    }

/* Import */
    public Import visit(gen.ABS.Absyn.AnyImport p, A arg)
    {
      ImportType importtype_ = p.importtype_.accept(this, arg);
      QualType qualtype_ = p.qualtype_.accept(this, arg);
      AnyIdent anyident_ = p.anyident_.accept(this, arg);

      return new gen.ABS.Absyn.AnyImport(importtype_, qualtype_, anyident_);
    }
    public Import visit(gen.ABS.Absyn.AnyFromImport p, A arg)
    {
      ImportType importtype_ = p.importtype_.accept(this, arg);
      ListAnyIdent listanyident_ = new ListAnyIdent();
      for (AnyIdent x : p.listanyident_) {
        listanyident_.add(x.accept(this,arg));
      }
      QualType qualtype_ = p.qualtype_.accept(this, arg);

      return new gen.ABS.Absyn.AnyFromImport(importtype_, listanyident_, qualtype_);
    }
    public Import visit(gen.ABS.Absyn.StarFromImport p, A arg)
    {
      ImportType importtype_ = p.importtype_.accept(this, arg);
      QualType qualtype_ = p.qualtype_.accept(this, arg);

      return new gen.ABS.Absyn.StarFromImport(importtype_, qualtype_);
    }

/* ImportType */
    public ImportType visit(gen.ABS.Absyn.ForeignImport p, A arg)
    {

      return new gen.ABS.Absyn.ForeignImport();
    }
    public ImportType visit(gen.ABS.Absyn.NormalImport p, A arg)
    {

      return new gen.ABS.Absyn.NormalImport();
    }

/* Type */
    public Type visit(gen.ABS.Absyn.TUnderscore p, A arg)
    {

      return new gen.ABS.Absyn.TUnderscore();
    }
    public Type visit(gen.ABS.Absyn.TSimple p, A arg)
    {
      QualType qualtype_ = p.qualtype_.accept(this, arg);

      return new gen.ABS.Absyn.TSimple(qualtype_);
    }
    public Type visit(gen.ABS.Absyn.TGen p, A arg)
    {
      QualType qualtype_ = p.qualtype_.accept(this, arg);
      ListType listtype_ = new ListType();
      for (Type x : p.listtype_) {
        listtype_.add(x.accept(this,arg));
      }

      return new gen.ABS.Absyn.TGen(qualtype_, listtype_);
    }

/* QualType */
    public QualType visit(gen.ABS.Absyn.QType p, A arg)
    {
      ListQualTypeSegment listqualtypesegment_ = new ListQualTypeSegment();
      for (QualTypeSegment x : p.listqualtypesegment_) {
        listqualtypesegment_.add(x.accept(this,arg));
      }

      return new gen.ABS.Absyn.QType(listqualtypesegment_);
    }

/* QualTypeSegment */
    public QualTypeSegment visit(gen.ABS.Absyn.QTypeSegment p, A arg)
    {
      String typeident_ = p.typeident_;

      return new gen.ABS.Absyn.QTypeSegment(typeident_);
    }

/* Decl */
    public Decl visit(gen.ABS.Absyn.TypeDecl p, A arg)
    {
      String typeident_ = p.typeident_;
      Type type_ = p.type_.accept(this, arg);

      return new gen.ABS.Absyn.TypeDecl(typeident_, type_);
    }
    public Decl visit(gen.ABS.Absyn.ExceptionDecl p, A arg)
    {
      ConstrIdent constrident_ = p.constrident_.accept(this, arg);

      return new gen.ABS.Absyn.ExceptionDecl(constrident_);
    }
    public Decl visit(gen.ABS.Absyn.DataDecl p, A arg)
    {
      String typeident_ = p.typeident_;
      ListConstrIdent listconstrident_ = new ListConstrIdent();
      for (ConstrIdent x : p.listconstrident_) {
        listconstrident_.add(x.accept(this,arg));
      }

      return new gen.ABS.Absyn.DataDecl(typeident_, listconstrident_);
    }
    public Decl visit(gen.ABS.Absyn.DataParDecl p, A arg)
    {
      String typeident_ = p.typeident_;
      ListTypeIdent listtypeident_ = p.listtypeident_;
      ListConstrIdent listconstrident_ = new ListConstrIdent();
      for (ConstrIdent x : p.listconstrident_) {
        listconstrident_.add(x.accept(this,arg));
      }

      return new gen.ABS.Absyn.DataParDecl(typeident_, listtypeident_, listconstrident_);
    }
    public Decl visit(gen.ABS.Absyn.FunDecl p, A arg)
    {
      Type type_ = p.type_.accept(this, arg);
      String ident_ = p.ident_;
      ListParam listparam_ = new ListParam();
      for (Param x : p.listparam_) {
        listparam_.add(x.accept(this,arg));
      }
      FunBody funbody_ = p.funbody_.accept(this, arg);

      return new gen.ABS.Absyn.FunDecl(type_, ident_, listparam_, funbody_);
    }
    public Decl visit(gen.ABS.Absyn.FunParDecl p, A arg)
    {
      Type type_ = p.type_.accept(this, arg);
      String ident_ = p.ident_;
      ListTypeIdent listtypeident_ = p.listtypeident_;
      ListParam listparam_ = new ListParam();
      for (Param x : p.listparam_) {
        listparam_.add(x.accept(this,arg));
      }
      FunBody funbody_ = p.funbody_.accept(this, arg);

      return new gen.ABS.Absyn.FunParDecl(type_, ident_, listtypeident_, listparam_, funbody_);
    }
    public Decl visit(gen.ABS.Absyn.InterfDecl p, A arg)
    {
      String typeident_ = p.typeident_;
      ListMethSignat listmethsignat_ = new ListMethSignat();
      for (MethSignat x : p.listmethsignat_) {
        listmethsignat_.add(x.accept(this,arg));
      }

      return new gen.ABS.Absyn.InterfDecl(typeident_, listmethsignat_);
    }
    public Decl visit(gen.ABS.Absyn.ExtendsDecl p, A arg)
    {
      String typeident_ = p.typeident_;
      ListQualType listqualtype_ = new ListQualType();
      for (QualType x : p.listqualtype_) {
        listqualtype_.add(x.accept(this,arg));
      }
      ListMethSignat listmethsignat_ = new ListMethSignat();
      for (MethSignat x : p.listmethsignat_) {
        listmethsignat_.add(x.accept(this,arg));
      }

      return new gen.ABS.Absyn.ExtendsDecl(typeident_, listqualtype_, listmethsignat_);
    }
    public Decl visit(gen.ABS.Absyn.ClassDecl p, A arg)
    {
      String typeident_ = p.typeident_;
      ListClassBody listclassbody_1 = new ListClassBody();
      for (ClassBody x : p.listclassbody_1) {
        listclassbody_1.add(x.accept(this,arg));
      }
      MaybeBlock maybeblock_ = p.maybeblock_.accept(this, arg);
      ListClassBody listclassbody_2 = new ListClassBody();
      for (ClassBody x : p.listclassbody_2) {
        listclassbody_2.add(x.accept(this,arg));
      }

      return new gen.ABS.Absyn.ClassDecl(typeident_, listclassbody_1, maybeblock_, listclassbody_2);
    }
    public Decl visit(gen.ABS.Absyn.ClassParamDecl p, A arg)
    {
      String typeident_ = p.typeident_;
      ListParam listparam_ = new ListParam();
      for (Param x : p.listparam_) {
        listparam_.add(x.accept(this,arg));
      }
      ListClassBody listclassbody_1 = new ListClassBody();
      for (ClassBody x : p.listclassbody_1) {
        listclassbody_1.add(x.accept(this,arg));
      }
      MaybeBlock maybeblock_ = p.maybeblock_.accept(this, arg);
      ListClassBody listclassbody_2 = new ListClassBody();
      for (ClassBody x : p.listclassbody_2) {
        listclassbody_2.add(x.accept(this,arg));
      }

      return new gen.ABS.Absyn.ClassParamDecl(typeident_, listparam_, listclassbody_1, maybeblock_, listclassbody_2);
    }
    public Decl visit(gen.ABS.Absyn.ClassImplements p, A arg)
    {
      String typeident_ = p.typeident_;
      ListQualType listqualtype_ = new ListQualType();
      for (QualType x : p.listqualtype_) {
        listqualtype_.add(x.accept(this,arg));
      }
      ListClassBody listclassbody_1 = new ListClassBody();
      for (ClassBody x : p.listclassbody_1) {
        listclassbody_1.add(x.accept(this,arg));
      }
      MaybeBlock maybeblock_ = p.maybeblock_.accept(this, arg);
      ListClassBody listclassbody_2 = new ListClassBody();
      for (ClassBody x : p.listclassbody_2) {
        listclassbody_2.add(x.accept(this,arg));
      }

      return new gen.ABS.Absyn.ClassImplements(typeident_, listqualtype_, listclassbody_1, maybeblock_, listclassbody_2);
    }
    public Decl visit(gen.ABS.Absyn.ClassParamImplements p, A arg)
    {
      String typeident_ = p.typeident_;
      ListParam listparam_ = new ListParam();
      for (Param x : p.listparam_) {
        listparam_.add(x.accept(this,arg));
      }
      ListQualType listqualtype_ = new ListQualType();
      for (QualType x : p.listqualtype_) {
        listqualtype_.add(x.accept(this,arg));
      }
      ListClassBody listclassbody_1 = new ListClassBody();
      for (ClassBody x : p.listclassbody_1) {
        listclassbody_1.add(x.accept(this,arg));
      }
      MaybeBlock maybeblock_ = p.maybeblock_.accept(this, arg);
      ListClassBody listclassbody_2 = new ListClassBody();
      for (ClassBody x : p.listclassbody_2) {
        listclassbody_2.add(x.accept(this,arg));
      }

      return new gen.ABS.Absyn.ClassParamImplements(typeident_, listparam_, listqualtype_, listclassbody_1, maybeblock_, listclassbody_2);
    }

/* ConstrIdent */
    public ConstrIdent visit(gen.ABS.Absyn.SinglConstrIdent p, A arg)
    {
      String typeident_ = p.typeident_;

      return new gen.ABS.Absyn.SinglConstrIdent(typeident_);
    }
    public ConstrIdent visit(gen.ABS.Absyn.ParamConstrIdent p, A arg)
    {
      String typeident_ = p.typeident_;
      ListConstrType listconstrtype_ = new ListConstrType();
      for (ConstrType x : p.listconstrtype_) {
        listconstrtype_.add(x.accept(this,arg));
      }

      return new gen.ABS.Absyn.ParamConstrIdent(typeident_, listconstrtype_);
    }

/* ConstrType */
    public ConstrType visit(gen.ABS.Absyn.EmptyConstrType p, A arg)
    {
      Type type_ = p.type_.accept(this, arg);

      return new gen.ABS.Absyn.EmptyConstrType(type_);
    }
    public ConstrType visit(gen.ABS.Absyn.RecordConstrType p, A arg)
    {
      Type type_ = p.type_.accept(this, arg);
      String ident_ = p.ident_;

      return new gen.ABS.Absyn.RecordConstrType(type_, ident_);
    }

/* FunBody */
    public FunBody visit(gen.ABS.Absyn.BuiltinFunBody p, A arg)
    {

      return new gen.ABS.Absyn.BuiltinFunBody();
    }
    public FunBody visit(gen.ABS.Absyn.NormalFunBody p, A arg)
    {
      PureExp pureexp_ = p.pureexp_.accept(this, arg);

      return new gen.ABS.Absyn.NormalFunBody(pureexp_);
    }

/* MethSignat */
    public MethSignat visit(gen.ABS.Absyn.MethSig p, A arg)
    {
      Type type_ = p.type_.accept(this, arg);
      String ident_ = p.ident_;
      ListParam listparam_ = new ListParam();
      for (Param x : p.listparam_) {
        listparam_.add(x.accept(this,arg));
      }

      return new gen.ABS.Absyn.MethSig(type_, ident_, listparam_);
    }

/* ClassBody */
    public ClassBody visit(gen.ABS.Absyn.FieldClassBody p, A arg)
    {
      Type type_ = p.type_.accept(this, arg);
      String ident_ = p.ident_;

      return new gen.ABS.Absyn.FieldClassBody(type_, ident_);
    }
    public ClassBody visit(gen.ABS.Absyn.FieldAssignClassBody p, A arg)
    {
      Type type_ = p.type_.accept(this, arg);
      String ident_ = p.ident_;
      PureExp pureexp_ = p.pureexp_.accept(this, arg);

      return new gen.ABS.Absyn.FieldAssignClassBody(type_, ident_, pureexp_);
    }
    public ClassBody visit(gen.ABS.Absyn.MethClassBody p, A arg)
    {
      Type type_ = p.type_.accept(this, arg);
      String ident_ = p.ident_;
      ListParam listparam_ = new ListParam();
      for (Param x : p.listparam_) {
        listparam_.add(x.accept(this,arg));
      }
      Block block_ = p.block_.accept(this, arg);

      return new gen.ABS.Absyn.MethClassBody(type_, ident_, listparam_, block_);
    }

/* Block */
    public Block visit(gen.ABS.Absyn.Bloc p, A arg)
    {
      ListStm liststm_ = new ListStm();
      for (Stm x : p.liststm_) {
        liststm_.add(x.accept(this,arg));
      }

      return new gen.ABS.Absyn.Bloc(liststm_);
    }

/* MaybeBlock */
    public MaybeBlock visit(gen.ABS.Absyn.JustBlock p, A arg)
    {
      Block block_ = p.block_.accept(this, arg);

      return new gen.ABS.Absyn.JustBlock(block_);
    }
    public MaybeBlock visit(gen.ABS.Absyn.NoBlock p, A arg)
    {

      return new gen.ABS.Absyn.NoBlock();
    }

/* Param */
    public Param visit(gen.ABS.Absyn.Par p, A arg)
    {
      Type type_ = p.type_.accept(this, arg);
      String ident_ = p.ident_;

      return new gen.ABS.Absyn.Par(type_, ident_);
    }

/* Stm */
    public Stm visit(gen.ABS.Absyn.SExp p, A arg)
    {
      Exp exp_ = p.exp_.accept(this, arg);

      return new gen.ABS.Absyn.SExp(exp_);
    }
    public Stm visit(gen.ABS.Absyn.SBlock p, A arg)
    {
      ListStm liststm_ = new ListStm();
      for (Stm x : p.liststm_) {
        liststm_.add(x.accept(this,arg));
      }

      return new gen.ABS.Absyn.SBlock(liststm_);
    }
    public Stm visit(gen.ABS.Absyn.SWhile p, A arg)
    {
      PureExp pureexp_ = p.pureexp_.accept(this, arg);
      Stm stm_ = p.stm_.accept(this, arg);

      return new gen.ABS.Absyn.SWhile(pureexp_, stm_);
    }
    public Stm visit(gen.ABS.Absyn.SReturn p, A arg)
    {
      Exp exp_ = p.exp_.accept(this, arg);

      return new gen.ABS.Absyn.SReturn(exp_);
    }
    public Stm visit(gen.ABS.Absyn.SAss p, A arg)
    {
      String ident_ = p.ident_;
      Exp exp_ = p.exp_.accept(this, arg);

      return new gen.ABS.Absyn.SAss(ident_, exp_);
    }
    public Stm visit(gen.ABS.Absyn.SFieldAss p, A arg)
    {
      String ident_ = p.ident_;
      Exp exp_ = p.exp_.accept(this, arg);

      return new gen.ABS.Absyn.SFieldAss(ident_, exp_);
    }
    public Stm visit(gen.ABS.Absyn.SDec p, A arg)
    {
      Type type_ = p.type_.accept(this, arg);
      String ident_ = p.ident_;

      return new gen.ABS.Absyn.SDec(type_, ident_);
    }
    public Stm visit(gen.ABS.Absyn.SDecAss p, A arg)
    {
      Type type_ = p.type_.accept(this, arg);
      String ident_ = p.ident_;
      Exp exp_ = p.exp_.accept(this, arg);

      return new gen.ABS.Absyn.SDecAss(type_, ident_, exp_);
    }
    public Stm visit(gen.ABS.Absyn.SIf p, A arg)
    {
      PureExp pureexp_ = p.pureexp_.accept(this, arg);
      Stm stm_ = p.stm_.accept(this, arg);

      return new gen.ABS.Absyn.SIf(pureexp_, stm_);
    }
    public Stm visit(gen.ABS.Absyn.SIfElse p, A arg)
    {
      PureExp pureexp_ = p.pureexp_.accept(this, arg);
      Stm stm_1 = p.stm_1.accept(this, arg);
      Stm stm_2 = p.stm_2.accept(this, arg);

      return new gen.ABS.Absyn.SIfElse(pureexp_, stm_1, stm_2);
    }
    public Stm visit(gen.ABS.Absyn.SSuspend p, A arg)
    {

      return new gen.ABS.Absyn.SSuspend();
    }
    public Stm visit(gen.ABS.Absyn.SSkip p, A arg)
    {

      return new gen.ABS.Absyn.SSkip();
    }
    public Stm visit(gen.ABS.Absyn.SAssert p, A arg)
    {
      PureExp pureexp_ = p.pureexp_.accept(this, arg);

      return new gen.ABS.Absyn.SAssert(pureexp_);
    }
    public Stm visit(gen.ABS.Absyn.SAwait p, A arg)
    {
      Guard guard_ = p.guard_.accept(this, arg);

      return new gen.ABS.Absyn.SAwait(guard_);
    }
    public Stm visit(gen.ABS.Absyn.SThrow p, A arg)
    {
      PureExp pureexp_ = p.pureexp_.accept(this, arg);

      return new gen.ABS.Absyn.SThrow(pureexp_);
    }
    public Stm visit(gen.ABS.Absyn.STryCatchFinally p, A arg)
    {
      Stm stm_ = p.stm_.accept(this, arg);
      ListCatchBranch listcatchbranch_ = new ListCatchBranch();
      for (CatchBranch x : p.listcatchbranch_) {
        listcatchbranch_.add(x.accept(this,arg));
      }
      MaybeFinally maybefinally_ = p.maybefinally_.accept(this, arg);

      return new gen.ABS.Absyn.STryCatchFinally(stm_, listcatchbranch_, maybefinally_);
    }

/* CatchBranch */
    public CatchBranch visit(gen.ABS.Absyn.CatchBranc p, A arg)
    {
      Pattern pattern_ = p.pattern_.accept(this, arg);
      Stm stm_ = p.stm_.accept(this, arg);

      return new gen.ABS.Absyn.CatchBranc(pattern_, stm_);
    }

/* MaybeFinally */
    public MaybeFinally visit(gen.ABS.Absyn.JustFinally p, A arg)
    {
      Stm stm_ = p.stm_.accept(this, arg);

      return new gen.ABS.Absyn.JustFinally(stm_);
    }
    public MaybeFinally visit(gen.ABS.Absyn.NoFinally p, A arg)
    {

      return new gen.ABS.Absyn.NoFinally();
    }

/* Guard */
    public Guard visit(gen.ABS.Absyn.VarGuard p, A arg)
    {
      String ident_ = p.ident_;

      return new gen.ABS.Absyn.VarGuard(ident_);
    }
    public Guard visit(gen.ABS.Absyn.FieldGuard p, A arg)
    {
      String ident_ = p.ident_;

      return new gen.ABS.Absyn.FieldGuard(ident_);
    }
    public Guard visit(gen.ABS.Absyn.ExpGuard p, A arg)
    {
      PureExp pureexp_ = p.pureexp_.accept(this, arg);

      return new gen.ABS.Absyn.ExpGuard(pureexp_);
    }
    public Guard visit(gen.ABS.Absyn.AndGuard p, A arg)
    {
      Guard guard_1 = p.guard_1.accept(this, arg);
      Guard guard_2 = p.guard_2.accept(this, arg);

      return new gen.ABS.Absyn.AndGuard(guard_1, guard_2);
    }

/* Exp */
    public Exp visit(gen.ABS.Absyn.ExpP p, A arg)
    {
      PureExp pureexp_ = p.pureexp_.accept(this, arg);

      return new gen.ABS.Absyn.ExpP(pureexp_);
    }
    public Exp visit(gen.ABS.Absyn.ExpE p, A arg)
    {
      EffExp effexp_ = p.effexp_.accept(this, arg);

      return new gen.ABS.Absyn.ExpE(effexp_);
    }

/* PureExp */
    public PureExp visit(gen.ABS.Absyn.EOr p, A arg)
    {
      PureExp pureexp_1 = p.pureexp_1.accept(this, arg);
      PureExp pureexp_2 = p.pureexp_2.accept(this, arg);

      return new gen.ABS.Absyn.EOr(pureexp_1, pureexp_2);
    }
    public PureExp visit(gen.ABS.Absyn.Let p, A arg)
    {
      Param param_ = p.param_.accept(this, arg);
      PureExp pureexp_1 = p.pureexp_1.accept(this, arg);
      PureExp pureexp_2 = p.pureexp_2.accept(this, arg);

      return new gen.ABS.Absyn.Let(param_, pureexp_1, pureexp_2);
    }
    public PureExp visit(gen.ABS.Absyn.If p, A arg)
    {
      PureExp pureexp_1 = p.pureexp_1.accept(this, arg);
      PureExp pureexp_2 = p.pureexp_2.accept(this, arg);
      PureExp pureexp_3 = p.pureexp_3.accept(this, arg);

      return new gen.ABS.Absyn.If(pureexp_1, pureexp_2, pureexp_3);
    }
    public PureExp visit(gen.ABS.Absyn.Case p, A arg)
    {
      PureExp pureexp_ = p.pureexp_.accept(this, arg);
      ListCaseBranch listcasebranch_ = new ListCaseBranch();
      for (CaseBranch x : p.listcasebranch_) {
        listcasebranch_.add(x.accept(this,arg));
      }

      return new gen.ABS.Absyn.Case(pureexp_, listcasebranch_);
    }
    public PureExp visit(gen.ABS.Absyn.EAnd p, A arg)
    {
      PureExp pureexp_1 = p.pureexp_1.accept(this, arg);
      PureExp pureexp_2 = p.pureexp_2.accept(this, arg);

      return new gen.ABS.Absyn.EAnd(pureexp_1, pureexp_2);
    }
    public PureExp visit(gen.ABS.Absyn.EEq p, A arg)
    {
      PureExp pureexp_1 = p.pureexp_1.accept(this, arg);
      PureExp pureexp_2 = p.pureexp_2.accept(this, arg);

      return new gen.ABS.Absyn.EEq(pureexp_1, pureexp_2);
    }
    public PureExp visit(gen.ABS.Absyn.ENeq p, A arg)
    {
      PureExp pureexp_1 = p.pureexp_1.accept(this, arg);
      PureExp pureexp_2 = p.pureexp_2.accept(this, arg);

      return new gen.ABS.Absyn.ENeq(pureexp_1, pureexp_2);
    }
    public PureExp visit(gen.ABS.Absyn.ELt p, A arg)
    {
      PureExp pureexp_1 = p.pureexp_1.accept(this, arg);
      PureExp pureexp_2 = p.pureexp_2.accept(this, arg);

      return new gen.ABS.Absyn.ELt(pureexp_1, pureexp_2);
    }
    public PureExp visit(gen.ABS.Absyn.ELe p, A arg)
    {
      PureExp pureexp_1 = p.pureexp_1.accept(this, arg);
      PureExp pureexp_2 = p.pureexp_2.accept(this, arg);

      return new gen.ABS.Absyn.ELe(pureexp_1, pureexp_2);
    }
    public PureExp visit(gen.ABS.Absyn.EGt p, A arg)
    {
      PureExp pureexp_1 = p.pureexp_1.accept(this, arg);
      PureExp pureexp_2 = p.pureexp_2.accept(this, arg);

      return new gen.ABS.Absyn.EGt(pureexp_1, pureexp_2);
    }
    public PureExp visit(gen.ABS.Absyn.EGe p, A arg)
    {
      PureExp pureexp_1 = p.pureexp_1.accept(this, arg);
      PureExp pureexp_2 = p.pureexp_2.accept(this, arg);

      return new gen.ABS.Absyn.EGe(pureexp_1, pureexp_2);
    }
    public PureExp visit(gen.ABS.Absyn.EAdd p, A arg)
    {
      PureExp pureexp_1 = p.pureexp_1.accept(this, arg);
      PureExp pureexp_2 = p.pureexp_2.accept(this, arg);

      return new gen.ABS.Absyn.EAdd(pureexp_1, pureexp_2);
    }
    public PureExp visit(gen.ABS.Absyn.ESub p, A arg)
    {
      PureExp pureexp_1 = p.pureexp_1.accept(this, arg);
      PureExp pureexp_2 = p.pureexp_2.accept(this, arg);

      return new gen.ABS.Absyn.ESub(pureexp_1, pureexp_2);
    }
    public PureExp visit(gen.ABS.Absyn.EMul p, A arg)
    {
      PureExp pureexp_1 = p.pureexp_1.accept(this, arg);
      PureExp pureexp_2 = p.pureexp_2.accept(this, arg);

      return new gen.ABS.Absyn.EMul(pureexp_1, pureexp_2);
    }
    public PureExp visit(gen.ABS.Absyn.EDiv p, A arg)
    {
      PureExp pureexp_1 = p.pureexp_1.accept(this, arg);
      PureExp pureexp_2 = p.pureexp_2.accept(this, arg);

      return new gen.ABS.Absyn.EDiv(pureexp_1, pureexp_2);
    }
    public PureExp visit(gen.ABS.Absyn.EMod p, A arg)
    {
      PureExp pureexp_1 = p.pureexp_1.accept(this, arg);
      PureExp pureexp_2 = p.pureexp_2.accept(this, arg);

      return new gen.ABS.Absyn.EMod(pureexp_1, pureexp_2);
    }
    public PureExp visit(gen.ABS.Absyn.ELogNeg p, A arg)
    {
      PureExp pureexp_ = p.pureexp_.accept(this, arg);

      return new gen.ABS.Absyn.ELogNeg(pureexp_);
    }
    public PureExp visit(gen.ABS.Absyn.EIntNeg p, A arg)
    {
      PureExp pureexp_ = p.pureexp_.accept(this, arg);

      return new gen.ABS.Absyn.EIntNeg(pureexp_);
    }
    public PureExp visit(gen.ABS.Absyn.EFunCall p, A arg)
    {
      String ident_ = p.ident_;
      ListPureExp listpureexp_ = new ListPureExp();
      for (PureExp x : p.listpureexp_) {
        listpureexp_.add(x.accept(this,arg));
      }

      return new gen.ABS.Absyn.EFunCall(ident_, listpureexp_);
    }
    public PureExp visit(gen.ABS.Absyn.EQualFunCall p, A arg)
    {
      QualType qualtype_ = p.qualtype_.accept(this, arg);
      String ident_ = p.ident_;
      ListPureExp listpureexp_ = new ListPureExp();
      for (PureExp x : p.listpureexp_) {
        listpureexp_.add(x.accept(this,arg));
      }

      return new gen.ABS.Absyn.EQualFunCall(qualtype_, ident_, listpureexp_);
    }
    public PureExp visit(gen.ABS.Absyn.ENaryFunCall p, A arg)
    {
      String ident_ = p.ident_;
      ListPureExp listpureexp_ = new ListPureExp();
      for (PureExp x : p.listpureexp_) {
        listpureexp_.add(x.accept(this,arg));
      }

      return new gen.ABS.Absyn.ENaryFunCall(ident_, listpureexp_);
    }
    public PureExp visit(gen.ABS.Absyn.ENaryQualFunCall p, A arg)
    {
      QualType qualtype_ = p.qualtype_.accept(this, arg);
      String ident_ = p.ident_;
      ListPureExp listpureexp_ = new ListPureExp();
      for (PureExp x : p.listpureexp_) {
        listpureexp_.add(x.accept(this,arg));
      }

      return new gen.ABS.Absyn.ENaryQualFunCall(qualtype_, ident_, listpureexp_);
    }
    public PureExp visit(gen.ABS.Absyn.EVar p, A arg)
    {
      String ident_ = p.ident_;

      return new gen.ABS.Absyn.EVar(ident_);
    }
    public PureExp visit(gen.ABS.Absyn.EThis p, A arg)
    {
      String ident_ = p.ident_;

      return new gen.ABS.Absyn.EThis(ident_);
    }
    public PureExp visit(gen.ABS.Absyn.EQualVar p, A arg)
    {
      QualType qualtype_ = p.qualtype_.accept(this, arg);
      String ident_ = p.ident_;

      return new gen.ABS.Absyn.EQualVar(qualtype_, ident_);
    }
    public PureExp visit(gen.ABS.Absyn.ESinglConstr p, A arg)
    {
      QualType qualtype_ = p.qualtype_.accept(this, arg);

      return new gen.ABS.Absyn.ESinglConstr(qualtype_);
    }
    public PureExp visit(gen.ABS.Absyn.EParamConstr p, A arg)
    {
      QualType qualtype_ = p.qualtype_.accept(this, arg);
      ListPureExp listpureexp_ = new ListPureExp();
      for (PureExp x : p.listpureexp_) {
        listpureexp_.add(x.accept(this,arg));
      }

      return new gen.ABS.Absyn.EParamConstr(qualtype_, listpureexp_);
    }
    public PureExp visit(gen.ABS.Absyn.ELit p, A arg)
    {
      Literal literal_ = p.literal_.accept(this, arg);

      return new gen.ABS.Absyn.ELit(literal_);
    }

/* CaseBranch */
    public CaseBranch visit(gen.ABS.Absyn.CaseBranc p, A arg)
    {
      Pattern pattern_ = p.pattern_.accept(this, arg);
      PureExp pureexp_ = p.pureexp_.accept(this, arg);

      return new gen.ABS.Absyn.CaseBranc(pattern_, pureexp_);
    }

/* Pattern */
    public Pattern visit(gen.ABS.Absyn.PIdent p, A arg)
    {
      String ident_ = p.ident_;

      return new gen.ABS.Absyn.PIdent(ident_);
    }
    public Pattern visit(gen.ABS.Absyn.PLit p, A arg)
    {
      Literal literal_ = p.literal_.accept(this, arg);

      return new gen.ABS.Absyn.PLit(literal_);
    }
    public Pattern visit(gen.ABS.Absyn.PSinglConstr p, A arg)
    {
      String typeident_ = p.typeident_;

      return new gen.ABS.Absyn.PSinglConstr(typeident_);
    }
    public Pattern visit(gen.ABS.Absyn.PParamConstr p, A arg)
    {
      String typeident_ = p.typeident_;
      ListPattern listpattern_ = new ListPattern();
      for (Pattern x : p.listpattern_) {
        listpattern_.add(x.accept(this,arg));
      }

      return new gen.ABS.Absyn.PParamConstr(typeident_, listpattern_);
    }
    public Pattern visit(gen.ABS.Absyn.PUnderscore p, A arg)
    {

      return new gen.ABS.Absyn.PUnderscore();
    }

/* Literal */
    public Literal visit(gen.ABS.Absyn.LNull p, A arg)
    {

      return new gen.ABS.Absyn.LNull();
    }
    public Literal visit(gen.ABS.Absyn.LThis p, A arg)
    {

      return new gen.ABS.Absyn.LThis();
    }
    public Literal visit(gen.ABS.Absyn.LThisDC p, A arg)
    {

      return new gen.ABS.Absyn.LThisDC();
    }
    public Literal visit(gen.ABS.Absyn.LStr p, A arg)
    {
      String string_ = p.string_;

      return new gen.ABS.Absyn.LStr(string_);
    }
    public Literal visit(gen.ABS.Absyn.LInt p, A arg)
    {
      Integer integer_ = p.integer_;

      return new gen.ABS.Absyn.LInt(integer_);
    }

/* EffExp */
    public EffExp visit(gen.ABS.Absyn.New p, A arg)
    {
      Type type_ = p.type_.accept(this, arg);
      ListPureExp listpureexp_ = new ListPureExp();
      for (PureExp x : p.listpureexp_) {
        listpureexp_.add(x.accept(this,arg));
      }

      return new gen.ABS.Absyn.New(type_, listpureexp_);
    }
    public EffExp visit(gen.ABS.Absyn.NewLocal p, A arg)
    {
      Type type_ = p.type_.accept(this, arg);
      ListPureExp listpureexp_ = new ListPureExp();
      for (PureExp x : p.listpureexp_) {
        listpureexp_.add(x.accept(this,arg));
      }

      return new gen.ABS.Absyn.NewLocal(type_, listpureexp_);
    }
    public EffExp visit(gen.ABS.Absyn.SyncMethCall p, A arg)
    {
      PureExp pureexp_ = p.pureexp_.accept(this, arg);
      String ident_ = p.ident_;
      ListPureExp listpureexp_ = new ListPureExp();
      for (PureExp x : p.listpureexp_) {
        listpureexp_.add(x.accept(this,arg));
      }

      return new gen.ABS.Absyn.SyncMethCall(pureexp_, ident_, listpureexp_);
    }
    public EffExp visit(gen.ABS.Absyn.ThisSyncMethCall p, A arg)
    {
      String ident_ = p.ident_;
      ListPureExp listpureexp_ = new ListPureExp();
      for (PureExp x : p.listpureexp_) {
        listpureexp_.add(x.accept(this,arg));
      }

      return new gen.ABS.Absyn.ThisSyncMethCall(ident_, listpureexp_);
    }
    public EffExp visit(gen.ABS.Absyn.AsyncMethCall p, A arg)
    {
      PureExp pureexp_ = p.pureexp_.accept(this, arg);
      String ident_ = p.ident_;
      ListPureExp listpureexp_ = new ListPureExp();
      for (PureExp x : p.listpureexp_) {
        listpureexp_.add(x.accept(this,arg));
      }

      return new gen.ABS.Absyn.AsyncMethCall(pureexp_, ident_, listpureexp_);
    }
    public EffExp visit(gen.ABS.Absyn.ThisAsyncMethCall p, A arg)
    {
      String ident_ = p.ident_;
      ListPureExp listpureexp_ = new ListPureExp();
      for (PureExp x : p.listpureexp_) {
        listpureexp_.add(x.accept(this,arg));
      }

      return new gen.ABS.Absyn.ThisAsyncMethCall(ident_, listpureexp_);
    }
    public EffExp visit(gen.ABS.Absyn.Get p, A arg)
    {
      PureExp pureexp_ = p.pureexp_.accept(this, arg);

      return new gen.ABS.Absyn.Get(pureexp_);
    }
    public EffExp visit(gen.ABS.Absyn.Spawns p, A arg)
    {
      PureExp pureexp_ = p.pureexp_.accept(this, arg);
      Type type_ = p.type_.accept(this, arg);
      ListPureExp listpureexp_ = new ListPureExp();
      for (PureExp x : p.listpureexp_) {
        listpureexp_.add(x.accept(this,arg));
      }

      return new gen.ABS.Absyn.Spawns(pureexp_, type_, listpureexp_);
    }

}