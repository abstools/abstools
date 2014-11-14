package gen.ABS;

import gen.ABS.Absyn.*;

/** BNFC-Generated All Visitor */
public interface AllVisitor<R,A> extends
  gen.ABS.Absyn.AnyIdent.Visitor<R,A>,
  gen.ABS.Absyn.Program.Visitor<R,A>,
  gen.ABS.Absyn.Module.Visitor<R,A>,
  gen.ABS.Absyn.Export.Visitor<R,A>,
  gen.ABS.Absyn.Import.Visitor<R,A>,
  gen.ABS.Absyn.ImportType.Visitor<R,A>,
  gen.ABS.Absyn.Type.Visitor<R,A>,
  gen.ABS.Absyn.QualType.Visitor<R,A>,
  gen.ABS.Absyn.QualTypeSegment.Visitor<R,A>,
  gen.ABS.Absyn.Decl.Visitor<R,A>,
  gen.ABS.Absyn.ConstrIdent.Visitor<R,A>,
  gen.ABS.Absyn.ConstrType.Visitor<R,A>,
  gen.ABS.Absyn.FunBody.Visitor<R,A>,
  gen.ABS.Absyn.MethSignat.Visitor<R,A>,
  gen.ABS.Absyn.ClassBody.Visitor<R,A>,
  gen.ABS.Absyn.Block.Visitor<R,A>,
  gen.ABS.Absyn.MaybeBlock.Visitor<R,A>,
  gen.ABS.Absyn.Param.Visitor<R,A>,
  gen.ABS.Absyn.Stm.Visitor<R,A>,
  gen.ABS.Absyn.CatchBranch.Visitor<R,A>,
  gen.ABS.Absyn.MaybeFinally.Visitor<R,A>,
  gen.ABS.Absyn.Guard.Visitor<R,A>,
  gen.ABS.Absyn.Exp.Visitor<R,A>,
  gen.ABS.Absyn.PureExp.Visitor<R,A>,
  gen.ABS.Absyn.CaseBranch.Visitor<R,A>,
  gen.ABS.Absyn.Pattern.Visitor<R,A>,
  gen.ABS.Absyn.Literal.Visitor<R,A>,
  gen.ABS.Absyn.EffExp.Visitor<R,A>
{}
