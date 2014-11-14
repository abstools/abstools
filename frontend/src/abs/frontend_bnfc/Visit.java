package abs.frontend_bnfc;

import gen.ABS.Absyn.*;
import gen.ABS.VisitSkel;
/*** BNFC-Generated Visitor Design Pattern Skeleton. ***/
/* This implements the common visitor design pattern.
   Tests show it to be slightly less efficient than the
   instanceof method, but easier to use. 
   Replace the R and A parameters with the desired return
   and context types.*/

/* main class defined for all the non terminals entered in the grammar file*/
/*  Adding code for the compiler to interpret each of the above nodes */
//author: Chetan Nagarajagowda
public class Visit extends VisitSkel 
{
    // Identifiers (variables and types)
    //Node 1 
    // completed
  public class AnyIdentVisitor<R,A> implements AnyIdent.Visitor<R,A>
  {
      // each non terminal becomes a JAVA method in the skeleton
      // R is the java output
      // ABS is the input
      // A is just the string or argument
      // p is the object,should not be in the return value
      
    public Java.AnyIdent visit(ABS.Absyn.AnyIden p, A arg)
    {
      /* Code For AnyIden Goes Here */
        Java.Ident x = p.ident_.visit();
        return new Java.AnyIdent x;
       
    }
    public Java.AnyIdent visit(ABS.Absyn.AnyTyIden p, A arg)
    {
      /* Code For AnyTyIden Goes Here */

      //p.typeident_;
         
        Java.TypeIdent y = p.typeident_.visit();
        return new Java.AnyIdent y ;
	
	//return (new Java.AnyTyIden (p.typeident_));
    }

  }
   // Program (Entrypoints)
   // entry point code for visiting different module
   // visiting modules and returning the object to the parent node
   //Node 2
   // completed
  public class ProgramVisitor<R,A> implements Program.Visitor<R,A>
  {
    public Java.Program visit(ABS.Absyn.Prog p, A arg)
    {
      /* Code For Prog Goes Here */
	List<JavaModule> ms = empty();
      for (Module x : p.listmodule_) {
	  Java.Module m = x.visit();
          ms.append(m);
      }

      return new Java.Program(ms);
    }

  }
    // Modules(export&impor&declartions)
    //Node 3:
    // completed
  public class ModuleVisitor<R,A> implements Module.Visitor<R,A>
  {
       /* Code For Module Goes Here */
   
      public Java.Module visit(ABS.Absyn.Modul p, A arg)
      {
	  Java.QualType x = p.qualtype_.visit();
         p.qualtype_.accept(new QualTypeVisitor<R,A>(), arg);       
      /* Code For Module Goes Here */
        List<JavaExport> es = empty();
      
      for (Export x : p.listexport_) {
	  Java.Export i = x.visit();
          es.append(i);
      }
     
   
      /* Code For Module Goes Here */
        List<JavaImport> is = empty();
      for (Import y : p.listimport_) {
	  Java.Import n = y.visit();
          is.append(n);
      }
     
    
      /* Code For Module Goes Here */
        List<JavaDecl> ds = empty();
      for (Decl z : p.listdecl_) {
	  Java.Decl r = z.visit();
	  ds.append(r);
       }
      Java.MaybeBlock z = p.maybeblock_.visit();
      p.maybeblock_.accept(new MaybeBlockVisitor<R,A>(), arg);

      return new Java.Module (x,es,is,ds,z);
      }
  }
     

      // return ds;
      // return as;
      // return es;
      // return object what should be returned(as there are list of modules) not      //sure check with Behrooz
    
  
    // Exports()
    //Node 4
    //Completed
  public class ExportVisitor<R,A> implements Export.Visitor<R,A>
  {
      public Java.Export visit(ABS.Absyn.AnyExport p, A arg)
    {
      /* Code For AnyExport(AnyIdent) Goes Here */
	List<JavaAnyIdent> es = empty;
      for (AnyIdent x : p.listanyident_) {
	  Java.AnyIden i = x.visit();
          es.append(i);
      }

      return new Java.Export(es) ;
    }
    
 /* Code For AnyFromExport Goes Here */
    public Java.Export visit(ABS.Absyn.AnyFromExport p, A arg)
    {
      /* Code For AnyFromExport Goes Here */
	List<JavaAnyIdent> es = empty;
          for (AnyIdent x : p.listanyident_) {
	  Java.AnyIden i = x.visit();
          es.append(i);
           }

	  Java.QualType x = p.qualtype_.visit();
      // p.type_.accept(new TypeVisitor<R,A>(), arg);

      //return (Java.AnyFromExport (p.type_.accept( )));
      // cross verify with Behrooz and Nikolaous
	  // return (Java.AnyFromExport (p.export(anyident_(type_))));
	  return new Java.Export(es, x);
    }

  
    public Java.Export visit(ABS.Absyn.StarExport p, A arg)
    {
      /* Code For StarExport Goes Here */
        
	return null;
    }
    public Java.Export visit(ABS.Absyn.StarFromExport p, A arg)
    {
      /* Code For StarFromExport Goes Here */

	// p.type_.accept(new TypeVisitor<R,A>(), arg);

      // for(StarFromExport x : p.ExportVisitor(TypeVisitor)){
      //  Java.StarFromExport i = x.visit();
      // }

        Java.QualType x = p.qualtype_.visit();
        return ;
    }

  }
    // Imports()
    //Node 5
    //Completed
  public class ImportVisitor<R,A> implements Import.Visitor<R,A>
  {
    public Java.Import visit(ABS.Absyn.AnyImport p, A arg)
    {
      /* Code For AnyImport Goes Here */
	
      p.importtype_.accept(new ImportTypeVisitor<R,A>(), arg);
      p.type_.accept(new TypeVisitor<R,A>(), arg);
      p.anyident_.accept(new AnyIdentVisitor<R,A>(), arg);
         
      //For importtype
      Java.ImportType n = p.importtype_.visit();
      Java.Type x= p.qualtype_.visit();
      Java.AnyIdent y= p.anyident_.visit();
        
      return new Java.Import(n,x,y);
    }

    public Java.Import visit(ABS.Absyn.AnyFromImport p, A arg)
    {
      /* Code For AnyFromImport Goes Here */


	Java.ImportType e = p.importtype_.visit();
        p.importtype_.accept(new ImportTypeVisitor<R,A>(), arg);
	List<JavaAnyIdent> es = empty();
      for (AnyIdent x : p.listanyident_) {
	  Java.AnyIden i = x.visit();
          es.append(i);
      }
           Java.Type a =p.qualtype_.visit();
     
	   return new Java.Import (e,es,a);
    }
     
    
    public Java.Import visit(ABS.Absyn.StarFromImport p, A arg)
    {
      /* Code For StarFromImport Goes Here */

      p.importtype_.accept(new ImportTypeVisitor<R,A>(), arg);
      p.type_.accept(new TypeVisitor<R,A>(), arg);
      Java.ImportType x =p.importtype_.visit();
      Java.Type y = p.qualtype_.visit();
      return new Java.Import (x,y);
    }

  }
 // Import Types(Not part of Imports)
    //Node 6
    //Completed
  public class ImportTypeVisitor<R,A> implements ImportType.Visitor<R,A>
  {
    public Java.ImportType visit(ABS.Absyn.ForeignImport p, A arg)
    {
      /* Code For ForeignImport Goes Here */

        //did not find fimporttype_ check with Behrooz and Nikolaous
	//Java.fimport r = p.importtype_.visit();
	// return r;
	return null;
    }
    // this method not sure.keep it at last.
    public Java.ImportType visit(ABS.Absyn.NormalImport p, A arg)
    {
      /* Code For NormalImport Goes Here */

	// Java.import r = p.importtype_.visit();
	//	return r;
	return null;
    }

  }
    //Type valid names
    //Node 7
    //completed
  public class TypeVisitor<R,A> implements Type.Visitor<R,A>
  {
    public Java.Type visit(ABS.Absyn.TUnderscore p, A arg)
    {
      /* Code For UnderscoreType Goes Here */

	return null;
        }
    public Java.Type visit(ABS.Absyn.TSimple p, A arg)
    {
      /* Code For TSimple Goes Here */
	
	  Java.QualType i = p.qualtype_.visit();
	  return i;
      }

     
    }
    public Java.TGen visit(ABS.Absyn.TGen p, A arg)
    {
      /* Code For TGenType Goes Here */
	Java.QualType i =p.qualtype_.visit();
	List<JavaType> ms = empty();
      for (Type x : p.listtype_) {
           Java.Type i = x.visit();
           ms.append(i);
      }
      
      return new Java.TGen(i,ms);
    }

    //list wrapper TypeIdentifiers
    //Node 8
    //Completed
  public class QualTypevisitor<R,A> implements QualType.Visitor<R,A>
  {
    public Java.QType visit(ABS.Absyn.QType p, A arg)
    {
      /* Code For QType  Goes Here */
        List<JavaQualTypeSegment> ms = empty();
	for(QualTypeSegment x : p.listqualtypesegement_){
	    Java.QualTypeSegment i = x.visit();
	    ms.append(i);
      }
	return new Java.QType(ms);
    }


    public class QualTypeSegmentVisitor<R,A> implements QualTypeSegment.visitor<R,A>
	{
	    //Code for QTypeSegment goes here
	    public Java.QTypeSegment visit(ABS.Absyn.QTypeSegment p, A arg)
	    {
		Java.TypeIdent i = p.typeident_.visit();
                return i;
	    }

        
	}
    // Declarationsall(all comes under this main declaration visitor class wih     //methods for ADT,fucntions, interfaces and classes)
    //Node 9
    //Completed
  public class DeclVisitor<R,A> implements Decl.Visitor<R,A>
  {
    public Java.TypeDecl visit(ABS.Absyn.TypeDecl p, A arg)
    {
      /* Code For TypeDecl Goes Here */

      //p.typeident_;
      p.type_.accept(new TypeVisitor<R,A>(), arg);
      Java.TypeIdent r = p.typeident_.visit();
      Java.Type s =p.type_.visit();
      return new Java.TypeDecl(r,s);
    }

   public Java.ExceptionDecl visit(ABS.Absyn.ExceptionDecl p, A arg)
    {
      /* Code For ExceptionDecl Goes Here */

      //p.typeident_;
      p.constrident_.accept(new ConstrIdentVisitor<R,A>(), arg);
      Java.ConstrIdent r = p.constrident_.visit();

      return r;
    }

   
    public Java.DataDecl visit(ABS.Absyn.DataDecl p, A arg)
    {
      /* Code For DataDecl Goes Here */

      //p.typeident_;
	Java.TypeIdent t = p.typeident_.visit();
	List<JavaConstrIdent> ms = empty();
      for (ConstrIdent x : p.listconstrident_) {
	  Java.ConstrIdent i = x.visit();
          ms.append(i);
      }

      return new Java.DataDecl (t,ms);
    }
    public Java.DataParDecl visit(ABS.Absyn.DataParDecl p, A arg)
    {
      /* Code For DataParDecl Goes Here */

	List<JavaString> sx = empty();
        for (String x : p.listtypeident_) {
	   Java.String i = x.visit();
           sx.append(i);
        }

       List<JavaConstrIdent> cs = empty();
       for (ConstrIdent y : p.listconstrident_) {
	  Java.ConstrIdent i =y.visit();
          cs.append(i);
       }
       return new Java.DataParDecl (sx,cs);
    }

    //Declarations(Functions) continuations in the main class Declaration visitor
     public Java.FunDecl visit(ABS.Absyn.FunDecl p, A arg)
    {
      /* Code For FunDecl Goes Here */

      p.type_.accept(new TypeVisitor<R,A>(), arg);
      Java.Type i = p.type_.visit();
      Java.Ident m = p.ident_.visit();
      //p.ident_;
      List<JavaParam> ms = empty;
      for (Param x : p.listparam_) {
	  Java.Param i = x.visit();
          ms.append(i);
      }
      Java.FunBody n = p.funbody_.visit();
      p.funbody_.accept(new FunBodyVisitor<R,A>(), arg);
      return new Java.FunDecl(i,m,ms,n);
    }
    public Java.FunParDecl visit(ABS.Absyn.FunParDecl p, A arg)
    {
      /* Code For FunParDecl Goes Here */

      p.type_.accept(new TypeVisitor<R,A>(), arg);
      Java.Type a = p.type_.visit();
      Java.Ident b= p.ident_.visit();
      //p.ident_;
      List<JavaString> js = empty();
      for (String x : p.listtypeident_) {
	  Java.String i = x.visit();
          js.append(i);
      }
      List<JavaParam> ps = empty();
      for (Param x : p.listparam_) {
	  Java.Param i = x.visit();
          ps.append(i);
       }
      p.funbody_.accept(new FunBodyVisitor<R,A>(), arg);
      Java.FunBody g = p.funbody_.visit();

      return new Java.FunParDecl(a,b,js,ps,g);
    }


    public Java.InterfDecl visit(ABS.Absyn.InterfDecl p, A arg)
    {
      /* Code For InterfDecl Goes Here */

      //p.typeident_;
	Java.TypeIdent i = p.typeident_.visit();
	List<JavaMethSignat> ms = empty();
      for (MethSignat x : p.listmethsignat_) {
	  Java.MethSignat m = x.visit();
	  ms.append(m);
      }

      return new Java.InterfDecl(i,ms);
    }
    public Java.ExtendsDecl visit(ABS.Absyn.ExtendsDecl p, A arg)
    {
      /* Code For ExtendsDecl Goes Here */

      //p.typeident_;
     Java.TypeIdent a = p.typeident_.visit();
      List<JavaQualType> qs = empty();
       for (QualType x : p.listqualtype_) {
	   Java.QualType i = x.visit();
           qs.append(i);
      }
        List<JavaMethSignat> ms = empty();
      for (MethSignat x : p.listmethsignat_) {
	  Java.MethSignat m = x.visit();
	  ms.append(m);
      }

      return new Java.ExtendsDecl(a,qs,ms);
    }
    public Java.ClassDecl visit(ABS.Absyn.ClassDecl p, A arg)
    {
      /* Code For ClassDecl Goes Here */

      //p.typeident_;
	Java.TypeIdent c = p.typeident_.visit();

	List<JavaClassBody> cs = empty();
      for (ClassBody x : p.listclassbody_1) {
	  Java.ClassBody i =x.visit();
	  cs.append(i);
        }

      p.maybeblock_.accept(new MaybeBlockVisitor<R,A>(), arg);
      Java.MaybeBlock m = p.maybeblock_.visit();

      List<JavaClassBody> ds = empty();
      for (ClassBody x : p.listclassbody_2) {
	  Java.ClassBody n = x.visit();
          ds.append(n);
         }

      return new Java.ClassDecl(c,cs,m,ds);
    }
   
    public Java.ClassParamDecl visit(ABS.Absyn.ClassParamDecl p, A arg)
    {
      /* Code For ClassParamDecl Goes Here */

      //p.typeident_;
        
      Java.TypeIdent c = p.typeident_.visit();
      List<JavaParam> ps = empty();
      for (Param x : p.listparam_) {
	  Java.Param i = x.visit();
	  ps.append(i);
        }
      List<JavaClassBody> cs = empty();
      for (ClassBody x : p.listclassbody_1) {
	  Java.ClassBody i = x.visit();
          cs.append(i);
      }

      p.maybeblock_.accept(new MaybeBlockVisitor<R,A>(), arg);
      Java.MaybeBlock m = p.maybeblock_.visit();
      
      List<JavaClassBody> fs = empty();
      for (ClassBody x : p.listclassbody_2) {
	  Java.ClassBody i = x.visit();
	  fs.append(i);
     }

      return new Java.ClassParamDecl(c, ps, cs, m, fs);
    }
    public Java.ClassImplements visit(ABS.Absyn.ClassImplements p, A arg)
    {
      /* Code For ClassImplements Goes Here */

      //p.typeident_;
	Java.TypeIdent t = p.typeident_.visit();
 
	List<JavaQualType> qs = empty();
      for (QualType x : p.listqualtype_) {
	  Java.QualType i = x.visit();
          qs.append(i);
      }
      List<JavaClassBody> gs = empty();
      for (ClassBody x : p.listclassbody_1) {
	  Java.ClassBody i = x.visit();
          gs.append(i);
      }
      p.maybeblock_.accept(new MaybeBlockVisitor<R,A>(), arg);
      Java.MaybeBlock m = p.maybeblock_.visit();
      List<ClassBody> hs = empty();
       for (ClassBody x : p.listclassbody_2) {
	   Java.ClassBody i = x.visit();
	   hs.append(i);
       }

       return new Java.ClassImplements(t, qs, gs, m, hs);
    }
  
    public Java.ClassParamImplements visit(ABS.Absyn.ClassParamImplements p, A arg)
    {
      /* Code For ClassParamImplements Goes Here */

      //p.typeident_;
	Java.TypeIdent t = p.typeident_.visit();
    
	List<JavaParam> pi = empty();
      for (Param x : p.listparam_) {
	  Java.Param i = x.visit();
	  pi.append(i);
      }
     
      List<JavaQualType> qs = empty();
      for (QualType x : p.listqualtype_) {
	  Java.QualType i = x.visit();
	  qs.append(i);
       }
      List<JavaClassBody> cs = empty();
      for (ClassBody x : p.listclassbody_1) {
	  Java.ClassBody i = x.visit();
	  cs. append(i);
      }
      p.maybeblock_.accept(new MaybeBlockVisitor<R,A>(), arg);
      Java.MaybeBlock n = p.maybeblock_.visit();

      List<JavaClassBody> ds = empty();
      for (ClassBody x : p.listclassbody_2) {
	  Java.ClassBody i = x.visit();
          ds.append(i);
      }

      return new Java.ClassParamImplements(t, pi, qs, cs, n, ds);
    }

  }

    //Node 11
  // Declarations(ADT)
  // Completed

     public class ConstrIdentVisitor<R,A> implements ConstrIdent.Visitor<R,A>
  {
    public Java.SingleConstrIdent visit(ABS.Absyn.SinglConstrIdent p, A arg)
    {
      /* Code For SinglConstrIdent Goes Here */

      //p.typeident_;
	Java.TypeIdent i = p.typeident_.visit();

      return i;
    }
    public Java.ParamConstrIdent visit(ABS.Absyn.ParamConstrIdent p, A arg)
    {
      /* Code For ParamConstrIdent Goes Here */

      //p.typeident_;
        Java.TypeIdent r = p.typeident_.visit();
	List<ConstrType> ct = empty();
      for (ConstrType x : p.listconstrtype_) {
	  Java.ConstrType i = x.visit();
          ct.append(i);
      }

      return new Java.ParamConstrIdent (r, ct);
    }

  }
  public class ConstrTypeVisitor<R,A> implements ConstrType.Visitor<R,A>
  {
    public Java.EmptyConstrType visit(ABS.Absyn.EmptyConstrType p, A arg)
    {
      /* Code For EmptyConstrType Goes Here */

      p.type_.accept(new TypeVisitor<R,A>(), arg);
      Java.Type i= p.type_.visit();
      return i;
    }
    public Java.RecordConstrType visit(ABS.Absyn.RecordConstrType p, A arg)
    {
      /* Code For RecordConstrType Goes Here */

      p.type_.accept(new TypeVisitor<R,A>(), arg);
      //p.ident_;
      Java.Type y = p.type_.visit();
      Java.Ident x = p.ident_.visit();
      return new Java.RecordConstrType(y,x);
    }

  }
  //Node 11
   //declarations(Built &Normal)
   
  public class FunBodyVisitor<R,A> implements FunBody.Visitor<R,A>
  {
    public Java.BuiltinFunBody visit(ABS.Absyn.BuiltinFunBody p, A arg)
    {
      /* Code For BuiltinFunBody Goes Here */


	//Check with Behrooz. not sure with builtin method for ABS
	//return (Java.BuiltinFunBody (p.Built);
	return null;
    }
    public Java.NormalFunBody visit(ABS.Absyn.NormalFunBody p, A arg)
    {
      /* Code For NormalFunBody Goes Here */

      p.pureexp_.accept(new PureExpVisitor<R,A>(), arg);
      Java.PureExp s =p.pureexp_.visit();
      return s;
    }

  }
    //Declarations(MethodsignatVisitor)
    //Node 12
  public class MethSignatVisitor<R,A> implements MethSignat.Visitor<R,A>
  {
    public Java.MethSig visit(ABS.Absyn.MethSig p, A arg)
    {
      /* Code For MethSig Goes Here */

      p.type_.accept(new TypeVisitor<R,A>(), arg);
      Java.Type i = p.type_.visit();
      //p.ident_;
      Java.Ident n =p.ident_.visit();
      List<JavaParam> ps = empty();
      for (Param x : p.listparam_) {
       Java.Param n = x.visit();
       ps.append(n);
      }

      return new Java.MethSig(i, n, ps);
    }

  }


    //Declarations(Class bodys)
    // node 13
  public class ClassBodyVisitor<R,A> implements ClassBody.Visitor<R,A>
  {
    public Java.FieldClassBody visit(ABS.Absyn.FieldClassBody p, A arg)
    {
	Java.Type i = p.type_.visit();
        Java.Ident n = p.ident_.visit();
	return new Java.FieldClassBody(i,n);
    }
    public Java.FieldAssignClassBody visit(ABS.Absyn.FieldAssignClassBody p, A arg)
    {
      /* Code For FieldAssignClassBody Goes Here */
        Java.Type i = p.type_.visit();
        Java.Ident n = p.ident_.visit();
        Java.PureExp m = p.pureexp_.visit();
	return new Java.FieldAssignClassBody(i,n,m);
     
    }
    public Java.MethClassBody visit(ABS.Absyn.MethClassBody p, A arg)
    {
      /* Code For MethClassBody Goes Here */

      p.type_.accept(new TypeVisitor<R,A>(), arg);
      Java.Type n = p.type_.visit();
      //p.ident_;
      List<JavaParam> as = empty();
      for (Param x : p.listparam_) {
	  Java. Param i = x.visit();
          as.append(i);
       }
      p.block_.accept(new BlockVisitor<R,A>(), arg);
      Java.Block r = p.block_.visit();
      return new Java.MethClassBody(n, as, r );
    }

  }
    // Blocks
    //Node 14
  public class BlockVisitor<R,A> implements Block.Visitor<R,A>
  {
      public Java.Bloc  visit(ABS.Absyn.Bloc p, A arg)
    {
      /* Code For Bloc Goes Here */
	List <JavaStm> ms = empty;
      for (Stm x : p.liststm_) {
	  Java.Stm i = x.visit();
          ms.append(i);
      }

      return new Java(ms);
    }

  }
    // JustBlocks & NoBlocks
    //Node 15
  public class MaybeBlockVisitor<R,A> implements MaybeBlock.Visitor<R,A>
  {
    public Java.JustBlock visit(ABS.Absyn.JustBlock p, A arg)
    {
      /* Code For JustBlock Goes Here */

      p.block_.accept(new BlockVisitor<R,A>(), arg);
      Java.Block r = p.block_.visit();
      return r;
    }
    public Java.NoBlock visit(ABS.Absyn.NoBlock p, A arg)
    {
      /* Code For NoBlock Goes Here */

        return null;
    }

  }
    // Formal Paramaters to functions
    // Node 16
  public class ParamVisitor<R,A> implements Param.Visitor<R,A>
  {
    public Java.Par  visit(ABS.Absyn.Par p, A arg)
    {
      /* Code For Par Goes Here */

      p.type_.accept(new TypeVisitor<R,A>(), arg);
      //p.ident_;
      Java.Type b = p.type_.visit();
      Java.Ident c =p.type_.visit();
      return new Java.Par (b,c);
    }

  }
    //Statements
    //Node 17
  public class StmVisitor<R,A> implements Stm.Visitor<R,A>
  {
    public Java.SExp visit(ABS.Absyn.SExp p, A arg)
    {
      /* Code For SExp Goes Here */

      p.exp_.accept(new ExpVisitor<R,A>(), arg);
      Java.Exp k = p.exp_.visit();
      return k;
    }
    public Java.SBlock visit(ABS.Absyn.SBlock p, A arg)
    {
      /* Code For SBlock Goes Here */
	List<JavaStm> sg = empty();
      for (Stm x : p.liststm_) {
	  Java.Stm i = x.visit();
          sg.append(i);
      }

      return sg;
    }
    public Java.SWhile visit(ABS.Absyn.SWhile p, A arg)
    {
      /* Code For SWhile Goes Here */

      p.pureexp_.accept(new PureExpVisitor<R,A>(), arg);
      p.stm_.accept(new StmVisitor<R,A>(), arg);
      Java.PureExp k = p.pureexp_.visit();
      Java.Stm i = p.stm_.visit();
      return new Java.SWhile(k,i);
    }
    public Java.SReturn visit(ABS.Absyn.SReturn p, A arg)
    {
      /* Code For SReturn Goes Here */

      p.exp_.accept(new ExpVisitor<R,A>(), arg);
      Java.Exp a = p.exp_.visit();
      return a;
    }
    public Java.SAss visit(ABS.Absyn.SAss p, A arg)
    {
      /* Code For SAss Goes Here */

      //p.ident_;
      p.exp_.accept(new ExpVisitor<R,A>(), arg);
      Java.Ident s = p.ident_.visit();
      Java.Exp n = p.exp_.visit();
      return new Java.SAss(s,n);
    }
    public Java.SFieldAss visit(ABS.Absyn.SFieldAss p, A arg)
    {
      /* Code For SFieldAss Goes Here */

      //p.ident_;
      p.exp_.accept(new ExpVisitor<R,A>(), arg);
      Java.Ident l = p.ident_.visit();
      Java.Exp m = p.exp_.visit();
      return new Java.SFieldAss(l,m);
    }
    public Java.SDec visit(ABS.Absyn.SDec p, A arg)
    {
      /* Code For SDec Goes Here */

      p.type_.accept(new TypeVisitor<R,A>(), arg);
      //p.ident_;
      Java.Type a = p.type_.visit();
      Java.Ident b = p.ident_.visit();

      return new Java.SDec(a,b);
    }
    public Java.SDecAss visit(ABS.Absyn.SDecAss p, A arg)
    {
      /* Code For SDecAss Goes Here */
      Java.Type g = p.type_.visit();
      Java.Ident h = p.ident_.visit();
      Java.Exp j = p.exp_.visit();
      p.type_.accept(new TypeVisitor<R,A>(), arg);
      //p.ident_;
      p.exp_.accept(new ExpVisitor<R,A>(), arg);

      return new Java.SDecAss(g,h,j);
    }
    public Java.SIf visit(ABS.Absyn.SIf p, A arg)
    {
      /* Code For SIf Goes Here */

      p.pureexp_.accept(new PureExpVisitor<R,A>(), arg);
      p.stm_.accept(new StmVisitor<R,A>(), arg);
      Java.PureExp a= p.pureexp_.visit();
      Java.Stm b = p.stm_.visit();

      return new Java.SIf(a,b);
    }
    public Java.SIfElse visit(ABS.Absyn.SIfElse p, A arg)
    {
      /* Code For SIfElse Goes Here */

      p.pureexp_.accept(new PureExpVisitor<R,A>(), arg);
      p.stm_1.accept(new StmVisitor<R,A>(), arg);
      p.stm_2.accept(new StmVisitor<R,A>(), arg);
      Java.PureExp m =p.pureexp_.visit();
      Java.Stm n =p.stm_1.visit();
      Java.Stm o =p.stm_2.visit();
      return new Java.SIfElse(m,n,o);
    }
    public Java.SSuspend visit(ABS.Absyn.SSuspend p, A arg)
    {
      /* Code For SSuspend Goes Here */


      return null;
    }
    public Java.SSkip visit(ABS.Absyn.SSkip p, A arg)
    {
      /* Code For SSkip Goes Here */


      return null;
    }
    public Java.SAssert visit(ABS.Absyn.SAssert p, A arg)
    {
      /* Code For SAssert Goes Here */

      p.pureexp_.accept(new PureExpVisitor<R,A>(), arg);
      Java.PureExp a = p.pureexp_.visit();
      return a;
    }
    public Java.SAwait visit(ABS.Absyn.SAwait p, A arg)
    {
      /* Code For SAwait Goes Here */

      p.guard_.accept(new GuardVisitor<R,A>(), arg);
      Java.Guard b = p.guard_.visit();
      return b;
    }

    public Java.SThrow visit(ABS.Absyn.SThrow p, A arg)
    {
      /* Code For SThrow Goes Here */

      p.pureexp_.accept(new PureExpVisitor<R,A>(), arg);
      Java.PureExp i = p.pureexp_.visit();
      return i;
    }
    public Java.STryCatchFinally visit(ABS.Absyn.STryCatchFinally p, A arg)
    {
      /* Code For STryCatchFinally Goes Here */

      p.stm_.accept(new StmVisitor<R,A>(), arg);
      Java.Stm m = p.stm_.visit();

      List<JavaCatchBranch> cb = empty();
      for (CatchBranch x : p.listcatchbranch_) {
	  Java.CatchBranch i = x.visit();
	  cb.append(i);
       }
      
      p.maybefinally_.accept(new MaybeFinallyVisitor<R,A>(), arg);
      Java.MaybeFinally f = p.maybefinally_.visit();
      return new Java.STryCatchFinally (m, cb, f );
    }

  }

public class CatchBranchVisitor<R,A> implements CatchBranch.Visitor<R,A>
  {
    public Java.CatchBranc visit(ABS.Absyn.CatchBranc p, A arg)
    {
      /* Code For CatchBranc Goes Here */
	Java.Pattern e = p.pattern_.visit();
        p.pattern_.accept(new PatternVisitor<R,A>(), arg);
	Java.Stm j = p.stm_.visit();
      p.stm_.accept(new StmVisitor<R,A>(), arg);

      return new Java.CatchBranc(e,j);
    }

  }
  public class MaybeFinallyVisitor<R,A> implements MaybeFinally.Visitor<R,A>
  {
    public JustFinally visit(ABS.Absyn.JustFinally p, A arg)
    {
      /* Code For JustFinally Goes Here */

      p.stm_.accept(new StmVisitor<R,A>(), arg);
      Java.Stm a = p.stm_.visit();
      return a;
    }
    public Java.NoFinally visit(ABS.Absyn.NoFinally p, A arg)
    {
      /* Code For NoFinally Goes Here */
      return null;
    }

  }

    //Await guards
    // Node 18
  public class GuardVisitor<R,A> implements Guard.Visitor<R,A>
  {
    public Java.Guard visit(ABS.Absyn.VarGuard p, A arg)
    {
      /* Code For VarGuard Goes Here */

      //p.ident_;
        Java.Ident a = p.ident_.visit();
	return new Java.Guard (a);

	// the other way is
        Java
    }
    public Java.Guard visit(ABS.Absyn.FieldGuard p, A arg)
    {
      /* Code For FieldGuard Goes Here */

      //p.ident_;
        Java.Ident b =p.ident_.visit();
	return b;
    }
    public Java.Guard visit(ABS.Absyn.ExpGuard p, A arg)
    {
      /* Code For ExpGuard Goes Here */

      p.pureexp_.accept(new PureExpVisitor<R,A>(), arg);
      Java.PureExp m = p.pureexp_.visit();
      return new Java.Guard(m);
    }
    public Java.AndGuard visit(ABS.Absyn.AndGuard p, A arg)
    {
      /* Code For AndGuard Goes Here */

      p.guard_1.accept(new GuardVisitor<R,A>(), arg);
      p.guard_2.accept(new GuardVisitor<R,A>(), arg);
      Java.Guard m =p.guard_1.visit();
      Java.Guard n =p.guard_2.visit();

      return new Java.AndGuard(m,n);
    }

  }
    //Expressions
    //Node 19
  public class ExpVisitor<R,A> implements Exp.Visitor<R,A>
  {
    public Java.Exp  visit(ABS.Absyn.ExpP p, A arg)
    {
      /* Code For ExpP Goes Here */

      p.pureexp_.accept(new PureExpVisitor<R,A>(), arg);
      Java.PureExp m = p.pureexp_();
      return new Java.Exp (m);
      
    }

    public Java.PureExp visit (ABS.Absyn.ExpP p, A arg)
    {
	Java.PureExp m = p1.pureexp_1.visit();
        Java.PureExp1 n =p2.pureexp_2.visit();
	return new Java.PureExp(m,n);
    }
    public Java.PureExp1 visit(ABS.Absyn.ExpE p, A arg)
    {
      /* Code For ExpE Goes Here */

      p.effexp_.accept(new EffExpVisitor<R,A>(), arg);
      Java.PureExp1 m = p1.pureexp_1
      return (Java.ExpE (p.effexp_));
    }

  }
    //Pure Expressions
    // Node 20
  public class PureExpVisitor<R,A> implements PureExp.Visitor<R,A>
  {
    public R visit(ABS.Absyn.EOr p, A arg)
    {
      /* Code For EOr Goes Here */

      p.pureexp_1.accept(new PureExpVisitor<R,A>(), arg);
      p.pureexp_2.accept(new PureExpVisitor<R,A>(), arg);

      return null;
    }
    public R visit(ABS.Absyn.Let p, A arg)
    {
      /* Code For Let Goes Here */

      p.param_.accept(new ParamVisitor<R,A>(), arg);
      p.pureexp_1.accept(new PureExpVisitor<R,A>(), arg);
      p.pureexp_2.accept(new PureExpVisitor<R,A>(), arg);

      return null;
    }
    public R visit(ABS.Absyn.If p, A arg)
    {
      /* Code For If Goes Here */

      p.pureexp_1.accept(new PureExpVisitor<R,A>(), arg);
      p.pureexp_2.accept(new PureExpVisitor<R,A>(), arg);
      p.pureexp_3.accept(new PureExpVisitor<R,A>(), arg);

      return null;
    }
    public R visit(ABS.Absyn.Case p, A arg)
    {
      /* Code For Case Goes Here */

      p.pureexp_.accept(new PureExpVisitor<R,A>(), arg);
      for (CaseBranch x : p.listcasebranch_) {
      }

      return null;
    }
    public R visit(ABS.Absyn.EAnd p, A arg)
    {
      /* Code For EAnd Goes Here */

      p.pureexp_1.accept(new PureExpVisitor<R,A>(), arg);
      p.pureexp_2.accept(new PureExpVisitor<R,A>(), arg);

      return null;
    }
    public R visit(ABS.Absyn.EEq p, A arg)
    {
      /* Code For EEq Goes Here */

      p.pureexp_1.accept(new PureExpVisitor<R,A>(), arg);
      p.pureexp_2.accept(new PureExpVisitor<R,A>(), arg);

      return null;
    }
    public R visit(ABS.Absyn.ENeq p, A arg)
    {
      /* Code For ENeq Goes Here */

      p.pureexp_1.accept(new PureExpVisitor<R,A>(), arg);
      p.pureexp_2.accept(new PureExpVisitor<R,A>(), arg);

      return null;
    }
    public R visit(ABS.Absyn.ELt p, A arg)
    {
      /* Code For ELt Goes Here */

      p.pureexp_1.accept(new PureExpVisitor<R,A>(), arg);
      p.pureexp_2.accept(new PureExpVisitor<R,A>(), arg);

      return null;
    }
    public R visit(ABS.Absyn.ELe p, A arg)
    {
      /* Code For ELe Goes Here */

      p.pureexp_1.accept(new PureExpVisitor<R,A>(), arg);
      p.pureexp_2.accept(new PureExpVisitor<R,A>(), arg);

      return null;
    }
    public R visit(ABS.Absyn.EGt p, A arg)
    {
      /* Code For EGt Goes Here */

      p.pureexp_1.accept(new PureExpVisitor<R,A>(), arg);
      p.pureexp_2.accept(new PureExpVisitor<R,A>(), arg);

      return null;
    }
    public R visit(ABS.Absyn.EGe p, A arg)
    {
      /* Code For EGe Goes Here */

      p.pureexp_1.accept(new PureExpVisitor<R,A>(), arg);
      p.pureexp_2.accept(new PureExpVisitor<R,A>(), arg);

      return null;
    }
    public R visit(ABS.Absyn.EAdd p, A arg)
    {
      /* Code For EAdd Goes Here */

      p.pureexp_1.accept(new PureExpVisitor<R,A>(), arg);
      p.pureexp_2.accept(new PureExpVisitor<R,A>(), arg);

      return null;
    }
    public R visit(ABS.Absyn.ESub p, A arg)
    {
      /* Code For ESub Goes Here */

      p.pureexp_1.accept(new PureExpVisitor<R,A>(), arg);
      p.pureexp_2.accept(new PureExpVisitor<R,A>(), arg);

      return null;
    }
    public R visit(ABS.Absyn.EMul p, A arg)
    {
      /* Code For EMul Goes Here */

      p.pureexp_1.accept(new PureExpVisitor<R,A>(), arg);
      p.pureexp_2.accept(new PureExpVisitor<R,A>(), arg);

      return null;
    }
    public R visit(ABS.Absyn.EDiv p, A arg)
    {
      /* Code For EDiv Goes Here */

      p.pureexp_1.accept(new PureExpVisitor<R,A>(), arg);
      p.pureexp_2.accept(new PureExpVisitor<R,A>(), arg);

      return null;
    }
    public R visit(ABS.Absyn.EMod p, A arg)
    {
      /* Code For EMod Goes Here */

      p.pureexp_1.accept(new PureExpVisitor<R,A>(), arg);
      p.pureexp_2.accept(new PureExpVisitor<R,A>(), arg);

      return null;
    }
    public R visit(ABS.Absyn.ELogNeg p, A arg)
    {
      /* Code For ELogNeg Goes Here */

      p.pureexp_.accept(new PureExpVisitor<R,A>(), arg);

      return null;
    }
    public R visit(ABS.Absyn.EIntNeg p, A arg)
    {
      /* Code For EIntNeg Goes Here */

      p.pureexp_.accept(new PureExpVisitor<R,A>(), arg);

      return null;
    }
    public R visit(ABS.Absyn.EFunCall p, A arg)
    {
      /* Code For EFunCall Goes Here */

      //p.ident_;
      for (PureExp x : p.listpureexp_) {
      }

      return null;
    }
    public R visit(ABS.Absyn.EQualFunCall p, A arg)
    {
      /* Code For EQualFunCall Goes Here */

      p.type_.accept(new TypeVisitor<R,A>(), arg);
      //p.ident_;
      for (PureExp x : p.listpureexp_) {
      }

      return null;
    }
    public R visit(ABS.Absyn.ENaryFunCall p, A arg)
    {
      /* Code For ENaryFunCall Goes Here */

      //p.ident_;
      for (PureExp x : p.listpureexp_) {
      }

      return null;
    }
    public R visit(ABS.Absyn.ENaryQualFunCall p, A arg)
    {
      /* Code For ENaryQualFunCall Goes Here */

      p.type_.accept(new TypeVisitor<R,A>(), arg);
      //p.ident_;
      for (PureExp x : p.listpureexp_) {
      }

      return null;
    }
    public R visit(ABS.Absyn.EVar p, A arg)
    {
      /* Code For EVar Goes Here */

      //p.ident_;

      return null;
    }
    public R visit(ABS.Absyn.EThis p, A arg)
    {
      /* Code For EThis Goes Here */

      //p.ident_;

      return null;
    }
    public R visit(ABS.Absyn.EQualVar p, A arg)
    {
      /* Code For EQualVar Goes Here */

      p.type_.accept(new TypeVisitor<R,A>(), arg);
      //p.ident_;

      return null;
    }
    public R visit(ABS.Absyn.ESinglConstr p, A arg)
    {
      /* Code For ESinglConstr Goes Here */

      p.type_.accept(new TypeVisitor<R,A>(), arg);

      return null;
    }
    public R visit(ABS.Absyn.EParamConstr p, A arg)
    {
      /* Code For EParamConstr Goes Here */

      p.type_.accept(new TypeVisitor<R,A>(), arg);
      for (PureExp x : p.listpureexp_) {
      }

      return null;
    }
    public R visit(ABS.Absyn.ELit p, A arg)
    {
      /* Code For ELit Goes Here */

      p.literal_.accept(new LiteralVisitor<R,A>(), arg);

      return null;
    }

  }
    //Cases  
  //Node 21
  public class CaseBranchVisitor<R,A> implements CaseBranch.Visitor<R,A>
  {
    public R visit(ABS.Absyn.CaseBranc p, A arg)
    {
      /* Code For CaseBranc Goes Here */

      p.pattern_.accept(new PatternVisitor<R,A>(), arg);
      p.pureexp_.accept(new PureExpVisitor<R,A>(), arg);

      return null;
    }

  }
    // Need to start from this node today
  //Pattern matching
    //Node 22
  public class PatternVisitor<R,A> implements Pattern.Visitor<R,A>
  {
    public Java.Ident visit(ABS.Absyn.IdentPat p, A arg)
    {
      /* Code For IdentPat Goes Here */
        Java.Ident r = p.ident_.visit();
       
      //p.ident_;

	return r;
    }
    public Java.Literal visit(ABS.Absyn.LitPat p, A arg)
    {
      /* Code For LitPat Goes Here */

      p.literal_.accept(new LiteralVisitor<R,A>(), arg);
      Java.Literal r = p.literal_.visit();
      return r;
    }
    public Java.TypeIdent visit(ABS.Absyn.SinglConstrPat p, A arg)
    {
      /* Code For SinglConstrPat Goes Here */

      //p.typeident_;
        Java.TypeIdent r = p.typeident_.visit();
	return r;
    }
    public List<JavaPattern> visit(ABS.Absyn.ParamConstrPat p, A arg)
    {
      /* Code For ParamConstrPat Goes Here */

      //p.typeident_;
	List<JavaPattern> pr = empty;
      for (Pattern x : p.listpattern_) {
	  Java.Pattern i = x.visit();  
	  pr.append(i);
    }

      return (pr);
    }
    public R visit(ABS.Absyn.UnderscorePat p, A arg)
    {
      /* Code For UnderscorePat Goes Here */


	return (Java.UnderscorePat (p.under));
    }

  }
    //Literals   
 //Node 23
  public class LiteralVisitor<R,A> implements Literal.Visitor<R,A>
  {
    public Java.LNull visit(ABS.Absyn.LNull p, A arg)
    {
      /* Code For LNull Goes Here */

      	return null;
    }
    public Java.LThis visit(ABS.Absyn.LThis p, A arg)
    {
      /* Code For LThis Goes Here */


	return null;
    }
    public LThisDC visit(ABS.Absyn.LThisDC p, A arg)
    {
      /* Code For LThisDC Goes Here */

// no options provided for this node, check with behrooz equivalent in ABS 
	//return (Java.LThisDC (p.thisDC));
	return null;
    }
    public Java.String visit(ABS.Absyn.LStr p, A arg)
    {
      /* Code For LStr Goes Here */
        Java.String r = p.string_.visit();

      //p.string_;

	return r;
    }
    public Java.Integer visit(ABS.Absyn.LInt p, A arg)
    {
      /* Code For LInt Goes Here */

      //p.integer_;
        Java.Integer r =p.integer_.visit();
	return r;
    }

  }
    //effectful expressions    
    // Node 24
  public class EffExpVisitor<R,A> implements EffExp.Visitor<R,A>
  {
    public  visit(ABS.Absyn.New p, A arg)
    {
      /* Code For New Goes Here */
	
      p.type_.accept(new TypeVisitor<R,A>(), arg);
      List<JavaPureExp>
      for (PureExp x : p.listpureexp_) {
      }

      return null;
    }
    public R visit(ABS.Absyn.NewLocal p, A arg)
    {
      /* Code For NewLocal Goes Here */

      p.type_.accept(new TypeVisitor<R,A>(), arg);
      for (PureExp x : p.listpureexp_) {
      }

      return null;
    }
    public R visit(ABS.Absyn.SyncMethCall p, A arg)
    {
      /* Code For SyncMethCall Goes Here */

      p.pureexp_.accept(new PureExpVisitor<R,A>(), arg);
      //p.ident_;
      for (PureExp x : p.listpureexp_) {
      }

      return null;
    }
    public R visit(ABS.Absyn.ThisSyncMethCall p, A arg)
    {
      /* Code For ThisSyncMethCall Goes Here */

      //p.ident_;
      for (PureExp x : p.listpureexp_) {
      }

      return null;
    }
    public R visit(ABS.Absyn.AsyncMethCall p, A arg)
    {
      /* Code For AsyncMethCall Goes Here */

      p.pureexp_.accept(new PureExpVisitor<R,A>(), arg);
      //p.ident_;
      for (PureExp x : p.listpureexp_) {
      }

      return null;
    }
    public R visit(ABS.Absyn.ThisAsyncMethCall p, A arg)
    {
      /* Code For ThisAsyncMethCall Goes Here */

      //p.ident_;
      for (PureExp x : p.listpureexp_) {
      }

      return null;
    }
    public R visit(ABS.Absyn.Get p, A arg)
    {
      /* Code For Get Goes Here */

      p.pureexp_.accept(new PureExpVisitor<R,A>(), arg);

      return null;
    }
    public R visit(ABS.Absyn.Spawns p, A arg)
    {
      /* Code For Spawns Goes Here */

      p.pureexp_.accept(new PureExpVisitor<R,A>(), arg);
      p.type_.accept(new TypeVisitor<R,A>(), arg);
      for (PureExp x : p.listpureexp_) {
      }

      return null;
    }

  }
       

}
