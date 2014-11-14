package gen.ABS;
import gen.ABS.Absyn.*;

public class PrettyPrinter
{
  //For certain applications increasing the initial size of the buffer may improve performance.
  private static final int INITIAL_BUFFER_SIZE = 128;
  //You may wish to change the parentheses used in precedence.
  private static final String _L_PAREN = new String("(");
  private static final String _R_PAREN = new String(")");
  //You may wish to change render
  private static void render(String s)
  {
    if (s.equals("{"))
    {
       buf_.append("\n");
       indent();
       buf_.append(s);
       _n_ = _n_ + 2;
       buf_.append("\n");
       indent();
    }
    else if (s.equals("(") || s.equals("["))
       buf_.append(s);
    else if (s.equals(")") || s.equals("]"))
    {
       backup();
       buf_.append(s);
       buf_.append(" ");
    }
    else if (s.equals("}"))
    {
       _n_ = _n_ - 2;
       backup();
       backup();
       buf_.append(s);
       buf_.append("\n");
       indent();
    }
    else if (s.equals(","))
    {
       backup();
       buf_.append(s);
       buf_.append(" ");
    }
    else if (s.equals(";"))
    {
       backup();
       buf_.append(s);
       buf_.append("\n");
       indent();
    }
    else if (s.equals("")) return;
    else
    {
       buf_.append(s);
       buf_.append(" ");
    }
  }


  //  print and show methods are defined for each category.
  public static String print(gen.ABS.Absyn.AnyIdent foo)
  {
    pp(foo, 0);
    trim();
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String show(gen.ABS.Absyn.AnyIdent foo)
  {
    sh(foo);
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String print(gen.ABS.Absyn.ListAnyIdent foo)
  {
    pp(foo, 0);
    trim();
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String show(gen.ABS.Absyn.ListAnyIdent foo)
  {
    sh(foo);
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String print(gen.ABS.Absyn.Program foo)
  {
    pp(foo, 0);
    trim();
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String show(gen.ABS.Absyn.Program foo)
  {
    sh(foo);
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String print(gen.ABS.Absyn.ListModule foo)
  {
    pp(foo, 0);
    trim();
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String show(gen.ABS.Absyn.ListModule foo)
  {
    sh(foo);
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String print(gen.ABS.Absyn.Module foo)
  {
    pp(foo, 0);
    trim();
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String show(gen.ABS.Absyn.Module foo)
  {
    sh(foo);
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String print(gen.ABS.Absyn.Export foo)
  {
    pp(foo, 0);
    trim();
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String show(gen.ABS.Absyn.Export foo)
  {
    sh(foo);
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String print(gen.ABS.Absyn.ListExport foo)
  {
    pp(foo, 0);
    trim();
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String show(gen.ABS.Absyn.ListExport foo)
  {
    sh(foo);
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String print(gen.ABS.Absyn.Import foo)
  {
    pp(foo, 0);
    trim();
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String show(gen.ABS.Absyn.Import foo)
  {
    sh(foo);
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String print(gen.ABS.Absyn.ListImport foo)
  {
    pp(foo, 0);
    trim();
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String show(gen.ABS.Absyn.ListImport foo)
  {
    sh(foo);
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String print(gen.ABS.Absyn.ImportType foo)
  {
    pp(foo, 0);
    trim();
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String show(gen.ABS.Absyn.ImportType foo)
  {
    sh(foo);
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String print(gen.ABS.Absyn.Type foo)
  {
    pp(foo, 0);
    trim();
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String show(gen.ABS.Absyn.Type foo)
  {
    sh(foo);
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String print(gen.ABS.Absyn.ListType foo)
  {
    pp(foo, 0);
    trim();
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String show(gen.ABS.Absyn.ListType foo)
  {
    sh(foo);
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String print(gen.ABS.Absyn.ListQualType foo)
  {
    pp(foo, 0);
    trim();
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String show(gen.ABS.Absyn.ListQualType foo)
  {
    sh(foo);
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String print(gen.ABS.Absyn.QualType foo)
  {
    pp(foo, 0);
    trim();
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String show(gen.ABS.Absyn.QualType foo)
  {
    sh(foo);
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String print(gen.ABS.Absyn.QualTypeSegment foo)
  {
    pp(foo, 0);
    trim();
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String show(gen.ABS.Absyn.QualTypeSegment foo)
  {
    sh(foo);
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String print(gen.ABS.Absyn.ListQualTypeSegment foo)
  {
    pp(foo, 0);
    trim();
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String show(gen.ABS.Absyn.ListQualTypeSegment foo)
  {
    sh(foo);
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String print(gen.ABS.Absyn.ListDecl foo)
  {
    pp(foo, 0);
    trim();
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String show(gen.ABS.Absyn.ListDecl foo)
  {
    sh(foo);
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String print(gen.ABS.Absyn.Decl foo)
  {
    pp(foo, 0);
    trim();
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String show(gen.ABS.Absyn.Decl foo)
  {
    sh(foo);
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String print(gen.ABS.Absyn.ConstrIdent foo)
  {
    pp(foo, 0);
    trim();
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String show(gen.ABS.Absyn.ConstrIdent foo)
  {
    sh(foo);
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String print(gen.ABS.Absyn.ConstrType foo)
  {
    pp(foo, 0);
    trim();
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String show(gen.ABS.Absyn.ConstrType foo)
  {
    sh(foo);
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String print(gen.ABS.Absyn.ListConstrType foo)
  {
    pp(foo, 0);
    trim();
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String show(gen.ABS.Absyn.ListConstrType foo)
  {
    sh(foo);
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String print(gen.ABS.Absyn.ListTypeIdent foo)
  {
    pp(foo, 0);
    trim();
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String show(gen.ABS.Absyn.ListTypeIdent foo)
  {
    sh(foo);
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String print(gen.ABS.Absyn.ListConstrIdent foo)
  {
    pp(foo, 0);
    trim();
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String show(gen.ABS.Absyn.ListConstrIdent foo)
  {
    sh(foo);
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String print(gen.ABS.Absyn.FunBody foo)
  {
    pp(foo, 0);
    trim();
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String show(gen.ABS.Absyn.FunBody foo)
  {
    sh(foo);
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String print(gen.ABS.Absyn.MethSignat foo)
  {
    pp(foo, 0);
    trim();
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String show(gen.ABS.Absyn.MethSignat foo)
  {
    sh(foo);
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String print(gen.ABS.Absyn.ListMethSignat foo)
  {
    pp(foo, 0);
    trim();
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String show(gen.ABS.Absyn.ListMethSignat foo)
  {
    sh(foo);
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String print(gen.ABS.Absyn.ClassBody foo)
  {
    pp(foo, 0);
    trim();
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String show(gen.ABS.Absyn.ClassBody foo)
  {
    sh(foo);
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String print(gen.ABS.Absyn.ListClassBody foo)
  {
    pp(foo, 0);
    trim();
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String show(gen.ABS.Absyn.ListClassBody foo)
  {
    sh(foo);
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String print(gen.ABS.Absyn.Block foo)
  {
    pp(foo, 0);
    trim();
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String show(gen.ABS.Absyn.Block foo)
  {
    sh(foo);
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String print(gen.ABS.Absyn.MaybeBlock foo)
  {
    pp(foo, 0);
    trim();
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String show(gen.ABS.Absyn.MaybeBlock foo)
  {
    sh(foo);
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String print(gen.ABS.Absyn.ListParam foo)
  {
    pp(foo, 0);
    trim();
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String show(gen.ABS.Absyn.ListParam foo)
  {
    sh(foo);
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String print(gen.ABS.Absyn.Param foo)
  {
    pp(foo, 0);
    trim();
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String show(gen.ABS.Absyn.Param foo)
  {
    sh(foo);
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String print(gen.ABS.Absyn.ListStm foo)
  {
    pp(foo, 0);
    trim();
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String show(gen.ABS.Absyn.ListStm foo)
  {
    sh(foo);
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String print(gen.ABS.Absyn.Stm foo)
  {
    pp(foo, 0);
    trim();
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String show(gen.ABS.Absyn.Stm foo)
  {
    sh(foo);
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String print(gen.ABS.Absyn.CatchBranch foo)
  {
    pp(foo, 0);
    trim();
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String show(gen.ABS.Absyn.CatchBranch foo)
  {
    sh(foo);
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String print(gen.ABS.Absyn.ListCatchBranch foo)
  {
    pp(foo, 0);
    trim();
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String show(gen.ABS.Absyn.ListCatchBranch foo)
  {
    sh(foo);
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String print(gen.ABS.Absyn.MaybeFinally foo)
  {
    pp(foo, 0);
    trim();
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String show(gen.ABS.Absyn.MaybeFinally foo)
  {
    sh(foo);
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String print(gen.ABS.Absyn.Guard foo)
  {
    pp(foo, 0);
    trim();
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String show(gen.ABS.Absyn.Guard foo)
  {
    sh(foo);
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String print(gen.ABS.Absyn.Exp foo)
  {
    pp(foo, 0);
    trim();
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String show(gen.ABS.Absyn.Exp foo)
  {
    sh(foo);
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String print(gen.ABS.Absyn.ListPureExp foo)
  {
    pp(foo, 0);
    trim();
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String show(gen.ABS.Absyn.ListPureExp foo)
  {
    sh(foo);
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String print(gen.ABS.Absyn.PureExp foo)
  {
    pp(foo, 0);
    trim();
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String show(gen.ABS.Absyn.PureExp foo)
  {
    sh(foo);
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String print(gen.ABS.Absyn.CaseBranch foo)
  {
    pp(foo, 0);
    trim();
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String show(gen.ABS.Absyn.CaseBranch foo)
  {
    sh(foo);
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String print(gen.ABS.Absyn.ListCaseBranch foo)
  {
    pp(foo, 0);
    trim();
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String show(gen.ABS.Absyn.ListCaseBranch foo)
  {
    sh(foo);
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String print(gen.ABS.Absyn.ListPattern foo)
  {
    pp(foo, 0);
    trim();
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String show(gen.ABS.Absyn.ListPattern foo)
  {
    sh(foo);
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String print(gen.ABS.Absyn.Pattern foo)
  {
    pp(foo, 0);
    trim();
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String show(gen.ABS.Absyn.Pattern foo)
  {
    sh(foo);
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String print(gen.ABS.Absyn.Literal foo)
  {
    pp(foo, 0);
    trim();
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String show(gen.ABS.Absyn.Literal foo)
  {
    sh(foo);
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String print(gen.ABS.Absyn.EffExp foo)
  {
    pp(foo, 0);
    trim();
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String show(gen.ABS.Absyn.EffExp foo)
  {
    sh(foo);
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  /***   You shouldn't need to change anything beyond this point.   ***/

  private static void pp(gen.ABS.Absyn.AnyIdent foo, int _i_)
  {
    if (foo instanceof gen.ABS.Absyn.AnyIden)
    {
       gen.ABS.Absyn.AnyIden _anyiden = (gen.ABS.Absyn.AnyIden) foo;
       if (_i_ > 0) render(_L_PAREN);
       pp(_anyiden.ident_, 0);
       if (_i_ > 0) render(_R_PAREN);
    }
    else     if (foo instanceof gen.ABS.Absyn.AnyTyIden)
    {
       gen.ABS.Absyn.AnyTyIden _anytyiden = (gen.ABS.Absyn.AnyTyIden) foo;
       if (_i_ > 0) render(_L_PAREN);
       pp(_anytyiden.typeident_, 0);
       if (_i_ > 0) render(_R_PAREN);
    }
  }

  private static void pp(gen.ABS.Absyn.ListAnyIdent foo, int _i_)
  {
     for (java.util.Iterator<AnyIdent> it = foo.iterator(); it.hasNext();)
     {
       pp(it.next(), 0);
       if (it.hasNext()) {
         render(",");
       } else {
         render("");
       }
     }
  }

  private static void pp(gen.ABS.Absyn.Program foo, int _i_)
  {
    if (foo instanceof gen.ABS.Absyn.Prog)
    {
       gen.ABS.Absyn.Prog _prog = (gen.ABS.Absyn.Prog) foo;
       if (_i_ > 0) render(_L_PAREN);
       pp(_prog.listmodule_, 0);
       if (_i_ > 0) render(_R_PAREN);
    }
  }

  private static void pp(gen.ABS.Absyn.ListModule foo, int _i_)
  {
     for (java.util.Iterator<Module> it = foo.iterator(); it.hasNext();)
     {
       pp(it.next(), 0);
       if (it.hasNext()) {
         render("");
       } else {
         render("");
       }
     }
  }

  private static void pp(gen.ABS.Absyn.Module foo, int _i_)
  {
    if (foo instanceof gen.ABS.Absyn.Modul)
    {
       gen.ABS.Absyn.Modul _modul = (gen.ABS.Absyn.Modul) foo;
       if (_i_ > 0) render(_L_PAREN);
       render("module");
       pp(_modul.qualtype_, 0);
       render(";");
       pp(_modul.listexport_, 0);
       pp(_modul.listimport_, 0);
       pp(_modul.listdecl_, 0);
       pp(_modul.maybeblock_, 0);
       if (_i_ > 0) render(_R_PAREN);
    }
  }

  private static void pp(gen.ABS.Absyn.Export foo, int _i_)
  {
    if (foo instanceof gen.ABS.Absyn.AnyExport)
    {
       gen.ABS.Absyn.AnyExport _anyexport = (gen.ABS.Absyn.AnyExport) foo;
       if (_i_ > 0) render(_L_PAREN);
       render("export");
       pp(_anyexport.listanyident_, 0);
       if (_i_ > 0) render(_R_PAREN);
    }
    else     if (foo instanceof gen.ABS.Absyn.AnyFromExport)
    {
       gen.ABS.Absyn.AnyFromExport _anyfromexport = (gen.ABS.Absyn.AnyFromExport) foo;
       if (_i_ > 0) render(_L_PAREN);
       render("export");
       pp(_anyfromexport.listanyident_, 0);
       render("from");
       pp(_anyfromexport.qualtype_, 0);
       if (_i_ > 0) render(_R_PAREN);
    }
    else     if (foo instanceof gen.ABS.Absyn.StarExport)
    {
       gen.ABS.Absyn.StarExport _starexport = (gen.ABS.Absyn.StarExport) foo;
       if (_i_ > 0) render(_L_PAREN);
       render("export");
       render("*");
       if (_i_ > 0) render(_R_PAREN);
    }
    else     if (foo instanceof gen.ABS.Absyn.StarFromExport)
    {
       gen.ABS.Absyn.StarFromExport _starfromexport = (gen.ABS.Absyn.StarFromExport) foo;
       if (_i_ > 0) render(_L_PAREN);
       render("export");
       render("*");
       render("from");
       pp(_starfromexport.qualtype_, 0);
       if (_i_ > 0) render(_R_PAREN);
    }
  }

  private static void pp(gen.ABS.Absyn.ListExport foo, int _i_)
  {
     for (java.util.Iterator<Export> it = foo.iterator(); it.hasNext();)
     {
       pp(it.next(), 0);
       if (it.hasNext()) {
         render(";");
       } else {
         render(";");
       }
     }
  }

  private static void pp(gen.ABS.Absyn.Import foo, int _i_)
  {
    if (foo instanceof gen.ABS.Absyn.AnyImport)
    {
       gen.ABS.Absyn.AnyImport _anyimport = (gen.ABS.Absyn.AnyImport) foo;
       if (_i_ > 0) render(_L_PAREN);
       pp(_anyimport.importtype_, 0);
       pp(_anyimport.qualtype_, 0);
       render(".");
       pp(_anyimport.anyident_, 0);
       if (_i_ > 0) render(_R_PAREN);
    }
    else     if (foo instanceof gen.ABS.Absyn.AnyFromImport)
    {
       gen.ABS.Absyn.AnyFromImport _anyfromimport = (gen.ABS.Absyn.AnyFromImport) foo;
       if (_i_ > 0) render(_L_PAREN);
       pp(_anyfromimport.importtype_, 0);
       pp(_anyfromimport.listanyident_, 0);
       render("from");
       pp(_anyfromimport.qualtype_, 0);
       if (_i_ > 0) render(_R_PAREN);
    }
    else     if (foo instanceof gen.ABS.Absyn.StarFromImport)
    {
       gen.ABS.Absyn.StarFromImport _starfromimport = (gen.ABS.Absyn.StarFromImport) foo;
       if (_i_ > 0) render(_L_PAREN);
       pp(_starfromimport.importtype_, 0);
       render("*");
       render("from");
       pp(_starfromimport.qualtype_, 0);
       if (_i_ > 0) render(_R_PAREN);
    }
  }

  private static void pp(gen.ABS.Absyn.ListImport foo, int _i_)
  {
     for (java.util.Iterator<Import> it = foo.iterator(); it.hasNext();)
     {
       pp(it.next(), 0);
       if (it.hasNext()) {
         render(";");
       } else {
         render(";");
       }
     }
  }

  private static void pp(gen.ABS.Absyn.ImportType foo, int _i_)
  {
    if (foo instanceof gen.ABS.Absyn.ForeignImport)
    {
       gen.ABS.Absyn.ForeignImport _foreignimport = (gen.ABS.Absyn.ForeignImport) foo;
       if (_i_ > 0) render(_L_PAREN);
       render("fimport");
       if (_i_ > 0) render(_R_PAREN);
    }
    else     if (foo instanceof gen.ABS.Absyn.NormalImport)
    {
       gen.ABS.Absyn.NormalImport _normalimport = (gen.ABS.Absyn.NormalImport) foo;
       if (_i_ > 0) render(_L_PAREN);
       render("import");
       if (_i_ > 0) render(_R_PAREN);
    }
  }

  private static void pp(gen.ABS.Absyn.Type foo, int _i_)
  {
    if (foo instanceof gen.ABS.Absyn.TUnderscore)
    {
       gen.ABS.Absyn.TUnderscore _tunderscore = (gen.ABS.Absyn.TUnderscore) foo;
       if (_i_ > 0) render(_L_PAREN);
       render("_");
       if (_i_ > 0) render(_R_PAREN);
    }
    else     if (foo instanceof gen.ABS.Absyn.TSimple)
    {
       gen.ABS.Absyn.TSimple _tsimple = (gen.ABS.Absyn.TSimple) foo;
       if (_i_ > 0) render(_L_PAREN);
       pp(_tsimple.qualtype_, 0);
       if (_i_ > 0) render(_R_PAREN);
    }
    else     if (foo instanceof gen.ABS.Absyn.TGen)
    {
       gen.ABS.Absyn.TGen _tgen = (gen.ABS.Absyn.TGen) foo;
       if (_i_ > 0) render(_L_PAREN);
       pp(_tgen.qualtype_, 0);
       render("<");
       pp(_tgen.listtype_, 0);
       render(">");
       if (_i_ > 0) render(_R_PAREN);
    }
  }

  private static void pp(gen.ABS.Absyn.ListType foo, int _i_)
  {
     for (java.util.Iterator<Type> it = foo.iterator(); it.hasNext();)
     {
       pp(it.next(), 0);
       if (it.hasNext()) {
         render(",");
       } else {
         render("");
       }
     }
  }

  private static void pp(gen.ABS.Absyn.ListQualType foo, int _i_)
  {
     for (java.util.Iterator<QualType> it = foo.iterator(); it.hasNext();)
     {
       pp(it.next(), 0);
       if (it.hasNext()) {
         render(",");
       } else {
         render("");
       }
     }
  }

  private static void pp(gen.ABS.Absyn.QualType foo, int _i_)
  {
    if (foo instanceof gen.ABS.Absyn.QType)
    {
       gen.ABS.Absyn.QType _qtype = (gen.ABS.Absyn.QType) foo;
       if (_i_ > 0) render(_L_PAREN);
       pp(_qtype.listqualtypesegment_, 0);
       if (_i_ > 0) render(_R_PAREN);
    }
  }

  private static void pp(gen.ABS.Absyn.QualTypeSegment foo, int _i_)
  {
    if (foo instanceof gen.ABS.Absyn.QTypeSegment)
    {
       gen.ABS.Absyn.QTypeSegment _qtypesegment = (gen.ABS.Absyn.QTypeSegment) foo;
       if (_i_ > 0) render(_L_PAREN);
       pp(_qtypesegment.typeident_, 0);
       if (_i_ > 0) render(_R_PAREN);
    }
  }

  private static void pp(gen.ABS.Absyn.ListQualTypeSegment foo, int _i_)
  {
     for (java.util.Iterator<QualTypeSegment> it = foo.iterator(); it.hasNext();)
     {
       pp(it.next(), 0);
       if (it.hasNext()) {
         render(".");
       } else {
         render("");
       }
     }
  }

  private static void pp(gen.ABS.Absyn.ListDecl foo, int _i_)
  {
     for (java.util.Iterator<Decl> it = foo.iterator(); it.hasNext();)
     {
       pp(it.next(), 0);
       if (it.hasNext()) {
         render("");
       } else {
         render("");
       }
     }
  }

  private static void pp(gen.ABS.Absyn.Decl foo, int _i_)
  {
    if (foo instanceof gen.ABS.Absyn.TypeDecl)
    {
       gen.ABS.Absyn.TypeDecl _typedecl = (gen.ABS.Absyn.TypeDecl) foo;
       if (_i_ > 0) render(_L_PAREN);
       render("type");
       pp(_typedecl.typeident_, 0);
       render("=");
       pp(_typedecl.type_, 0);
       render(";");
       if (_i_ > 0) render(_R_PAREN);
    }
    else     if (foo instanceof gen.ABS.Absyn.ExceptionDecl)
    {
       gen.ABS.Absyn.ExceptionDecl _exceptiondecl = (gen.ABS.Absyn.ExceptionDecl) foo;
       if (_i_ > 0) render(_L_PAREN);
       render("exception");
       pp(_exceptiondecl.constrident_, 0);
       render(";");
       if (_i_ > 0) render(_R_PAREN);
    }
    else     if (foo instanceof gen.ABS.Absyn.DataDecl)
    {
       gen.ABS.Absyn.DataDecl _datadecl = (gen.ABS.Absyn.DataDecl) foo;
       if (_i_ > 0) render(_L_PAREN);
       render("data");
       pp(_datadecl.typeident_, 0);
       render("=");
       pp(_datadecl.listconstrident_, 0);
       render(";");
       if (_i_ > 0) render(_R_PAREN);
    }
    else     if (foo instanceof gen.ABS.Absyn.DataParDecl)
    {
       gen.ABS.Absyn.DataParDecl _datapardecl = (gen.ABS.Absyn.DataParDecl) foo;
       if (_i_ > 0) render(_L_PAREN);
       render("data");
       pp(_datapardecl.typeident_, 0);
       render("<");
       pp(_datapardecl.listtypeident_, 0);
       render(">");
       render("=");
       pp(_datapardecl.listconstrident_, 0);
       render(";");
       if (_i_ > 0) render(_R_PAREN);
    }
    else     if (foo instanceof gen.ABS.Absyn.FunDecl)
    {
       gen.ABS.Absyn.FunDecl _fundecl = (gen.ABS.Absyn.FunDecl) foo;
       if (_i_ > 0) render(_L_PAREN);
       render("def");
       pp(_fundecl.type_, 0);
       pp(_fundecl.ident_, 0);
       render("(");
       pp(_fundecl.listparam_, 0);
       render(")");
       render("=");
       pp(_fundecl.funbody_, 0);
       render(";");
       if (_i_ > 0) render(_R_PAREN);
    }
    else     if (foo instanceof gen.ABS.Absyn.FunParDecl)
    {
       gen.ABS.Absyn.FunParDecl _funpardecl = (gen.ABS.Absyn.FunParDecl) foo;
       if (_i_ > 0) render(_L_PAREN);
       render("def");
       pp(_funpardecl.type_, 0);
       pp(_funpardecl.ident_, 0);
       render("<");
       pp(_funpardecl.listtypeident_, 0);
       render(">");
       render("(");
       pp(_funpardecl.listparam_, 0);
       render(")");
       render("=");
       pp(_funpardecl.funbody_, 0);
       render(";");
       if (_i_ > 0) render(_R_PAREN);
    }
    else     if (foo instanceof gen.ABS.Absyn.InterfDecl)
    {
       gen.ABS.Absyn.InterfDecl _interfdecl = (gen.ABS.Absyn.InterfDecl) foo;
       if (_i_ > 0) render(_L_PAREN);
       render("interface");
       pp(_interfdecl.typeident_, 0);
       render("{");
       pp(_interfdecl.listmethsignat_, 0);
       render("}");
       if (_i_ > 0) render(_R_PAREN);
    }
    else     if (foo instanceof gen.ABS.Absyn.ExtendsDecl)
    {
       gen.ABS.Absyn.ExtendsDecl _extendsdecl = (gen.ABS.Absyn.ExtendsDecl) foo;
       if (_i_ > 0) render(_L_PAREN);
       render("interface");
       pp(_extendsdecl.typeident_, 0);
       render("extends");
       pp(_extendsdecl.listqualtype_, 0);
       render("{");
       pp(_extendsdecl.listmethsignat_, 0);
       render("}");
       if (_i_ > 0) render(_R_PAREN);
    }
    else     if (foo instanceof gen.ABS.Absyn.ClassDecl)
    {
       gen.ABS.Absyn.ClassDecl _classdecl = (gen.ABS.Absyn.ClassDecl) foo;
       if (_i_ > 0) render(_L_PAREN);
       render("class");
       pp(_classdecl.typeident_, 0);
       render("{");
       pp(_classdecl.listclassbody_1, 0);
       pp(_classdecl.maybeblock_, 0);
       pp(_classdecl.listclassbody_2, 0);
       render("}");
       if (_i_ > 0) render(_R_PAREN);
    }
    else     if (foo instanceof gen.ABS.Absyn.ClassParamDecl)
    {
       gen.ABS.Absyn.ClassParamDecl _classparamdecl = (gen.ABS.Absyn.ClassParamDecl) foo;
       if (_i_ > 0) render(_L_PAREN);
       render("class");
       pp(_classparamdecl.typeident_, 0);
       render("(");
       pp(_classparamdecl.listparam_, 0);
       render(")");
       render("{");
       pp(_classparamdecl.listclassbody_1, 0);
       pp(_classparamdecl.maybeblock_, 0);
       pp(_classparamdecl.listclassbody_2, 0);
       render("}");
       if (_i_ > 0) render(_R_PAREN);
    }
    else     if (foo instanceof gen.ABS.Absyn.ClassImplements)
    {
       gen.ABS.Absyn.ClassImplements _classimplements = (gen.ABS.Absyn.ClassImplements) foo;
       if (_i_ > 0) render(_L_PAREN);
       render("class");
       pp(_classimplements.typeident_, 0);
       render("implements");
       pp(_classimplements.listqualtype_, 0);
       render("{");
       pp(_classimplements.listclassbody_1, 0);
       pp(_classimplements.maybeblock_, 0);
       pp(_classimplements.listclassbody_2, 0);
       render("}");
       if (_i_ > 0) render(_R_PAREN);
    }
    else     if (foo instanceof gen.ABS.Absyn.ClassParamImplements)
    {
       gen.ABS.Absyn.ClassParamImplements _classparamimplements = (gen.ABS.Absyn.ClassParamImplements) foo;
       if (_i_ > 0) render(_L_PAREN);
       render("class");
       pp(_classparamimplements.typeident_, 0);
       render("(");
       pp(_classparamimplements.listparam_, 0);
       render(")");
       render("implements");
       pp(_classparamimplements.listqualtype_, 0);
       render("{");
       pp(_classparamimplements.listclassbody_1, 0);
       pp(_classparamimplements.maybeblock_, 0);
       pp(_classparamimplements.listclassbody_2, 0);
       render("}");
       if (_i_ > 0) render(_R_PAREN);
    }
  }

  private static void pp(gen.ABS.Absyn.ConstrIdent foo, int _i_)
  {
    if (foo instanceof gen.ABS.Absyn.SinglConstrIdent)
    {
       gen.ABS.Absyn.SinglConstrIdent _singlconstrident = (gen.ABS.Absyn.SinglConstrIdent) foo;
       if (_i_ > 0) render(_L_PAREN);
       pp(_singlconstrident.typeident_, 0);
       if (_i_ > 0) render(_R_PAREN);
    }
    else     if (foo instanceof gen.ABS.Absyn.ParamConstrIdent)
    {
       gen.ABS.Absyn.ParamConstrIdent _paramconstrident = (gen.ABS.Absyn.ParamConstrIdent) foo;
       if (_i_ > 0) render(_L_PAREN);
       pp(_paramconstrident.typeident_, 0);
       render("(");
       pp(_paramconstrident.listconstrtype_, 0);
       render(")");
       if (_i_ > 0) render(_R_PAREN);
    }
  }

  private static void pp(gen.ABS.Absyn.ConstrType foo, int _i_)
  {
    if (foo instanceof gen.ABS.Absyn.EmptyConstrType)
    {
       gen.ABS.Absyn.EmptyConstrType _emptyconstrtype = (gen.ABS.Absyn.EmptyConstrType) foo;
       if (_i_ > 0) render(_L_PAREN);
       pp(_emptyconstrtype.type_, 0);
       if (_i_ > 0) render(_R_PAREN);
    }
    else     if (foo instanceof gen.ABS.Absyn.RecordConstrType)
    {
       gen.ABS.Absyn.RecordConstrType _recordconstrtype = (gen.ABS.Absyn.RecordConstrType) foo;
       if (_i_ > 0) render(_L_PAREN);
       pp(_recordconstrtype.type_, 0);
       pp(_recordconstrtype.ident_, 0);
       if (_i_ > 0) render(_R_PAREN);
    }
  }

  private static void pp(gen.ABS.Absyn.ListConstrType foo, int _i_)
  {
     for (java.util.Iterator<ConstrType> it = foo.iterator(); it.hasNext();)
     {
       pp(it.next(), 0);
       if (it.hasNext()) {
         render(",");
       } else {
         render("");
       }
     }
  }

  private static void pp(gen.ABS.Absyn.ListTypeIdent foo, int _i_)
  {
     for (java.util.Iterator<String> it = foo.iterator(); it.hasNext();)
     {
       pp(it.next(), 0);
       if (it.hasNext()) {
         render(",");
       } else {
         render("");
       }
     }
  }

  private static void pp(gen.ABS.Absyn.ListConstrIdent foo, int _i_)
  {
     for (java.util.Iterator<ConstrIdent> it = foo.iterator(); it.hasNext();)
     {
       pp(it.next(), 0);
       if (it.hasNext()) {
         render("|");
       } else {
         render("");
       }
     }
  }

  private static void pp(gen.ABS.Absyn.FunBody foo, int _i_)
  {
    if (foo instanceof gen.ABS.Absyn.BuiltinFunBody)
    {
       gen.ABS.Absyn.BuiltinFunBody _builtinfunbody = (gen.ABS.Absyn.BuiltinFunBody) foo;
       if (_i_ > 0) render(_L_PAREN);
       render("builtin");
       if (_i_ > 0) render(_R_PAREN);
    }
    else     if (foo instanceof gen.ABS.Absyn.NormalFunBody)
    {
       gen.ABS.Absyn.NormalFunBody _normalfunbody = (gen.ABS.Absyn.NormalFunBody) foo;
       if (_i_ > 0) render(_L_PAREN);
       pp(_normalfunbody.pureexp_, 0);
       if (_i_ > 0) render(_R_PAREN);
    }
  }

  private static void pp(gen.ABS.Absyn.MethSignat foo, int _i_)
  {
    if (foo instanceof gen.ABS.Absyn.MethSig)
    {
       gen.ABS.Absyn.MethSig _methsig = (gen.ABS.Absyn.MethSig) foo;
       if (_i_ > 0) render(_L_PAREN);
       pp(_methsig.type_, 0);
       pp(_methsig.ident_, 0);
       render("(");
       pp(_methsig.listparam_, 0);
       render(")");
       if (_i_ > 0) render(_R_PAREN);
    }
  }

  private static void pp(gen.ABS.Absyn.ListMethSignat foo, int _i_)
  {
     for (java.util.Iterator<MethSignat> it = foo.iterator(); it.hasNext();)
     {
       pp(it.next(), 0);
       if (it.hasNext()) {
         render(";");
       } else {
         render(";");
       }
     }
  }

  private static void pp(gen.ABS.Absyn.ClassBody foo, int _i_)
  {
    if (foo instanceof gen.ABS.Absyn.FieldClassBody)
    {
       gen.ABS.Absyn.FieldClassBody _fieldclassbody = (gen.ABS.Absyn.FieldClassBody) foo;
       if (_i_ > 0) render(_L_PAREN);
       pp(_fieldclassbody.type_, 0);
       pp(_fieldclassbody.ident_, 0);
       render(";");
       if (_i_ > 0) render(_R_PAREN);
    }
    else     if (foo instanceof gen.ABS.Absyn.FieldAssignClassBody)
    {
       gen.ABS.Absyn.FieldAssignClassBody _fieldassignclassbody = (gen.ABS.Absyn.FieldAssignClassBody) foo;
       if (_i_ > 0) render(_L_PAREN);
       pp(_fieldassignclassbody.type_, 0);
       pp(_fieldassignclassbody.ident_, 0);
       render("=");
       pp(_fieldassignclassbody.pureexp_, 0);
       render(";");
       if (_i_ > 0) render(_R_PAREN);
    }
    else     if (foo instanceof gen.ABS.Absyn.MethClassBody)
    {
       gen.ABS.Absyn.MethClassBody _methclassbody = (gen.ABS.Absyn.MethClassBody) foo;
       if (_i_ > 0) render(_L_PAREN);
       pp(_methclassbody.type_, 0);
       pp(_methclassbody.ident_, 0);
       render("(");
       pp(_methclassbody.listparam_, 0);
       render(")");
       pp(_methclassbody.block_, 0);
       if (_i_ > 0) render(_R_PAREN);
    }
  }

  private static void pp(gen.ABS.Absyn.ListClassBody foo, int _i_)
  {
     for (java.util.Iterator<ClassBody> it = foo.iterator(); it.hasNext();)
     {
       pp(it.next(), 0);
       if (it.hasNext()) {
         render("");
       } else {
         render("");
       }
     }
  }

  private static void pp(gen.ABS.Absyn.Block foo, int _i_)
  {
    if (foo instanceof gen.ABS.Absyn.Bloc)
    {
       gen.ABS.Absyn.Bloc _bloc = (gen.ABS.Absyn.Bloc) foo;
       if (_i_ > 0) render(_L_PAREN);
       render("{");
       pp(_bloc.liststm_, 0);
       render("}");
       if (_i_ > 0) render(_R_PAREN);
    }
  }

  private static void pp(gen.ABS.Absyn.MaybeBlock foo, int _i_)
  {
    if (foo instanceof gen.ABS.Absyn.JustBlock)
    {
       gen.ABS.Absyn.JustBlock _justblock = (gen.ABS.Absyn.JustBlock) foo;
       if (_i_ > 0) render(_L_PAREN);
       pp(_justblock.block_, 0);
       if (_i_ > 0) render(_R_PAREN);
    }
    else     if (foo instanceof gen.ABS.Absyn.NoBlock)
    {
       gen.ABS.Absyn.NoBlock _noblock = (gen.ABS.Absyn.NoBlock) foo;
       if (_i_ > 0) render(_L_PAREN);
       if (_i_ > 0) render(_R_PAREN);
    }
  }

  private static void pp(gen.ABS.Absyn.ListParam foo, int _i_)
  {
     for (java.util.Iterator<Param> it = foo.iterator(); it.hasNext();)
     {
       pp(it.next(), 0);
       if (it.hasNext()) {
         render(",");
       } else {
         render("");
       }
     }
  }

  private static void pp(gen.ABS.Absyn.Param foo, int _i_)
  {
    if (foo instanceof gen.ABS.Absyn.Par)
    {
       gen.ABS.Absyn.Par _par = (gen.ABS.Absyn.Par) foo;
       if (_i_ > 0) render(_L_PAREN);
       pp(_par.type_, 0);
       pp(_par.ident_, 0);
       if (_i_ > 0) render(_R_PAREN);
    }
  }

  private static void pp(gen.ABS.Absyn.ListStm foo, int _i_)
  {
     for (java.util.Iterator<Stm> it = foo.iterator(); it.hasNext();)
     {
       pp(it.next(), 0);
       if (it.hasNext()) {
         render("");
       } else {
         render("");
       }
     }
  }

  private static void pp(gen.ABS.Absyn.Stm foo, int _i_)
  {
    if (foo instanceof gen.ABS.Absyn.SExp)
    {
       gen.ABS.Absyn.SExp _sexp = (gen.ABS.Absyn.SExp) foo;
       if (_i_ > 0) render(_L_PAREN);
       pp(_sexp.exp_, 0);
       render(";");
       if (_i_ > 0) render(_R_PAREN);
    }
    else     if (foo instanceof gen.ABS.Absyn.SBlock)
    {
       gen.ABS.Absyn.SBlock _sblock = (gen.ABS.Absyn.SBlock) foo;
       if (_i_ > 0) render(_L_PAREN);
       render("{");
       pp(_sblock.liststm_, 0);
       render("}");
       if (_i_ > 0) render(_R_PAREN);
    }
    else     if (foo instanceof gen.ABS.Absyn.SWhile)
    {
       gen.ABS.Absyn.SWhile _swhile = (gen.ABS.Absyn.SWhile) foo;
       if (_i_ > 0) render(_L_PAREN);
       render("while");
       render("(");
       pp(_swhile.pureexp_, 0);
       render(")");
       pp(_swhile.stm_, 0);
       if (_i_ > 0) render(_R_PAREN);
    }
    else     if (foo instanceof gen.ABS.Absyn.SReturn)
    {
       gen.ABS.Absyn.SReturn _sreturn = (gen.ABS.Absyn.SReturn) foo;
       if (_i_ > 0) render(_L_PAREN);
       render("return");
       pp(_sreturn.exp_, 0);
       render(";");
       if (_i_ > 0) render(_R_PAREN);
    }
    else     if (foo instanceof gen.ABS.Absyn.SAss)
    {
       gen.ABS.Absyn.SAss _sass = (gen.ABS.Absyn.SAss) foo;
       if (_i_ > 0) render(_L_PAREN);
       pp(_sass.ident_, 0);
       render("=");
       pp(_sass.exp_, 0);
       render(";");
       if (_i_ > 0) render(_R_PAREN);
    }
    else     if (foo instanceof gen.ABS.Absyn.SFieldAss)
    {
       gen.ABS.Absyn.SFieldAss _sfieldass = (gen.ABS.Absyn.SFieldAss) foo;
       if (_i_ > 0) render(_L_PAREN);
       render("this");
       render(".");
       pp(_sfieldass.ident_, 0);
       render("=");
       pp(_sfieldass.exp_, 0);
       render(";");
       if (_i_ > 0) render(_R_PAREN);
    }
    else     if (foo instanceof gen.ABS.Absyn.SDec)
    {
       gen.ABS.Absyn.SDec _sdec = (gen.ABS.Absyn.SDec) foo;
       if (_i_ > 0) render(_L_PAREN);
       pp(_sdec.type_, 0);
       pp(_sdec.ident_, 0);
       render(";");
       if (_i_ > 0) render(_R_PAREN);
    }
    else     if (foo instanceof gen.ABS.Absyn.SDecAss)
    {
       gen.ABS.Absyn.SDecAss _sdecass = (gen.ABS.Absyn.SDecAss) foo;
       if (_i_ > 0) render(_L_PAREN);
       pp(_sdecass.type_, 0);
       pp(_sdecass.ident_, 0);
       render("=");
       pp(_sdecass.exp_, 0);
       render(";");
       if (_i_ > 0) render(_R_PAREN);
    }
    else     if (foo instanceof gen.ABS.Absyn.SIf)
    {
       gen.ABS.Absyn.SIf _sif = (gen.ABS.Absyn.SIf) foo;
       if (_i_ > 0) render(_L_PAREN);
       render("if");
       render("(");
       pp(_sif.pureexp_, 0);
       render(")");
       pp(_sif.stm_, 0);
       if (_i_ > 0) render(_R_PAREN);
    }
    else     if (foo instanceof gen.ABS.Absyn.SIfElse)
    {
       gen.ABS.Absyn.SIfElse _sifelse = (gen.ABS.Absyn.SIfElse) foo;
       if (_i_ > 0) render(_L_PAREN);
       render("if");
       render("(");
       pp(_sifelse.pureexp_, 0);
       render(")");
       pp(_sifelse.stm_1, 0);
       render("else");
       pp(_sifelse.stm_2, 0);
       if (_i_ > 0) render(_R_PAREN);
    }
    else     if (foo instanceof gen.ABS.Absyn.SSuspend)
    {
       gen.ABS.Absyn.SSuspend _ssuspend = (gen.ABS.Absyn.SSuspend) foo;
       if (_i_ > 0) render(_L_PAREN);
       render("suspend");
       render(";");
       if (_i_ > 0) render(_R_PAREN);
    }
    else     if (foo instanceof gen.ABS.Absyn.SSkip)
    {
       gen.ABS.Absyn.SSkip _sskip = (gen.ABS.Absyn.SSkip) foo;
       if (_i_ > 0) render(_L_PAREN);
       render("skip");
       render(";");
       if (_i_ > 0) render(_R_PAREN);
    }
    else     if (foo instanceof gen.ABS.Absyn.SAssert)
    {
       gen.ABS.Absyn.SAssert _sassert = (gen.ABS.Absyn.SAssert) foo;
       if (_i_ > 0) render(_L_PAREN);
       render("assert");
       pp(_sassert.pureexp_, 0);
       render(";");
       if (_i_ > 0) render(_R_PAREN);
    }
    else     if (foo instanceof gen.ABS.Absyn.SAwait)
    {
       gen.ABS.Absyn.SAwait _sawait = (gen.ABS.Absyn.SAwait) foo;
       if (_i_ > 0) render(_L_PAREN);
       render("await");
       pp(_sawait.guard_, 0);
       render(";");
       if (_i_ > 0) render(_R_PAREN);
    }
    else     if (foo instanceof gen.ABS.Absyn.SThrow)
    {
       gen.ABS.Absyn.SThrow _sthrow = (gen.ABS.Absyn.SThrow) foo;
       if (_i_ > 0) render(_L_PAREN);
       render("throw");
       pp(_sthrow.pureexp_, 0);
       render(";");
       if (_i_ > 0) render(_R_PAREN);
    }
    else     if (foo instanceof gen.ABS.Absyn.STryCatchFinally)
    {
       gen.ABS.Absyn.STryCatchFinally _strycatchfinally = (gen.ABS.Absyn.STryCatchFinally) foo;
       if (_i_ > 0) render(_L_PAREN);
       render("try");
       pp(_strycatchfinally.stm_, 0);
       render("catch");
       render("{");
       pp(_strycatchfinally.listcatchbranch_, 0);
       render("}");
       pp(_strycatchfinally.maybefinally_, 0);
       if (_i_ > 0) render(_R_PAREN);
    }
  }

  private static void pp(gen.ABS.Absyn.CatchBranch foo, int _i_)
  {
    if (foo instanceof gen.ABS.Absyn.CatchBranc)
    {
       gen.ABS.Absyn.CatchBranc _catchbranc = (gen.ABS.Absyn.CatchBranc) foo;
       if (_i_ > 0) render(_L_PAREN);
       pp(_catchbranc.pattern_, 0);
       render("=>");
       pp(_catchbranc.stm_, 0);
       if (_i_ > 0) render(_R_PAREN);
    }
  }

  private static void pp(gen.ABS.Absyn.ListCatchBranch foo, int _i_)
  {
     for (java.util.Iterator<CatchBranch> it = foo.iterator(); it.hasNext();)
     {
       pp(it.next(), 0);
       if (it.hasNext()) {
         render("");
       } else {
         render("");
       }
     }
  }

  private static void pp(gen.ABS.Absyn.MaybeFinally foo, int _i_)
  {
    if (foo instanceof gen.ABS.Absyn.JustFinally)
    {
       gen.ABS.Absyn.JustFinally _justfinally = (gen.ABS.Absyn.JustFinally) foo;
       if (_i_ > 0) render(_L_PAREN);
       render("finally");
       pp(_justfinally.stm_, 0);
       if (_i_ > 0) render(_R_PAREN);
    }
    else     if (foo instanceof gen.ABS.Absyn.NoFinally)
    {
       gen.ABS.Absyn.NoFinally _nofinally = (gen.ABS.Absyn.NoFinally) foo;
       if (_i_ > 0) render(_L_PAREN);
       if (_i_ > 0) render(_R_PAREN);
    }
  }

  private static void pp(gen.ABS.Absyn.Guard foo, int _i_)
  {
    if (foo instanceof gen.ABS.Absyn.VarGuard)
    {
       gen.ABS.Absyn.VarGuard _varguard = (gen.ABS.Absyn.VarGuard) foo;
       if (_i_ > 0) render(_L_PAREN);
       pp(_varguard.ident_, 0);
       render("?");
       if (_i_ > 0) render(_R_PAREN);
    }
    else     if (foo instanceof gen.ABS.Absyn.FieldGuard)
    {
       gen.ABS.Absyn.FieldGuard _fieldguard = (gen.ABS.Absyn.FieldGuard) foo;
       if (_i_ > 0) render(_L_PAREN);
       render("this");
       render(".");
       pp(_fieldguard.ident_, 0);
       render("?");
       if (_i_ > 0) render(_R_PAREN);
    }
    else     if (foo instanceof gen.ABS.Absyn.ExpGuard)
    {
       gen.ABS.Absyn.ExpGuard _expguard = (gen.ABS.Absyn.ExpGuard) foo;
       if (_i_ > 0) render(_L_PAREN);
       pp(_expguard.pureexp_, 0);
       if (_i_ > 0) render(_R_PAREN);
    }
    else     if (foo instanceof gen.ABS.Absyn.AndGuard)
    {
       gen.ABS.Absyn.AndGuard _andguard = (gen.ABS.Absyn.AndGuard) foo;
       if (_i_ > 0) render(_L_PAREN);
       pp(_andguard.guard_1, 0);
       render("&");
       pp(_andguard.guard_2, 0);
       if (_i_ > 0) render(_R_PAREN);
    }
  }

  private static void pp(gen.ABS.Absyn.Exp foo, int _i_)
  {
    if (foo instanceof gen.ABS.Absyn.ExpP)
    {
       gen.ABS.Absyn.ExpP _expp = (gen.ABS.Absyn.ExpP) foo;
       if (_i_ > 0) render(_L_PAREN);
       pp(_expp.pureexp_, 0);
       if (_i_ > 0) render(_R_PAREN);
    }
    else     if (foo instanceof gen.ABS.Absyn.ExpE)
    {
       gen.ABS.Absyn.ExpE _expe = (gen.ABS.Absyn.ExpE) foo;
       if (_i_ > 0) render(_L_PAREN);
       pp(_expe.effexp_, 0);
       if (_i_ > 0) render(_R_PAREN);
    }
  }

  private static void pp(gen.ABS.Absyn.ListPureExp foo, int _i_)
  {
     for (java.util.Iterator<PureExp> it = foo.iterator(); it.hasNext();)
     {
       pp(it.next(), 0);
       if (it.hasNext()) {
         render(",");
       } else {
         render("");
       }
     }
  }

  private static void pp(gen.ABS.Absyn.PureExp foo, int _i_)
  {
    if (foo instanceof gen.ABS.Absyn.EOr)
    {
       gen.ABS.Absyn.EOr _eor = (gen.ABS.Absyn.EOr) foo;
       if (_i_ > 0) render(_L_PAREN);
       pp(_eor.pureexp_1, 0);
       render("||");
       pp(_eor.pureexp_2, 1);
       if (_i_ > 0) render(_R_PAREN);
    }
    else     if (foo instanceof gen.ABS.Absyn.Let)
    {
       gen.ABS.Absyn.Let _let = (gen.ABS.Absyn.Let) foo;
       if (_i_ > 0) render(_L_PAREN);
       render("let");
       render("(");
       pp(_let.param_, 0);
       render(")");
       render("=");
       pp(_let.pureexp_1, 0);
       render("in");
       pp(_let.pureexp_2, 0);
       if (_i_ > 0) render(_R_PAREN);
    }
    else     if (foo instanceof gen.ABS.Absyn.If)
    {
       gen.ABS.Absyn.If _if = (gen.ABS.Absyn.If) foo;
       if (_i_ > 0) render(_L_PAREN);
       render("if");
       pp(_if.pureexp_1, 0);
       render("then");
       pp(_if.pureexp_2, 0);
       render("else");
       pp(_if.pureexp_3, 0);
       if (_i_ > 0) render(_R_PAREN);
    }
    else     if (foo instanceof gen.ABS.Absyn.Case)
    {
       gen.ABS.Absyn.Case _case = (gen.ABS.Absyn.Case) foo;
       if (_i_ > 0) render(_L_PAREN);
       render("case");
       pp(_case.pureexp_, 0);
       render("{");
       pp(_case.listcasebranch_, 0);
       render("}");
       if (_i_ > 0) render(_R_PAREN);
    }
    else     if (foo instanceof gen.ABS.Absyn.EAnd)
    {
       gen.ABS.Absyn.EAnd _eand = (gen.ABS.Absyn.EAnd) foo;
       if (_i_ > 1) render(_L_PAREN);
       pp(_eand.pureexp_1, 1);
       render("&&");
       pp(_eand.pureexp_2, 2);
       if (_i_ > 1) render(_R_PAREN);
    }
    else     if (foo instanceof gen.ABS.Absyn.EEq)
    {
       gen.ABS.Absyn.EEq _eeq = (gen.ABS.Absyn.EEq) foo;
       if (_i_ > 2) render(_L_PAREN);
       pp(_eeq.pureexp_1, 2);
       render("==");
       pp(_eeq.pureexp_2, 3);
       if (_i_ > 2) render(_R_PAREN);
    }
    else     if (foo instanceof gen.ABS.Absyn.ENeq)
    {
       gen.ABS.Absyn.ENeq _eneq = (gen.ABS.Absyn.ENeq) foo;
       if (_i_ > 2) render(_L_PAREN);
       pp(_eneq.pureexp_1, 2);
       render("!=");
       pp(_eneq.pureexp_2, 3);
       if (_i_ > 2) render(_R_PAREN);
    }
    else     if (foo instanceof gen.ABS.Absyn.ELt)
    {
       gen.ABS.Absyn.ELt _elt = (gen.ABS.Absyn.ELt) foo;
       if (_i_ > 3) render(_L_PAREN);
       pp(_elt.pureexp_1, 3);
       render("<");
       pp(_elt.pureexp_2, 4);
       if (_i_ > 3) render(_R_PAREN);
    }
    else     if (foo instanceof gen.ABS.Absyn.ELe)
    {
       gen.ABS.Absyn.ELe _ele = (gen.ABS.Absyn.ELe) foo;
       if (_i_ > 3) render(_L_PAREN);
       pp(_ele.pureexp_1, 3);
       render("<=");
       pp(_ele.pureexp_2, 4);
       if (_i_ > 3) render(_R_PAREN);
    }
    else     if (foo instanceof gen.ABS.Absyn.EGt)
    {
       gen.ABS.Absyn.EGt _egt = (gen.ABS.Absyn.EGt) foo;
       if (_i_ > 3) render(_L_PAREN);
       pp(_egt.pureexp_1, 3);
       render(">");
       pp(_egt.pureexp_2, 4);
       if (_i_ > 3) render(_R_PAREN);
    }
    else     if (foo instanceof gen.ABS.Absyn.EGe)
    {
       gen.ABS.Absyn.EGe _ege = (gen.ABS.Absyn.EGe) foo;
       if (_i_ > 3) render(_L_PAREN);
       pp(_ege.pureexp_1, 3);
       render(">=");
       pp(_ege.pureexp_2, 4);
       if (_i_ > 3) render(_R_PAREN);
    }
    else     if (foo instanceof gen.ABS.Absyn.EAdd)
    {
       gen.ABS.Absyn.EAdd _eadd = (gen.ABS.Absyn.EAdd) foo;
       if (_i_ > 4) render(_L_PAREN);
       pp(_eadd.pureexp_1, 4);
       render("+");
       pp(_eadd.pureexp_2, 5);
       if (_i_ > 4) render(_R_PAREN);
    }
    else     if (foo instanceof gen.ABS.Absyn.ESub)
    {
       gen.ABS.Absyn.ESub _esub = (gen.ABS.Absyn.ESub) foo;
       if (_i_ > 4) render(_L_PAREN);
       pp(_esub.pureexp_1, 4);
       render("-");
       pp(_esub.pureexp_2, 5);
       if (_i_ > 4) render(_R_PAREN);
    }
    else     if (foo instanceof gen.ABS.Absyn.EMul)
    {
       gen.ABS.Absyn.EMul _emul = (gen.ABS.Absyn.EMul) foo;
       if (_i_ > 5) render(_L_PAREN);
       pp(_emul.pureexp_1, 5);
       render("*");
       pp(_emul.pureexp_2, 6);
       if (_i_ > 5) render(_R_PAREN);
    }
    else     if (foo instanceof gen.ABS.Absyn.EDiv)
    {
       gen.ABS.Absyn.EDiv _ediv = (gen.ABS.Absyn.EDiv) foo;
       if (_i_ > 5) render(_L_PAREN);
       pp(_ediv.pureexp_1, 5);
       render("/");
       pp(_ediv.pureexp_2, 6);
       if (_i_ > 5) render(_R_PAREN);
    }
    else     if (foo instanceof gen.ABS.Absyn.EMod)
    {
       gen.ABS.Absyn.EMod _emod = (gen.ABS.Absyn.EMod) foo;
       if (_i_ > 5) render(_L_PAREN);
       pp(_emod.pureexp_1, 5);
       render("%");
       pp(_emod.pureexp_2, 6);
       if (_i_ > 5) render(_R_PAREN);
    }
    else     if (foo instanceof gen.ABS.Absyn.ELogNeg)
    {
       gen.ABS.Absyn.ELogNeg _elogneg = (gen.ABS.Absyn.ELogNeg) foo;
       if (_i_ > 6) render(_L_PAREN);
       render("~");
       pp(_elogneg.pureexp_, 6);
       if (_i_ > 6) render(_R_PAREN);
    }
    else     if (foo instanceof gen.ABS.Absyn.EIntNeg)
    {
       gen.ABS.Absyn.EIntNeg _eintneg = (gen.ABS.Absyn.EIntNeg) foo;
       if (_i_ > 6) render(_L_PAREN);
       render("-");
       pp(_eintneg.pureexp_, 6);
       if (_i_ > 6) render(_R_PAREN);
    }
    else     if (foo instanceof gen.ABS.Absyn.EFunCall)
    {
       gen.ABS.Absyn.EFunCall _efuncall = (gen.ABS.Absyn.EFunCall) foo;
       if (_i_ > 7) render(_L_PAREN);
       pp(_efuncall.ident_, 0);
       render("(");
       pp(_efuncall.listpureexp_, 0);
       render(")");
       if (_i_ > 7) render(_R_PAREN);
    }
    else     if (foo instanceof gen.ABS.Absyn.EQualFunCall)
    {
       gen.ABS.Absyn.EQualFunCall _equalfuncall = (gen.ABS.Absyn.EQualFunCall) foo;
       if (_i_ > 7) render(_L_PAREN);
       pp(_equalfuncall.qualtype_, 0);
       render(".");
       pp(_equalfuncall.ident_, 0);
       render("(");
       pp(_equalfuncall.listpureexp_, 0);
       render(")");
       if (_i_ > 7) render(_R_PAREN);
    }
    else     if (foo instanceof gen.ABS.Absyn.ENaryFunCall)
    {
       gen.ABS.Absyn.ENaryFunCall _enaryfuncall = (gen.ABS.Absyn.ENaryFunCall) foo;
       if (_i_ > 7) render(_L_PAREN);
       pp(_enaryfuncall.ident_, 0);
       render("[");
       pp(_enaryfuncall.listpureexp_, 0);
       render("]");
       if (_i_ > 7) render(_R_PAREN);
    }
    else     if (foo instanceof gen.ABS.Absyn.ENaryQualFunCall)
    {
       gen.ABS.Absyn.ENaryQualFunCall _enaryqualfuncall = (gen.ABS.Absyn.ENaryQualFunCall) foo;
       if (_i_ > 7) render(_L_PAREN);
       pp(_enaryqualfuncall.qualtype_, 0);
       render(".");
       pp(_enaryqualfuncall.ident_, 0);
       render("[");
       pp(_enaryqualfuncall.listpureexp_, 0);
       render("]");
       if (_i_ > 7) render(_R_PAREN);
    }
    else     if (foo instanceof gen.ABS.Absyn.EVar)
    {
       gen.ABS.Absyn.EVar _evar = (gen.ABS.Absyn.EVar) foo;
       if (_i_ > 7) render(_L_PAREN);
       pp(_evar.ident_, 0);
       if (_i_ > 7) render(_R_PAREN);
    }
    else     if (foo instanceof gen.ABS.Absyn.EThis)
    {
       gen.ABS.Absyn.EThis _ethis = (gen.ABS.Absyn.EThis) foo;
       if (_i_ > 7) render(_L_PAREN);
       render("this");
       render(".");
       pp(_ethis.ident_, 0);
       if (_i_ > 7) render(_R_PAREN);
    }
    else     if (foo instanceof gen.ABS.Absyn.EQualVar)
    {
       gen.ABS.Absyn.EQualVar _equalvar = (gen.ABS.Absyn.EQualVar) foo;
       if (_i_ > 7) render(_L_PAREN);
       pp(_equalvar.qualtype_, 0);
       render(".");
       pp(_equalvar.ident_, 0);
       if (_i_ > 7) render(_R_PAREN);
    }
    else     if (foo instanceof gen.ABS.Absyn.ESinglConstr)
    {
       gen.ABS.Absyn.ESinglConstr _esinglconstr = (gen.ABS.Absyn.ESinglConstr) foo;
       if (_i_ > 7) render(_L_PAREN);
       pp(_esinglconstr.qualtype_, 0);
       if (_i_ > 7) render(_R_PAREN);
    }
    else     if (foo instanceof gen.ABS.Absyn.EParamConstr)
    {
       gen.ABS.Absyn.EParamConstr _eparamconstr = (gen.ABS.Absyn.EParamConstr) foo;
       if (_i_ > 7) render(_L_PAREN);
       pp(_eparamconstr.qualtype_, 0);
       render("(");
       pp(_eparamconstr.listpureexp_, 0);
       render(")");
       if (_i_ > 7) render(_R_PAREN);
    }
    else     if (foo instanceof gen.ABS.Absyn.ELit)
    {
       gen.ABS.Absyn.ELit _elit = (gen.ABS.Absyn.ELit) foo;
       if (_i_ > 7) render(_L_PAREN);
       pp(_elit.literal_, 0);
       if (_i_ > 7) render(_R_PAREN);
    }
  }

  private static void pp(gen.ABS.Absyn.CaseBranch foo, int _i_)
  {
    if (foo instanceof gen.ABS.Absyn.CaseBranc)
    {
       gen.ABS.Absyn.CaseBranc _casebranc = (gen.ABS.Absyn.CaseBranc) foo;
       if (_i_ > 0) render(_L_PAREN);
       pp(_casebranc.pattern_, 0);
       render("=>");
       pp(_casebranc.pureexp_, 0);
       if (_i_ > 0) render(_R_PAREN);
    }
  }

  private static void pp(gen.ABS.Absyn.ListCaseBranch foo, int _i_)
  {
     for (java.util.Iterator<CaseBranch> it = foo.iterator(); it.hasNext();)
     {
       pp(it.next(), 0);
       if (it.hasNext()) {
         render(";");
       } else {
         render("");
       }
     }
  }

  private static void pp(gen.ABS.Absyn.ListPattern foo, int _i_)
  {
     for (java.util.Iterator<Pattern> it = foo.iterator(); it.hasNext();)
     {
       pp(it.next(), 0);
       if (it.hasNext()) {
         render(",");
       } else {
         render("");
       }
     }
  }

  private static void pp(gen.ABS.Absyn.Pattern foo, int _i_)
  {
    if (foo instanceof gen.ABS.Absyn.PIdent)
    {
       gen.ABS.Absyn.PIdent _pident = (gen.ABS.Absyn.PIdent) foo;
       if (_i_ > 0) render(_L_PAREN);
       pp(_pident.ident_, 0);
       if (_i_ > 0) render(_R_PAREN);
    }
    else     if (foo instanceof gen.ABS.Absyn.PLit)
    {
       gen.ABS.Absyn.PLit _plit = (gen.ABS.Absyn.PLit) foo;
       if (_i_ > 0) render(_L_PAREN);
       pp(_plit.literal_, 0);
       if (_i_ > 0) render(_R_PAREN);
    }
    else     if (foo instanceof gen.ABS.Absyn.PSinglConstr)
    {
       gen.ABS.Absyn.PSinglConstr _psinglconstr = (gen.ABS.Absyn.PSinglConstr) foo;
       if (_i_ > 0) render(_L_PAREN);
       pp(_psinglconstr.typeident_, 0);
       if (_i_ > 0) render(_R_PAREN);
    }
    else     if (foo instanceof gen.ABS.Absyn.PParamConstr)
    {
       gen.ABS.Absyn.PParamConstr _pparamconstr = (gen.ABS.Absyn.PParamConstr) foo;
       if (_i_ > 0) render(_L_PAREN);
       pp(_pparamconstr.typeident_, 0);
       render("(");
       pp(_pparamconstr.listpattern_, 0);
       render(")");
       if (_i_ > 0) render(_R_PAREN);
    }
    else     if (foo instanceof gen.ABS.Absyn.PUnderscore)
    {
       gen.ABS.Absyn.PUnderscore _punderscore = (gen.ABS.Absyn.PUnderscore) foo;
       if (_i_ > 0) render(_L_PAREN);
       render("_");
       if (_i_ > 0) render(_R_PAREN);
    }
  }

  private static void pp(gen.ABS.Absyn.Literal foo, int _i_)
  {
    if (foo instanceof gen.ABS.Absyn.LNull)
    {
       gen.ABS.Absyn.LNull _lnull = (gen.ABS.Absyn.LNull) foo;
       if (_i_ > 0) render(_L_PAREN);
       render("null");
       if (_i_ > 0) render(_R_PAREN);
    }
    else     if (foo instanceof gen.ABS.Absyn.LThis)
    {
       gen.ABS.Absyn.LThis _lthis = (gen.ABS.Absyn.LThis) foo;
       if (_i_ > 0) render(_L_PAREN);
       render("this");
       if (_i_ > 0) render(_R_PAREN);
    }
    else     if (foo instanceof gen.ABS.Absyn.LThisDC)
    {
       gen.ABS.Absyn.LThisDC _lthisdc = (gen.ABS.Absyn.LThisDC) foo;
       if (_i_ > 0) render(_L_PAREN);
       render("thisDC");
       if (_i_ > 0) render(_R_PAREN);
    }
    else     if (foo instanceof gen.ABS.Absyn.LStr)
    {
       gen.ABS.Absyn.LStr _lstr = (gen.ABS.Absyn.LStr) foo;
       if (_i_ > 0) render(_L_PAREN);
       printQuoted(_lstr.string_);
       if (_i_ > 0) render(_R_PAREN);
    }
    else     if (foo instanceof gen.ABS.Absyn.LInt)
    {
       gen.ABS.Absyn.LInt _lint = (gen.ABS.Absyn.LInt) foo;
       if (_i_ > 0) render(_L_PAREN);
       pp(_lint.integer_, 0);
       if (_i_ > 0) render(_R_PAREN);
    }
  }

  private static void pp(gen.ABS.Absyn.EffExp foo, int _i_)
  {
    if (foo instanceof gen.ABS.Absyn.New)
    {
       gen.ABS.Absyn.New _new = (gen.ABS.Absyn.New) foo;
       if (_i_ > 0) render(_L_PAREN);
       render("new");
       pp(_new.type_, 0);
       render("(");
       pp(_new.listpureexp_, 0);
       render(")");
       if (_i_ > 0) render(_R_PAREN);
    }
    else     if (foo instanceof gen.ABS.Absyn.NewLocal)
    {
       gen.ABS.Absyn.NewLocal _newlocal = (gen.ABS.Absyn.NewLocal) foo;
       if (_i_ > 0) render(_L_PAREN);
       render("new");
       render("local");
       pp(_newlocal.type_, 0);
       render("(");
       pp(_newlocal.listpureexp_, 0);
       render(")");
       if (_i_ > 0) render(_R_PAREN);
    }
    else     if (foo instanceof gen.ABS.Absyn.SyncMethCall)
    {
       gen.ABS.Absyn.SyncMethCall _syncmethcall = (gen.ABS.Absyn.SyncMethCall) foo;
       if (_i_ > 0) render(_L_PAREN);
       pp(_syncmethcall.pureexp_, 0);
       render(".");
       pp(_syncmethcall.ident_, 0);
       render("(");
       pp(_syncmethcall.listpureexp_, 0);
       render(")");
       if (_i_ > 0) render(_R_PAREN);
    }
    else     if (foo instanceof gen.ABS.Absyn.ThisSyncMethCall)
    {
       gen.ABS.Absyn.ThisSyncMethCall _thissyncmethcall = (gen.ABS.Absyn.ThisSyncMethCall) foo;
       if (_i_ > 0) render(_L_PAREN);
       render("this");
       render(".");
       pp(_thissyncmethcall.ident_, 0);
       render("(");
       pp(_thissyncmethcall.listpureexp_, 0);
       render(")");
       if (_i_ > 0) render(_R_PAREN);
    }
    else     if (foo instanceof gen.ABS.Absyn.AsyncMethCall)
    {
       gen.ABS.Absyn.AsyncMethCall _asyncmethcall = (gen.ABS.Absyn.AsyncMethCall) foo;
       if (_i_ > 0) render(_L_PAREN);
       pp(_asyncmethcall.pureexp_, 0);
       render("!");
       pp(_asyncmethcall.ident_, 0);
       render("(");
       pp(_asyncmethcall.listpureexp_, 0);
       render(")");
       if (_i_ > 0) render(_R_PAREN);
    }
    else     if (foo instanceof gen.ABS.Absyn.ThisAsyncMethCall)
    {
       gen.ABS.Absyn.ThisAsyncMethCall _thisasyncmethcall = (gen.ABS.Absyn.ThisAsyncMethCall) foo;
       if (_i_ > 0) render(_L_PAREN);
       render("this");
       render("!");
       pp(_thisasyncmethcall.ident_, 0);
       render("(");
       pp(_thisasyncmethcall.listpureexp_, 0);
       render(")");
       if (_i_ > 0) render(_R_PAREN);
    }
    else     if (foo instanceof gen.ABS.Absyn.Get)
    {
       gen.ABS.Absyn.Get _get = (gen.ABS.Absyn.Get) foo;
       if (_i_ > 0) render(_L_PAREN);
       pp(_get.pureexp_, 0);
       render(".");
       render("get");
       if (_i_ > 0) render(_R_PAREN);
    }
    else     if (foo instanceof gen.ABS.Absyn.Spawns)
    {
       gen.ABS.Absyn.Spawns _spawns = (gen.ABS.Absyn.Spawns) foo;
       if (_i_ > 0) render(_L_PAREN);
       pp(_spawns.pureexp_, 0);
       render("spawns");
       pp(_spawns.type_, 0);
       render("(");
       pp(_spawns.listpureexp_, 0);
       render(")");
       if (_i_ > 0) render(_R_PAREN);
    }
  }


  private static void sh(gen.ABS.Absyn.AnyIdent foo)
  {
    if (foo instanceof gen.ABS.Absyn.AnyIden)
    {
       gen.ABS.Absyn.AnyIden _anyiden = (gen.ABS.Absyn.AnyIden) foo;
       render("(");
       render("AnyIden");
       sh(_anyiden.ident_);
       render(")");
    }
    if (foo instanceof gen.ABS.Absyn.AnyTyIden)
    {
       gen.ABS.Absyn.AnyTyIden _anytyiden = (gen.ABS.Absyn.AnyTyIden) foo;
       render("(");
       render("AnyTyIden");
       sh(_anytyiden.typeident_);
       render(")");
    }
  }

  private static void sh(gen.ABS.Absyn.ListAnyIdent foo)
  {
     for (java.util.Iterator<AnyIdent> it = foo.iterator(); it.hasNext();)
     {
       sh(it.next());
       if (it.hasNext())
         render(",");
     }
  }

  private static void sh(gen.ABS.Absyn.Program foo)
  {
    if (foo instanceof gen.ABS.Absyn.Prog)
    {
       gen.ABS.Absyn.Prog _prog = (gen.ABS.Absyn.Prog) foo;
       render("(");
       render("Prog");
       render("[");
       sh(_prog.listmodule_);
       render("]");
       render(")");
    }
  }

  private static void sh(gen.ABS.Absyn.ListModule foo)
  {
     for (java.util.Iterator<Module> it = foo.iterator(); it.hasNext();)
     {
       sh(it.next());
       if (it.hasNext())
         render(",");
     }
  }

  private static void sh(gen.ABS.Absyn.Module foo)
  {
    if (foo instanceof gen.ABS.Absyn.Modul)
    {
       gen.ABS.Absyn.Modul _modul = (gen.ABS.Absyn.Modul) foo;
       render("(");
       render("Modul");
       sh(_modul.qualtype_);
       render("[");
       sh(_modul.listexport_);
       render("]");
       render("[");
       sh(_modul.listimport_);
       render("]");
       render("[");
       sh(_modul.listdecl_);
       render("]");
       sh(_modul.maybeblock_);
       render(")");
    }
  }

  private static void sh(gen.ABS.Absyn.Export foo)
  {
    if (foo instanceof gen.ABS.Absyn.AnyExport)
    {
       gen.ABS.Absyn.AnyExport _anyexport = (gen.ABS.Absyn.AnyExport) foo;
       render("(");
       render("AnyExport");
       render("[");
       sh(_anyexport.listanyident_);
       render("]");
       render(")");
    }
    if (foo instanceof gen.ABS.Absyn.AnyFromExport)
    {
       gen.ABS.Absyn.AnyFromExport _anyfromexport = (gen.ABS.Absyn.AnyFromExport) foo;
       render("(");
       render("AnyFromExport");
       render("[");
       sh(_anyfromexport.listanyident_);
       render("]");
       sh(_anyfromexport.qualtype_);
       render(")");
    }
    if (foo instanceof gen.ABS.Absyn.StarExport)
    {
       gen.ABS.Absyn.StarExport _starexport = (gen.ABS.Absyn.StarExport) foo;
       render("StarExport");
    }
    if (foo instanceof gen.ABS.Absyn.StarFromExport)
    {
       gen.ABS.Absyn.StarFromExport _starfromexport = (gen.ABS.Absyn.StarFromExport) foo;
       render("(");
       render("StarFromExport");
       sh(_starfromexport.qualtype_);
       render(")");
    }
  }

  private static void sh(gen.ABS.Absyn.ListExport foo)
  {
     for (java.util.Iterator<Export> it = foo.iterator(); it.hasNext();)
     {
       sh(it.next());
       if (it.hasNext())
         render(",");
     }
  }

  private static void sh(gen.ABS.Absyn.Import foo)
  {
    if (foo instanceof gen.ABS.Absyn.AnyImport)
    {
       gen.ABS.Absyn.AnyImport _anyimport = (gen.ABS.Absyn.AnyImport) foo;
       render("(");
       render("AnyImport");
       sh(_anyimport.importtype_);
       sh(_anyimport.qualtype_);
       sh(_anyimport.anyident_);
       render(")");
    }
    if (foo instanceof gen.ABS.Absyn.AnyFromImport)
    {
       gen.ABS.Absyn.AnyFromImport _anyfromimport = (gen.ABS.Absyn.AnyFromImport) foo;
       render("(");
       render("AnyFromImport");
       sh(_anyfromimport.importtype_);
       render("[");
       sh(_anyfromimport.listanyident_);
       render("]");
       sh(_anyfromimport.qualtype_);
       render(")");
    }
    if (foo instanceof gen.ABS.Absyn.StarFromImport)
    {
       gen.ABS.Absyn.StarFromImport _starfromimport = (gen.ABS.Absyn.StarFromImport) foo;
       render("(");
       render("StarFromImport");
       sh(_starfromimport.importtype_);
       sh(_starfromimport.qualtype_);
       render(")");
    }
  }

  private static void sh(gen.ABS.Absyn.ListImport foo)
  {
     for (java.util.Iterator<Import> it = foo.iterator(); it.hasNext();)
     {
       sh(it.next());
       if (it.hasNext())
         render(",");
     }
  }

  private static void sh(gen.ABS.Absyn.ImportType foo)
  {
    if (foo instanceof gen.ABS.Absyn.ForeignImport)
    {
       gen.ABS.Absyn.ForeignImport _foreignimport = (gen.ABS.Absyn.ForeignImport) foo;
       render("ForeignImport");
    }
    if (foo instanceof gen.ABS.Absyn.NormalImport)
    {
       gen.ABS.Absyn.NormalImport _normalimport = (gen.ABS.Absyn.NormalImport) foo;
       render("NormalImport");
    }
  }

  private static void sh(gen.ABS.Absyn.Type foo)
  {
    if (foo instanceof gen.ABS.Absyn.TUnderscore)
    {
       gen.ABS.Absyn.TUnderscore _tunderscore = (gen.ABS.Absyn.TUnderscore) foo;
       render("TUnderscore");
    }
    if (foo instanceof gen.ABS.Absyn.TSimple)
    {
       gen.ABS.Absyn.TSimple _tsimple = (gen.ABS.Absyn.TSimple) foo;
       render("(");
       render("TSimple");
       sh(_tsimple.qualtype_);
       render(")");
    }
    if (foo instanceof gen.ABS.Absyn.TGen)
    {
       gen.ABS.Absyn.TGen _tgen = (gen.ABS.Absyn.TGen) foo;
       render("(");
       render("TGen");
       sh(_tgen.qualtype_);
       render("[");
       sh(_tgen.listtype_);
       render("]");
       render(")");
    }
  }

  private static void sh(gen.ABS.Absyn.ListType foo)
  {
     for (java.util.Iterator<Type> it = foo.iterator(); it.hasNext();)
     {
       sh(it.next());
       if (it.hasNext())
         render(",");
     }
  }

  private static void sh(gen.ABS.Absyn.ListQualType foo)
  {
     for (java.util.Iterator<QualType> it = foo.iterator(); it.hasNext();)
     {
       sh(it.next());
       if (it.hasNext())
         render(",");
     }
  }

  private static void sh(gen.ABS.Absyn.QualType foo)
  {
    if (foo instanceof gen.ABS.Absyn.QType)
    {
       gen.ABS.Absyn.QType _qtype = (gen.ABS.Absyn.QType) foo;
       render("(");
       render("QType");
       render("[");
       sh(_qtype.listqualtypesegment_);
       render("]");
       render(")");
    }
  }

  private static void sh(gen.ABS.Absyn.QualTypeSegment foo)
  {
    if (foo instanceof gen.ABS.Absyn.QTypeSegment)
    {
       gen.ABS.Absyn.QTypeSegment _qtypesegment = (gen.ABS.Absyn.QTypeSegment) foo;
       render("(");
       render("QTypeSegment");
       sh(_qtypesegment.typeident_);
       render(")");
    }
  }

  private static void sh(gen.ABS.Absyn.ListQualTypeSegment foo)
  {
     for (java.util.Iterator<QualTypeSegment> it = foo.iterator(); it.hasNext();)
     {
       sh(it.next());
       if (it.hasNext())
         render(",");
     }
  }

  private static void sh(gen.ABS.Absyn.ListDecl foo)
  {
     for (java.util.Iterator<Decl> it = foo.iterator(); it.hasNext();)
     {
       sh(it.next());
       if (it.hasNext())
         render(",");
     }
  }

  private static void sh(gen.ABS.Absyn.Decl foo)
  {
    if (foo instanceof gen.ABS.Absyn.TypeDecl)
    {
       gen.ABS.Absyn.TypeDecl _typedecl = (gen.ABS.Absyn.TypeDecl) foo;
       render("(");
       render("TypeDecl");
       sh(_typedecl.typeident_);
       sh(_typedecl.type_);
       render(")");
    }
    if (foo instanceof gen.ABS.Absyn.ExceptionDecl)
    {
       gen.ABS.Absyn.ExceptionDecl _exceptiondecl = (gen.ABS.Absyn.ExceptionDecl) foo;
       render("(");
       render("ExceptionDecl");
       sh(_exceptiondecl.constrident_);
       render(")");
    }
    if (foo instanceof gen.ABS.Absyn.DataDecl)
    {
       gen.ABS.Absyn.DataDecl _datadecl = (gen.ABS.Absyn.DataDecl) foo;
       render("(");
       render("DataDecl");
       sh(_datadecl.typeident_);
       render("[");
       sh(_datadecl.listconstrident_);
       render("]");
       render(")");
    }
    if (foo instanceof gen.ABS.Absyn.DataParDecl)
    {
       gen.ABS.Absyn.DataParDecl _datapardecl = (gen.ABS.Absyn.DataParDecl) foo;
       render("(");
       render("DataParDecl");
       sh(_datapardecl.typeident_);
       render("[");
       sh(_datapardecl.listtypeident_);
       render("]");
       render("[");
       sh(_datapardecl.listconstrident_);
       render("]");
       render(")");
    }
    if (foo instanceof gen.ABS.Absyn.FunDecl)
    {
       gen.ABS.Absyn.FunDecl _fundecl = (gen.ABS.Absyn.FunDecl) foo;
       render("(");
       render("FunDecl");
       sh(_fundecl.type_);
       sh(_fundecl.ident_);
       render("[");
       sh(_fundecl.listparam_);
       render("]");
       sh(_fundecl.funbody_);
       render(")");
    }
    if (foo instanceof gen.ABS.Absyn.FunParDecl)
    {
       gen.ABS.Absyn.FunParDecl _funpardecl = (gen.ABS.Absyn.FunParDecl) foo;
       render("(");
       render("FunParDecl");
       sh(_funpardecl.type_);
       sh(_funpardecl.ident_);
       render("[");
       sh(_funpardecl.listtypeident_);
       render("]");
       render("[");
       sh(_funpardecl.listparam_);
       render("]");
       sh(_funpardecl.funbody_);
       render(")");
    }
    if (foo instanceof gen.ABS.Absyn.InterfDecl)
    {
       gen.ABS.Absyn.InterfDecl _interfdecl = (gen.ABS.Absyn.InterfDecl) foo;
       render("(");
       render("InterfDecl");
       sh(_interfdecl.typeident_);
       render("[");
       sh(_interfdecl.listmethsignat_);
       render("]");
       render(")");
    }
    if (foo instanceof gen.ABS.Absyn.ExtendsDecl)
    {
       gen.ABS.Absyn.ExtendsDecl _extendsdecl = (gen.ABS.Absyn.ExtendsDecl) foo;
       render("(");
       render("ExtendsDecl");
       sh(_extendsdecl.typeident_);
       render("[");
       sh(_extendsdecl.listqualtype_);
       render("]");
       render("[");
       sh(_extendsdecl.listmethsignat_);
       render("]");
       render(")");
    }
    if (foo instanceof gen.ABS.Absyn.ClassDecl)
    {
       gen.ABS.Absyn.ClassDecl _classdecl = (gen.ABS.Absyn.ClassDecl) foo;
       render("(");
       render("ClassDecl");
       sh(_classdecl.typeident_);
       render("[");
       sh(_classdecl.listclassbody_1);
       render("]");
       sh(_classdecl.maybeblock_);
       render("[");
       sh(_classdecl.listclassbody_2);
       render("]");
       render(")");
    }
    if (foo instanceof gen.ABS.Absyn.ClassParamDecl)
    {
       gen.ABS.Absyn.ClassParamDecl _classparamdecl = (gen.ABS.Absyn.ClassParamDecl) foo;
       render("(");
       render("ClassParamDecl");
       sh(_classparamdecl.typeident_);
       render("[");
       sh(_classparamdecl.listparam_);
       render("]");
       render("[");
       sh(_classparamdecl.listclassbody_1);
       render("]");
       sh(_classparamdecl.maybeblock_);
       render("[");
       sh(_classparamdecl.listclassbody_2);
       render("]");
       render(")");
    }
    if (foo instanceof gen.ABS.Absyn.ClassImplements)
    {
       gen.ABS.Absyn.ClassImplements _classimplements = (gen.ABS.Absyn.ClassImplements) foo;
       render("(");
       render("ClassImplements");
       sh(_classimplements.typeident_);
       render("[");
       sh(_classimplements.listqualtype_);
       render("]");
       render("[");
       sh(_classimplements.listclassbody_1);
       render("]");
       sh(_classimplements.maybeblock_);
       render("[");
       sh(_classimplements.listclassbody_2);
       render("]");
       render(")");
    }
    if (foo instanceof gen.ABS.Absyn.ClassParamImplements)
    {
       gen.ABS.Absyn.ClassParamImplements _classparamimplements = (gen.ABS.Absyn.ClassParamImplements) foo;
       render("(");
       render("ClassParamImplements");
       sh(_classparamimplements.typeident_);
       render("[");
       sh(_classparamimplements.listparam_);
       render("]");
       render("[");
       sh(_classparamimplements.listqualtype_);
       render("]");
       render("[");
       sh(_classparamimplements.listclassbody_1);
       render("]");
       sh(_classparamimplements.maybeblock_);
       render("[");
       sh(_classparamimplements.listclassbody_2);
       render("]");
       render(")");
    }
  }

  private static void sh(gen.ABS.Absyn.ConstrIdent foo)
  {
    if (foo instanceof gen.ABS.Absyn.SinglConstrIdent)
    {
       gen.ABS.Absyn.SinglConstrIdent _singlconstrident = (gen.ABS.Absyn.SinglConstrIdent) foo;
       render("(");
       render("SinglConstrIdent");
       sh(_singlconstrident.typeident_);
       render(")");
    }
    if (foo instanceof gen.ABS.Absyn.ParamConstrIdent)
    {
       gen.ABS.Absyn.ParamConstrIdent _paramconstrident = (gen.ABS.Absyn.ParamConstrIdent) foo;
       render("(");
       render("ParamConstrIdent");
       sh(_paramconstrident.typeident_);
       render("[");
       sh(_paramconstrident.listconstrtype_);
       render("]");
       render(")");
    }
  }

  private static void sh(gen.ABS.Absyn.ConstrType foo)
  {
    if (foo instanceof gen.ABS.Absyn.EmptyConstrType)
    {
       gen.ABS.Absyn.EmptyConstrType _emptyconstrtype = (gen.ABS.Absyn.EmptyConstrType) foo;
       render("(");
       render("EmptyConstrType");
       sh(_emptyconstrtype.type_);
       render(")");
    }
    if (foo instanceof gen.ABS.Absyn.RecordConstrType)
    {
       gen.ABS.Absyn.RecordConstrType _recordconstrtype = (gen.ABS.Absyn.RecordConstrType) foo;
       render("(");
       render("RecordConstrType");
       sh(_recordconstrtype.type_);
       sh(_recordconstrtype.ident_);
       render(")");
    }
  }

  private static void sh(gen.ABS.Absyn.ListConstrType foo)
  {
     for (java.util.Iterator<ConstrType> it = foo.iterator(); it.hasNext();)
     {
       sh(it.next());
       if (it.hasNext())
         render(",");
     }
  }

  private static void sh(gen.ABS.Absyn.ListTypeIdent foo)
  {
     for (java.util.Iterator<String> it = foo.iterator(); it.hasNext();)
     {
       sh(it.next());
       if (it.hasNext())
         render(",");
     }
  }

  private static void sh(gen.ABS.Absyn.ListConstrIdent foo)
  {
     for (java.util.Iterator<ConstrIdent> it = foo.iterator(); it.hasNext();)
     {
       sh(it.next());
       if (it.hasNext())
         render(",");
     }
  }

  private static void sh(gen.ABS.Absyn.FunBody foo)
  {
    if (foo instanceof gen.ABS.Absyn.BuiltinFunBody)
    {
       gen.ABS.Absyn.BuiltinFunBody _builtinfunbody = (gen.ABS.Absyn.BuiltinFunBody) foo;
       render("BuiltinFunBody");
    }
    if (foo instanceof gen.ABS.Absyn.NormalFunBody)
    {
       gen.ABS.Absyn.NormalFunBody _normalfunbody = (gen.ABS.Absyn.NormalFunBody) foo;
       render("(");
       render("NormalFunBody");
       sh(_normalfunbody.pureexp_);
       render(")");
    }
  }

  private static void sh(gen.ABS.Absyn.MethSignat foo)
  {
    if (foo instanceof gen.ABS.Absyn.MethSig)
    {
       gen.ABS.Absyn.MethSig _methsig = (gen.ABS.Absyn.MethSig) foo;
       render("(");
       render("MethSig");
       sh(_methsig.type_);
       sh(_methsig.ident_);
       render("[");
       sh(_methsig.listparam_);
       render("]");
       render(")");
    }
  }

  private static void sh(gen.ABS.Absyn.ListMethSignat foo)
  {
     for (java.util.Iterator<MethSignat> it = foo.iterator(); it.hasNext();)
     {
       sh(it.next());
       if (it.hasNext())
         render(",");
     }
  }

  private static void sh(gen.ABS.Absyn.ClassBody foo)
  {
    if (foo instanceof gen.ABS.Absyn.FieldClassBody)
    {
       gen.ABS.Absyn.FieldClassBody _fieldclassbody = (gen.ABS.Absyn.FieldClassBody) foo;
       render("(");
       render("FieldClassBody");
       sh(_fieldclassbody.type_);
       sh(_fieldclassbody.ident_);
       render(")");
    }
    if (foo instanceof gen.ABS.Absyn.FieldAssignClassBody)
    {
       gen.ABS.Absyn.FieldAssignClassBody _fieldassignclassbody = (gen.ABS.Absyn.FieldAssignClassBody) foo;
       render("(");
       render("FieldAssignClassBody");
       sh(_fieldassignclassbody.type_);
       sh(_fieldassignclassbody.ident_);
       sh(_fieldassignclassbody.pureexp_);
       render(")");
    }
    if (foo instanceof gen.ABS.Absyn.MethClassBody)
    {
       gen.ABS.Absyn.MethClassBody _methclassbody = (gen.ABS.Absyn.MethClassBody) foo;
       render("(");
       render("MethClassBody");
       sh(_methclassbody.type_);
       sh(_methclassbody.ident_);
       render("[");
       sh(_methclassbody.listparam_);
       render("]");
       sh(_methclassbody.block_);
       render(")");
    }
  }

  private static void sh(gen.ABS.Absyn.ListClassBody foo)
  {
     for (java.util.Iterator<ClassBody> it = foo.iterator(); it.hasNext();)
     {
       sh(it.next());
       if (it.hasNext())
         render(",");
     }
  }

  private static void sh(gen.ABS.Absyn.Block foo)
  {
    if (foo instanceof gen.ABS.Absyn.Bloc)
    {
       gen.ABS.Absyn.Bloc _bloc = (gen.ABS.Absyn.Bloc) foo;
       render("(");
       render("Bloc");
       render("[");
       sh(_bloc.liststm_);
       render("]");
       render(")");
    }
  }

  private static void sh(gen.ABS.Absyn.MaybeBlock foo)
  {
    if (foo instanceof gen.ABS.Absyn.JustBlock)
    {
       gen.ABS.Absyn.JustBlock _justblock = (gen.ABS.Absyn.JustBlock) foo;
       render("(");
       render("JustBlock");
       sh(_justblock.block_);
       render(")");
    }
    if (foo instanceof gen.ABS.Absyn.NoBlock)
    {
       gen.ABS.Absyn.NoBlock _noblock = (gen.ABS.Absyn.NoBlock) foo;
       render("NoBlock");
    }
  }

  private static void sh(gen.ABS.Absyn.ListParam foo)
  {
     for (java.util.Iterator<Param> it = foo.iterator(); it.hasNext();)
     {
       sh(it.next());
       if (it.hasNext())
         render(",");
     }
  }

  private static void sh(gen.ABS.Absyn.Param foo)
  {
    if (foo instanceof gen.ABS.Absyn.Par)
    {
       gen.ABS.Absyn.Par _par = (gen.ABS.Absyn.Par) foo;
       render("(");
       render("Par");
       sh(_par.type_);
       sh(_par.ident_);
       render(")");
    }
  }

  private static void sh(gen.ABS.Absyn.ListStm foo)
  {
     for (java.util.Iterator<Stm> it = foo.iterator(); it.hasNext();)
     {
       sh(it.next());
       if (it.hasNext())
         render(",");
     }
  }

  private static void sh(gen.ABS.Absyn.Stm foo)
  {
    if (foo instanceof gen.ABS.Absyn.SExp)
    {
       gen.ABS.Absyn.SExp _sexp = (gen.ABS.Absyn.SExp) foo;
       render("(");
       render("SExp");
       sh(_sexp.exp_);
       render(")");
    }
    if (foo instanceof gen.ABS.Absyn.SBlock)
    {
       gen.ABS.Absyn.SBlock _sblock = (gen.ABS.Absyn.SBlock) foo;
       render("(");
       render("SBlock");
       render("[");
       sh(_sblock.liststm_);
       render("]");
       render(")");
    }
    if (foo instanceof gen.ABS.Absyn.SWhile)
    {
       gen.ABS.Absyn.SWhile _swhile = (gen.ABS.Absyn.SWhile) foo;
       render("(");
       render("SWhile");
       sh(_swhile.pureexp_);
       sh(_swhile.stm_);
       render(")");
    }
    if (foo instanceof gen.ABS.Absyn.SReturn)
    {
       gen.ABS.Absyn.SReturn _sreturn = (gen.ABS.Absyn.SReturn) foo;
       render("(");
       render("SReturn");
       sh(_sreturn.exp_);
       render(")");
    }
    if (foo instanceof gen.ABS.Absyn.SAss)
    {
       gen.ABS.Absyn.SAss _sass = (gen.ABS.Absyn.SAss) foo;
       render("(");
       render("SAss");
       sh(_sass.ident_);
       sh(_sass.exp_);
       render(")");
    }
    if (foo instanceof gen.ABS.Absyn.SFieldAss)
    {
       gen.ABS.Absyn.SFieldAss _sfieldass = (gen.ABS.Absyn.SFieldAss) foo;
       render("(");
       render("SFieldAss");
       sh(_sfieldass.ident_);
       sh(_sfieldass.exp_);
       render(")");
    }
    if (foo instanceof gen.ABS.Absyn.SDec)
    {
       gen.ABS.Absyn.SDec _sdec = (gen.ABS.Absyn.SDec) foo;
       render("(");
       render("SDec");
       sh(_sdec.type_);
       sh(_sdec.ident_);
       render(")");
    }
    if (foo instanceof gen.ABS.Absyn.SDecAss)
    {
       gen.ABS.Absyn.SDecAss _sdecass = (gen.ABS.Absyn.SDecAss) foo;
       render("(");
       render("SDecAss");
       sh(_sdecass.type_);
       sh(_sdecass.ident_);
       sh(_sdecass.exp_);
       render(")");
    }
    if (foo instanceof gen.ABS.Absyn.SIf)
    {
       gen.ABS.Absyn.SIf _sif = (gen.ABS.Absyn.SIf) foo;
       render("(");
       render("SIf");
       sh(_sif.pureexp_);
       sh(_sif.stm_);
       render(")");
    }
    if (foo instanceof gen.ABS.Absyn.SIfElse)
    {
       gen.ABS.Absyn.SIfElse _sifelse = (gen.ABS.Absyn.SIfElse) foo;
       render("(");
       render("SIfElse");
       sh(_sifelse.pureexp_);
       sh(_sifelse.stm_1);
       sh(_sifelse.stm_2);
       render(")");
    }
    if (foo instanceof gen.ABS.Absyn.SSuspend)
    {
       gen.ABS.Absyn.SSuspend _ssuspend = (gen.ABS.Absyn.SSuspend) foo;
       render("SSuspend");
    }
    if (foo instanceof gen.ABS.Absyn.SSkip)
    {
       gen.ABS.Absyn.SSkip _sskip = (gen.ABS.Absyn.SSkip) foo;
       render("SSkip");
    }
    if (foo instanceof gen.ABS.Absyn.SAssert)
    {
       gen.ABS.Absyn.SAssert _sassert = (gen.ABS.Absyn.SAssert) foo;
       render("(");
       render("SAssert");
       sh(_sassert.pureexp_);
       render(")");
    }
    if (foo instanceof gen.ABS.Absyn.SAwait)
    {
       gen.ABS.Absyn.SAwait _sawait = (gen.ABS.Absyn.SAwait) foo;
       render("(");
       render("SAwait");
       sh(_sawait.guard_);
       render(")");
    }
    if (foo instanceof gen.ABS.Absyn.SThrow)
    {
       gen.ABS.Absyn.SThrow _sthrow = (gen.ABS.Absyn.SThrow) foo;
       render("(");
       render("SThrow");
       sh(_sthrow.pureexp_);
       render(")");
    }
    if (foo instanceof gen.ABS.Absyn.STryCatchFinally)
    {
       gen.ABS.Absyn.STryCatchFinally _strycatchfinally = (gen.ABS.Absyn.STryCatchFinally) foo;
       render("(");
       render("STryCatchFinally");
       sh(_strycatchfinally.stm_);
       render("[");
       sh(_strycatchfinally.listcatchbranch_);
       render("]");
       sh(_strycatchfinally.maybefinally_);
       render(")");
    }
  }

  private static void sh(gen.ABS.Absyn.CatchBranch foo)
  {
    if (foo instanceof gen.ABS.Absyn.CatchBranc)
    {
       gen.ABS.Absyn.CatchBranc _catchbranc = (gen.ABS.Absyn.CatchBranc) foo;
       render("(");
       render("CatchBranc");
       sh(_catchbranc.pattern_);
       sh(_catchbranc.stm_);
       render(")");
    }
  }

  private static void sh(gen.ABS.Absyn.ListCatchBranch foo)
  {
     for (java.util.Iterator<CatchBranch> it = foo.iterator(); it.hasNext();)
     {
       sh(it.next());
       if (it.hasNext())
         render(",");
     }
  }

  private static void sh(gen.ABS.Absyn.MaybeFinally foo)
  {
    if (foo instanceof gen.ABS.Absyn.JustFinally)
    {
       gen.ABS.Absyn.JustFinally _justfinally = (gen.ABS.Absyn.JustFinally) foo;
       render("(");
       render("JustFinally");
       sh(_justfinally.stm_);
       render(")");
    }
    if (foo instanceof gen.ABS.Absyn.NoFinally)
    {
       gen.ABS.Absyn.NoFinally _nofinally = (gen.ABS.Absyn.NoFinally) foo;
       render("NoFinally");
    }
  }

  private static void sh(gen.ABS.Absyn.Guard foo)
  {
    if (foo instanceof gen.ABS.Absyn.VarGuard)
    {
       gen.ABS.Absyn.VarGuard _varguard = (gen.ABS.Absyn.VarGuard) foo;
       render("(");
       render("VarGuard");
       sh(_varguard.ident_);
       render(")");
    }
    if (foo instanceof gen.ABS.Absyn.FieldGuard)
    {
       gen.ABS.Absyn.FieldGuard _fieldguard = (gen.ABS.Absyn.FieldGuard) foo;
       render("(");
       render("FieldGuard");
       sh(_fieldguard.ident_);
       render(")");
    }
    if (foo instanceof gen.ABS.Absyn.ExpGuard)
    {
       gen.ABS.Absyn.ExpGuard _expguard = (gen.ABS.Absyn.ExpGuard) foo;
       render("(");
       render("ExpGuard");
       sh(_expguard.pureexp_);
       render(")");
    }
    if (foo instanceof gen.ABS.Absyn.AndGuard)
    {
       gen.ABS.Absyn.AndGuard _andguard = (gen.ABS.Absyn.AndGuard) foo;
       render("(");
       render("AndGuard");
       sh(_andguard.guard_1);
       sh(_andguard.guard_2);
       render(")");
    }
  }

  private static void sh(gen.ABS.Absyn.Exp foo)
  {
    if (foo instanceof gen.ABS.Absyn.ExpP)
    {
       gen.ABS.Absyn.ExpP _expp = (gen.ABS.Absyn.ExpP) foo;
       render("(");
       render("ExpP");
       sh(_expp.pureexp_);
       render(")");
    }
    if (foo instanceof gen.ABS.Absyn.ExpE)
    {
       gen.ABS.Absyn.ExpE _expe = (gen.ABS.Absyn.ExpE) foo;
       render("(");
       render("ExpE");
       sh(_expe.effexp_);
       render(")");
    }
  }

  private static void sh(gen.ABS.Absyn.ListPureExp foo)
  {
     for (java.util.Iterator<PureExp> it = foo.iterator(); it.hasNext();)
     {
       sh(it.next());
       if (it.hasNext())
         render(",");
     }
  }

  private static void sh(gen.ABS.Absyn.PureExp foo)
  {
    if (foo instanceof gen.ABS.Absyn.EOr)
    {
       gen.ABS.Absyn.EOr _eor = (gen.ABS.Absyn.EOr) foo;
       render("(");
       render("EOr");
       sh(_eor.pureexp_1);
       sh(_eor.pureexp_2);
       render(")");
    }
    if (foo instanceof gen.ABS.Absyn.Let)
    {
       gen.ABS.Absyn.Let _let = (gen.ABS.Absyn.Let) foo;
       render("(");
       render("Let");
       sh(_let.param_);
       sh(_let.pureexp_1);
       sh(_let.pureexp_2);
       render(")");
    }
    if (foo instanceof gen.ABS.Absyn.If)
    {
       gen.ABS.Absyn.If _if = (gen.ABS.Absyn.If) foo;
       render("(");
       render("If");
       sh(_if.pureexp_1);
       sh(_if.pureexp_2);
       sh(_if.pureexp_3);
       render(")");
    }
    if (foo instanceof gen.ABS.Absyn.Case)
    {
       gen.ABS.Absyn.Case _case = (gen.ABS.Absyn.Case) foo;
       render("(");
       render("Case");
       sh(_case.pureexp_);
       render("[");
       sh(_case.listcasebranch_);
       render("]");
       render(")");
    }
    if (foo instanceof gen.ABS.Absyn.EAnd)
    {
       gen.ABS.Absyn.EAnd _eand = (gen.ABS.Absyn.EAnd) foo;
       render("(");
       render("EAnd");
       sh(_eand.pureexp_1);
       sh(_eand.pureexp_2);
       render(")");
    }
    if (foo instanceof gen.ABS.Absyn.EEq)
    {
       gen.ABS.Absyn.EEq _eeq = (gen.ABS.Absyn.EEq) foo;
       render("(");
       render("EEq");
       sh(_eeq.pureexp_1);
       sh(_eeq.pureexp_2);
       render(")");
    }
    if (foo instanceof gen.ABS.Absyn.ENeq)
    {
       gen.ABS.Absyn.ENeq _eneq = (gen.ABS.Absyn.ENeq) foo;
       render("(");
       render("ENeq");
       sh(_eneq.pureexp_1);
       sh(_eneq.pureexp_2);
       render(")");
    }
    if (foo instanceof gen.ABS.Absyn.ELt)
    {
       gen.ABS.Absyn.ELt _elt = (gen.ABS.Absyn.ELt) foo;
       render("(");
       render("ELt");
       sh(_elt.pureexp_1);
       sh(_elt.pureexp_2);
       render(")");
    }
    if (foo instanceof gen.ABS.Absyn.ELe)
    {
       gen.ABS.Absyn.ELe _ele = (gen.ABS.Absyn.ELe) foo;
       render("(");
       render("ELe");
       sh(_ele.pureexp_1);
       sh(_ele.pureexp_2);
       render(")");
    }
    if (foo instanceof gen.ABS.Absyn.EGt)
    {
       gen.ABS.Absyn.EGt _egt = (gen.ABS.Absyn.EGt) foo;
       render("(");
       render("EGt");
       sh(_egt.pureexp_1);
       sh(_egt.pureexp_2);
       render(")");
    }
    if (foo instanceof gen.ABS.Absyn.EGe)
    {
       gen.ABS.Absyn.EGe _ege = (gen.ABS.Absyn.EGe) foo;
       render("(");
       render("EGe");
       sh(_ege.pureexp_1);
       sh(_ege.pureexp_2);
       render(")");
    }
    if (foo instanceof gen.ABS.Absyn.EAdd)
    {
       gen.ABS.Absyn.EAdd _eadd = (gen.ABS.Absyn.EAdd) foo;
       render("(");
       render("EAdd");
       sh(_eadd.pureexp_1);
       sh(_eadd.pureexp_2);
       render(")");
    }
    if (foo instanceof gen.ABS.Absyn.ESub)
    {
       gen.ABS.Absyn.ESub _esub = (gen.ABS.Absyn.ESub) foo;
       render("(");
       render("ESub");
       sh(_esub.pureexp_1);
       sh(_esub.pureexp_2);
       render(")");
    }
    if (foo instanceof gen.ABS.Absyn.EMul)
    {
       gen.ABS.Absyn.EMul _emul = (gen.ABS.Absyn.EMul) foo;
       render("(");
       render("EMul");
       sh(_emul.pureexp_1);
       sh(_emul.pureexp_2);
       render(")");
    }
    if (foo instanceof gen.ABS.Absyn.EDiv)
    {
       gen.ABS.Absyn.EDiv _ediv = (gen.ABS.Absyn.EDiv) foo;
       render("(");
       render("EDiv");
       sh(_ediv.pureexp_1);
       sh(_ediv.pureexp_2);
       render(")");
    }
    if (foo instanceof gen.ABS.Absyn.EMod)
    {
       gen.ABS.Absyn.EMod _emod = (gen.ABS.Absyn.EMod) foo;
       render("(");
       render("EMod");
       sh(_emod.pureexp_1);
       sh(_emod.pureexp_2);
       render(")");
    }
    if (foo instanceof gen.ABS.Absyn.ELogNeg)
    {
       gen.ABS.Absyn.ELogNeg _elogneg = (gen.ABS.Absyn.ELogNeg) foo;
       render("(");
       render("ELogNeg");
       sh(_elogneg.pureexp_);
       render(")");
    }
    if (foo instanceof gen.ABS.Absyn.EIntNeg)
    {
       gen.ABS.Absyn.EIntNeg _eintneg = (gen.ABS.Absyn.EIntNeg) foo;
       render("(");
       render("EIntNeg");
       sh(_eintneg.pureexp_);
       render(")");
    }
    if (foo instanceof gen.ABS.Absyn.EFunCall)
    {
       gen.ABS.Absyn.EFunCall _efuncall = (gen.ABS.Absyn.EFunCall) foo;
       render("(");
       render("EFunCall");
       sh(_efuncall.ident_);
       render("[");
       sh(_efuncall.listpureexp_);
       render("]");
       render(")");
    }
    if (foo instanceof gen.ABS.Absyn.EQualFunCall)
    {
       gen.ABS.Absyn.EQualFunCall _equalfuncall = (gen.ABS.Absyn.EQualFunCall) foo;
       render("(");
       render("EQualFunCall");
       sh(_equalfuncall.qualtype_);
       sh(_equalfuncall.ident_);
       render("[");
       sh(_equalfuncall.listpureexp_);
       render("]");
       render(")");
    }
    if (foo instanceof gen.ABS.Absyn.ENaryFunCall)
    {
       gen.ABS.Absyn.ENaryFunCall _enaryfuncall = (gen.ABS.Absyn.ENaryFunCall) foo;
       render("(");
       render("ENaryFunCall");
       sh(_enaryfuncall.ident_);
       render("[");
       sh(_enaryfuncall.listpureexp_);
       render("]");
       render(")");
    }
    if (foo instanceof gen.ABS.Absyn.ENaryQualFunCall)
    {
       gen.ABS.Absyn.ENaryQualFunCall _enaryqualfuncall = (gen.ABS.Absyn.ENaryQualFunCall) foo;
       render("(");
       render("ENaryQualFunCall");
       sh(_enaryqualfuncall.qualtype_);
       sh(_enaryqualfuncall.ident_);
       render("[");
       sh(_enaryqualfuncall.listpureexp_);
       render("]");
       render(")");
    }
    if (foo instanceof gen.ABS.Absyn.EVar)
    {
       gen.ABS.Absyn.EVar _evar = (gen.ABS.Absyn.EVar) foo;
       render("(");
       render("EVar");
       sh(_evar.ident_);
       render(")");
    }
    if (foo instanceof gen.ABS.Absyn.EThis)
    {
       gen.ABS.Absyn.EThis _ethis = (gen.ABS.Absyn.EThis) foo;
       render("(");
       render("EThis");
       sh(_ethis.ident_);
       render(")");
    }
    if (foo instanceof gen.ABS.Absyn.EQualVar)
    {
       gen.ABS.Absyn.EQualVar _equalvar = (gen.ABS.Absyn.EQualVar) foo;
       render("(");
       render("EQualVar");
       sh(_equalvar.qualtype_);
       sh(_equalvar.ident_);
       render(")");
    }
    if (foo instanceof gen.ABS.Absyn.ESinglConstr)
    {
       gen.ABS.Absyn.ESinglConstr _esinglconstr = (gen.ABS.Absyn.ESinglConstr) foo;
       render("(");
       render("ESinglConstr");
       sh(_esinglconstr.qualtype_);
       render(")");
    }
    if (foo instanceof gen.ABS.Absyn.EParamConstr)
    {
       gen.ABS.Absyn.EParamConstr _eparamconstr = (gen.ABS.Absyn.EParamConstr) foo;
       render("(");
       render("EParamConstr");
       sh(_eparamconstr.qualtype_);
       render("[");
       sh(_eparamconstr.listpureexp_);
       render("]");
       render(")");
    }
    if (foo instanceof gen.ABS.Absyn.ELit)
    {
       gen.ABS.Absyn.ELit _elit = (gen.ABS.Absyn.ELit) foo;
       render("(");
       render("ELit");
       sh(_elit.literal_);
       render(")");
    }
  }

  private static void sh(gen.ABS.Absyn.CaseBranch foo)
  {
    if (foo instanceof gen.ABS.Absyn.CaseBranc)
    {
       gen.ABS.Absyn.CaseBranc _casebranc = (gen.ABS.Absyn.CaseBranc) foo;
       render("(");
       render("CaseBranc");
       sh(_casebranc.pattern_);
       sh(_casebranc.pureexp_);
       render(")");
    }
  }

  private static void sh(gen.ABS.Absyn.ListCaseBranch foo)
  {
     for (java.util.Iterator<CaseBranch> it = foo.iterator(); it.hasNext();)
     {
       sh(it.next());
       if (it.hasNext())
         render(",");
     }
  }

  private static void sh(gen.ABS.Absyn.ListPattern foo)
  {
     for (java.util.Iterator<Pattern> it = foo.iterator(); it.hasNext();)
     {
       sh(it.next());
       if (it.hasNext())
         render(",");
     }
  }

  private static void sh(gen.ABS.Absyn.Pattern foo)
  {
    if (foo instanceof gen.ABS.Absyn.PIdent)
    {
       gen.ABS.Absyn.PIdent _pident = (gen.ABS.Absyn.PIdent) foo;
       render("(");
       render("PIdent");
       sh(_pident.ident_);
       render(")");
    }
    if (foo instanceof gen.ABS.Absyn.PLit)
    {
       gen.ABS.Absyn.PLit _plit = (gen.ABS.Absyn.PLit) foo;
       render("(");
       render("PLit");
       sh(_plit.literal_);
       render(")");
    }
    if (foo instanceof gen.ABS.Absyn.PSinglConstr)
    {
       gen.ABS.Absyn.PSinglConstr _psinglconstr = (gen.ABS.Absyn.PSinglConstr) foo;
       render("(");
       render("PSinglConstr");
       sh(_psinglconstr.typeident_);
       render(")");
    }
    if (foo instanceof gen.ABS.Absyn.PParamConstr)
    {
       gen.ABS.Absyn.PParamConstr _pparamconstr = (gen.ABS.Absyn.PParamConstr) foo;
       render("(");
       render("PParamConstr");
       sh(_pparamconstr.typeident_);
       render("[");
       sh(_pparamconstr.listpattern_);
       render("]");
       render(")");
    }
    if (foo instanceof gen.ABS.Absyn.PUnderscore)
    {
       gen.ABS.Absyn.PUnderscore _punderscore = (gen.ABS.Absyn.PUnderscore) foo;
       render("PUnderscore");
    }
  }

  private static void sh(gen.ABS.Absyn.Literal foo)
  {
    if (foo instanceof gen.ABS.Absyn.LNull)
    {
       gen.ABS.Absyn.LNull _lnull = (gen.ABS.Absyn.LNull) foo;
       render("LNull");
    }
    if (foo instanceof gen.ABS.Absyn.LThis)
    {
       gen.ABS.Absyn.LThis _lthis = (gen.ABS.Absyn.LThis) foo;
       render("LThis");
    }
    if (foo instanceof gen.ABS.Absyn.LThisDC)
    {
       gen.ABS.Absyn.LThisDC _lthisdc = (gen.ABS.Absyn.LThisDC) foo;
       render("LThisDC");
    }
    if (foo instanceof gen.ABS.Absyn.LStr)
    {
       gen.ABS.Absyn.LStr _lstr = (gen.ABS.Absyn.LStr) foo;
       render("(");
       render("LStr");
       sh(_lstr.string_);
       render(")");
    }
    if (foo instanceof gen.ABS.Absyn.LInt)
    {
       gen.ABS.Absyn.LInt _lint = (gen.ABS.Absyn.LInt) foo;
       render("(");
       render("LInt");
       sh(_lint.integer_);
       render(")");
    }
  }

  private static void sh(gen.ABS.Absyn.EffExp foo)
  {
    if (foo instanceof gen.ABS.Absyn.New)
    {
       gen.ABS.Absyn.New _new = (gen.ABS.Absyn.New) foo;
       render("(");
       render("New");
       sh(_new.type_);
       render("[");
       sh(_new.listpureexp_);
       render("]");
       render(")");
    }
    if (foo instanceof gen.ABS.Absyn.NewLocal)
    {
       gen.ABS.Absyn.NewLocal _newlocal = (gen.ABS.Absyn.NewLocal) foo;
       render("(");
       render("NewLocal");
       sh(_newlocal.type_);
       render("[");
       sh(_newlocal.listpureexp_);
       render("]");
       render(")");
    }
    if (foo instanceof gen.ABS.Absyn.SyncMethCall)
    {
       gen.ABS.Absyn.SyncMethCall _syncmethcall = (gen.ABS.Absyn.SyncMethCall) foo;
       render("(");
       render("SyncMethCall");
       sh(_syncmethcall.pureexp_);
       sh(_syncmethcall.ident_);
       render("[");
       sh(_syncmethcall.listpureexp_);
       render("]");
       render(")");
    }
    if (foo instanceof gen.ABS.Absyn.ThisSyncMethCall)
    {
       gen.ABS.Absyn.ThisSyncMethCall _thissyncmethcall = (gen.ABS.Absyn.ThisSyncMethCall) foo;
       render("(");
       render("ThisSyncMethCall");
       sh(_thissyncmethcall.ident_);
       render("[");
       sh(_thissyncmethcall.listpureexp_);
       render("]");
       render(")");
    }
    if (foo instanceof gen.ABS.Absyn.AsyncMethCall)
    {
       gen.ABS.Absyn.AsyncMethCall _asyncmethcall = (gen.ABS.Absyn.AsyncMethCall) foo;
       render("(");
       render("AsyncMethCall");
       sh(_asyncmethcall.pureexp_);
       sh(_asyncmethcall.ident_);
       render("[");
       sh(_asyncmethcall.listpureexp_);
       render("]");
       render(")");
    }
    if (foo instanceof gen.ABS.Absyn.ThisAsyncMethCall)
    {
       gen.ABS.Absyn.ThisAsyncMethCall _thisasyncmethcall = (gen.ABS.Absyn.ThisAsyncMethCall) foo;
       render("(");
       render("ThisAsyncMethCall");
       sh(_thisasyncmethcall.ident_);
       render("[");
       sh(_thisasyncmethcall.listpureexp_);
       render("]");
       render(")");
    }
    if (foo instanceof gen.ABS.Absyn.Get)
    {
       gen.ABS.Absyn.Get _get = (gen.ABS.Absyn.Get) foo;
       render("(");
       render("Get");
       sh(_get.pureexp_);
       render(")");
    }
    if (foo instanceof gen.ABS.Absyn.Spawns)
    {
       gen.ABS.Absyn.Spawns _spawns = (gen.ABS.Absyn.Spawns) foo;
       render("(");
       render("Spawns");
       sh(_spawns.pureexp_);
       sh(_spawns.type_);
       render("[");
       sh(_spawns.listpureexp_);
       render("]");
       render(")");
    }
  }


  private static void pp(Integer n, int _i_) { buf_.append(n); buf_.append(" "); }
  private static void pp(Double d, int _i_) { buf_.append(d); buf_.append(" "); }
  private static void pp(String s, int _i_) { buf_.append(s); buf_.append(" "); }
  private static void pp(Character c, int _i_) { buf_.append("'" + c.toString() + "'"); buf_.append(" "); }
  private static void sh(Integer n) { render(n.toString()); }
  private static void sh(Double d) { render(d.toString()); }
  private static void sh(Character c) { render(c.toString()); }
  private static void sh(String s) { printQuoted(s); }
  private static void printQuoted(String s) { render("\"" + s + "\""); }
  private static void indent()
  {
    int n = _n_;
    while (n > 0)
    {
      buf_.append(" ");
      n--;
    }
  }
  private static void backup()
  {
     if (buf_.charAt(buf_.length() - 1) == ' ') {
      buf_.setLength(buf_.length() - 1);
    }
  }
  private static void trim()
  {
     while (buf_.length() > 0 && buf_.charAt(0) == ' ')
        buf_.deleteCharAt(0); 
    while (buf_.length() > 0 && buf_.charAt(buf_.length()-1) == ' ')
        buf_.deleteCharAt(buf_.length()-1);
  }
  private static int _n_ = 0;
  private static StringBuilder buf_ = new StringBuilder(INITIAL_BUFFER_SIZE);
}

