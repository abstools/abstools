package abs.common;

public class StandardLib {
    public static final String STDLIB_DATATYPES_STRING = 
        "data Bool = True | False ;\n" +
        "data Unit = Unit ; \n" +
        "data Int ;\n" +
        "data String ;\n" +
        "data Fut<A> ;\n" +
        "data Opt<A> = None | Some(A); \n" +
  		  "data Either<A,B> = Left(A) | Right(B) ; \n"+
  		  ""
        ; 
    
    public static final String STDLIB_FUNCTIONS_STRING = 
   	 "def Bool isSome<A>(Opt<A> val) = " +
       " case val {"+
       " 	None => False ;"+
       "   Some(x) => True ;"+
 	     " } ; "+
  	     
 	     "def A fromSome<A>(Opt<A> val) ="+
 		  "  case val {"+
 		  "     Some(x) => x ;"+
 	     "  } ; \n"+
 	     
 	     "def A getOptValue<A>(Opt<A> val) = fromSome(val);" +
 	     
  		  "def A left<A,B>(Either<A,B> e) = "+
  		  "  case e {"+
        "    Left(x) => x;"+
        "  } ; \n"+

  		  "def B right<A,B>(Either<A,B> e) = "+
        "  case e {"+
        "      Right(x) => x;"+
        "  } ; \n"+
 	     "";
}
