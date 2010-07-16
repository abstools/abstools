package abs.frontend;

import static org.junit.Assert.fail;

import java.io.Reader;
import java.io.StringReader;

import abs.common.StandardLib;
import abs.frontend.analyser.SemanticError;
import abs.frontend.analyser.SemanticErrorList;
import abs.frontend.ast.AssignStmt;
import abs.frontend.ast.CaseBranch;
import abs.frontend.ast.CaseExp;
import abs.frontend.ast.Decl;
import abs.frontend.ast.Exp;
import abs.frontend.ast.ExpressionStmt;
import abs.frontend.ast.FunctionDecl;
import abs.frontend.ast.Model;
import abs.frontend.ast.ParametricFunctionDecl;
import abs.frontend.ast.Pattern;
import abs.frontend.ast.Stmt;
import abs.frontend.parser.ABSParser;
import abs.frontend.parser.ABSScanner;
import abs.frontend.parser.Main;
import abs.frontend.typechecker.Type;

public class FrontendTest {

    protected Model assertParseOkStdLib(String s) {
        return assertParseOk(StandardLib.STDLIB_STRING+s);
    }
    
    protected static void assertParseFileOk(String fileName) {
        assertParseFileOk(fileName, false);
    }

    protected static void assertTypeCheckFileOk(String fileName) {
        assertParseFileOk(fileName, true);
    }
    
    protected static void assertParseFileOk(String fileName, boolean typeCheck) {
        Model m = null;
        try {
            m = Main.parse(fileName);
        } catch (Throwable e) {
            fail("Failed to parse: "+ fileName +"\n"+e.getMessage());
            e.printStackTrace();
        }
        if (m != null) {
            int numSemErrs = m.getErrors().size();
            StringBuffer errs = new StringBuffer("Semantic errors: " + numSemErrs + "\n");
            if (numSemErrs > 0){
                for (SemanticError error : m.getErrors())
                    errs = errs.append(fileName + ":" + error.getMsgString() + "\n");  
                fail("Failed to parse: "+fileName+"\n"+errs.toString());
            } else if (typeCheck) {
                SemanticErrorList l = m.typeCheck();
                if (!l.isEmpty()) {
                    for (SemanticError error : l)
                        errs = errs.append(fileName + ":" + error.getMsgString() + "\n");  
                    fail("Failed to typecheck: "+fileName+"\n"+errs.toString());
                    
                }
            }
            
        }
    }
    
    
    protected Model assertParseOk(String s) {
        Model p = null;
        try {
            p = parse(s);
        } catch (Throwable t) {
            fail("Failed to parse: "+ s+"\n"+t.getMessage());
        }
        return p;
  }
    
    
    protected Model parse(String s) throws Throwable {
        ABSParser parser = new ABSParser();
        Reader reader = new StringReader(s);
        //      ABSScanner scanner = new ABSScanner(new BufferedReader(reader));
        ABSScanner scanner = new ABSScanner(reader);
        Model p = (Model)parser.parse(scanner);
        reader.close();
        return p;
    }
    
    protected Exp getFirstExp(String absCode) {
        Model m = assertParseOk(absCode);
        return getFirstExp(m);
    }


	protected Exp getFirstExp(Model m) {
	    Stmt s = m.getBlock().getStmt(0);
	    if (s instanceof AssignStmt)
	        return ((AssignStmt) s).getValue();
	    if (s instanceof ExpressionStmt) 
	        return ((ExpressionStmt) s).getExp();
	    throw new IllegalArgumentException();
   }


	protected Exp getFirstCaseExpr(Model m) {
        CaseExp ce = (CaseExp) getFirstFunctionExpr(m);
        CaseBranch b = ce.getBranch(0);
        return b.getRight();
    }

	protected Exp getSecondCaseExpr(Model m) {
      CaseExp ce = (CaseExp) getFirstFunctionExpr(m);
      CaseBranch b = ce.getBranch(1);
      return b.getRight();
  }
	
	protected Pattern getFirstCasePattern(Model m) {
      CaseExp ce = (CaseExp) getFirstFunctionExpr(m);
      CaseBranch b = ce.getBranch(0);
      return b.getLeft();
  }
	
	protected Decl getFirstDecl(Model m, Class<?> clazz) {
        for (Decl d : m.getDecls()) {
            if (clazz.isInstance(d)) {
                return d;
            }
        }
        throw new IllegalArgumentException("The model does not contain any "+clazz.getSimpleName());
	}
	
    protected FunctionDecl getFirstFunctionDecl(Model m) {
        return (FunctionDecl) getFirstDecl(m, FunctionDecl.class);
    }

    protected ParametricFunctionDecl getFirstParametricFunctionDecl(Model m) {
        return (ParametricFunctionDecl) getFirstDecl(m, ParametricFunctionDecl.class);
    }

	
	protected Exp getFirstFunctionExpr(Model m) {
	    return getFirstFunctionDecl(m).getFunDef();
    }


    protected Type getTypeOfFirstAssignment(Model m) {
        AssignStmt s = (AssignStmt) m.getBlock().getStmts().getChild(0);
        return s.getValue().getType();
    }
    
    
}
