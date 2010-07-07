package abs.frontend.analyser;

import static org.junit.Assert.fail;

import java.io.Reader;
import java.io.StringReader;

import abs.frontend.ast.AssignStmt;
import abs.frontend.ast.CaseBranch;
import abs.frontend.ast.CaseExp;
import abs.frontend.ast.Exp;
import abs.frontend.ast.FunctionDecl;
import abs.frontend.ast.Model;
import abs.frontend.parser.ABSParser;
import abs.frontend.parser.ABSScanner;

public class AnalyserTest {

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
	   AssignStmt s = (AssignStmt) m.getBlock().getStmts().getChild(0);
        return s.getValue();
   }


	protected Exp getFirstCaseExpr(Model m) {
        CaseExp ce = (CaseExp) getFirstFunctionExpr(m);
        CaseBranch b = ce.getBranch(1);
        return b.getRight();
    }


	protected Exp getFirstFunctionExpr(Model m) {
        FunctionDecl f = (FunctionDecl) m.getDecls().getChild(1);
        return f.getFunDef();
    }
    
    
}
