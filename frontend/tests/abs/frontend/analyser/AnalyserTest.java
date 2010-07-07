package abs.frontend.analyser;

import static org.junit.Assert.fail;

import java.io.Reader;
import java.io.StringReader;

import abs.frontend.ast.AssignStmt;
import abs.frontend.ast.Exp;
import abs.frontend.ast.Model;
import abs.frontend.ast.Type;
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
        AssignStmt s = (AssignStmt) m.getBlock().getStmts().getChild(0);
        return s.getValue();
    }
    
    
}
