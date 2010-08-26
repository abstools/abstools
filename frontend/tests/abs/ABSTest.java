package abs;

import static org.junit.Assert.fail;
import abs.frontend.analyser.SemanticError;
import abs.frontend.analyser.SemanticErrorList;
import abs.frontend.ast.Model;
import abs.frontend.parser.Main;

public class ABSTest {

    protected Model assertParseOk(String s, boolean withStdLib) {
        Model p = null;
        try {
            p = Main.parseString(s, withStdLib);
        } catch (Throwable t) {
            fail("Failed to parse: "+ s+"\n"+t.getMessage());
        }
        return p;
    }
    
    protected Model assertParseOk(String s) {
        return assertParseOk(s, false);
  }

    protected Model assertParseFileOk(String fileName, boolean typeCheck, boolean withStdLib) {
        Model m = null;
        try {
            m = Main.parse(fileName, withStdLib);
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
        return m;
    }
    

}
