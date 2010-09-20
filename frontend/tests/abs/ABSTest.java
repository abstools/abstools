package abs;

import static org.junit.Assert.fail;

import java.io.File;

import abs.frontend.analyser.SemanticError;
import abs.frontend.analyser.SemanticErrorList;
import abs.frontend.ast.Model;
import abs.frontend.parser.Main;

public class ABSTest {

    protected Model assertParseOk(String s, boolean withStdLib) {
    	return assertParseOk(s, withStdLib, true);
    }

    	protected Model assertParseOk(String s, boolean withStdLib, boolean addModuleName) {
    	String preamble = "module UnitTest; export *; ";
    	if (withStdLib)
    		preamble = preamble+" import * from ABS.StdLib;";
    	if (addModuleName)
    		s = preamble+s;
        Model p = null;
        try {
            p = Main.parseString(s, withStdLib);
        } catch (Exception t) {
        	System.out.println(t.getClass().getName());
            t.printStackTrace();
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
            m = Main.parse(new File(fileName), withStdLib);
        } catch (Throwable e) {
            e.printStackTrace();
            fail("Failed to parse: "+ fileName +"\n"+e.getMessage());
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
