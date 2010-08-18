package abs;

import static org.junit.Assert.fail;
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
    

}
