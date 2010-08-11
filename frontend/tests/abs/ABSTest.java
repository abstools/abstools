package abs;

import static org.junit.Assert.fail;

import java.io.ByteArrayInputStream;
import java.io.InputStream;
import java.io.Reader;
import java.io.StringReader;

import abs.frontend.ast.Model;
import abs.frontend.parser.ABSParser;
import abs.frontend.parser.ABSScanner;

public class ABSTest {

    protected Model parse(String s) throws Throwable {
        ABSParser parser = new ABSParser();
        Reader reader = new StringReader(s);
        //      ABSScanner scanner = new ABSScanner(new BufferedReader(reader));
        ABSScanner scanner = new ABSScanner(reader);
        Model p = (Model)parser.parse(scanner);
        reader.close();
        return p;
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
    

    protected InputStream getInputStream(String absCode) {
        return new ByteArrayInputStream(absCode.getBytes());
    }
    
    
    
}
