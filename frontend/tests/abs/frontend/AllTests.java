
package abs.frontend;

import abs.frontend.parser.*;

import org.junit.runner.RunWith;
import org.junit.runners.Suite;

@RunWith(Suite.class)
@Suite.SuiteClasses({
        ScannerTest.class, 
		ParserTest.class
        })
public class AllTests {
}

