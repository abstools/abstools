
package abs.frontend;

import abs.frontend.analyser.InterfaceDeclarationTest;
import abs.frontend.parser.*;

import org.junit.runner.RunWith;
import org.junit.runners.Suite;

@RunWith(Suite.class)
@Suite.SuiteClasses({
		//        ScannerTest.class, 
		ParserTest.class
		, InterfaceDeclarationTest.class
        })
public class AllTests {
}

