
package abs.frontend;

import abs.frontend.analyser.InterfaceDeclarationTest;
import abs.frontend.analyser.VarResolutionTest;
import abs.frontend.parser.*;
import abs.frontend.typesystem.TypingTest;

import org.junit.runner.RunWith;
import org.junit.runners.Suite;

@RunWith(Suite.class)
@Suite.SuiteClasses({
		//        ScannerTest.class, 
		ParserTest.class
		, InterfaceDeclarationTest.class
		, ParseSamplesTest.class
		, VarResolutionTest.class
		, TypingTest.class
		
        })
public class AllTests {
	// TODO: document this
}

