
package abs.backend;

import org.junit.runner.RunWith;
import org.junit.runners.Suite;

import abs.backend.java.SemanticTests;
import abs.backend.java.StmtTests;
import abs.backend.java.InterfaceTests;
import abs.backend.java.PrimitiveTests;

@RunWith(Suite.class)
@Suite.SuiteClasses( {
        PrimitiveTests.class,
        InterfaceTests.class,
        StmtTests.class,
        SemanticTests.class}
        )
public class AllTests {
}

