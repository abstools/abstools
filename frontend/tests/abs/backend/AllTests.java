
package abs.backend;

import org.junit.runner.RunWith;
import org.junit.runners.Suite;

import abs.backend.java.ExpressionTests;
import abs.backend.java.InterfaceTests;
import abs.backend.java.PrimitiveTests;

@RunWith(Suite.class)
@Suite.SuiteClasses( {
        PrimitiveTests.class,
        InterfaceTests.class,
        ExpressionTests.class }
        )
public class AllTests {
}

