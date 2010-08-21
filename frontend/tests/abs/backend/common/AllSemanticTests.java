package abs.backend.common;

import org.junit.runner.RunWith;
import org.junit.runners.Suite;

import abs.backend.java.JavaPrimitiveTests;
import abs.backend.java.JavaStmtTests;

@RunWith(Suite.class)
@Suite.SuiteClasses( {
        JavaPrimitiveTests.class,
        JavaStmtTests.class,
        ObjectTests.class,
        FunctionalTests.class}
        )
public class AllSemanticTests {

}
