
package abs.backend;

import org.junit.runner.RunWith;
import org.junit.runners.Suite;

import abs.backend.java.JavaBackendTest;

@RunWith(Suite.class)
@Suite.SuiteClasses({
		JavaBackendTest.class
        })
public class AllTests {
}

