package abs;

import org.junit.runner.RunWith;
import org.junit.runners.Suite;

@RunWith(Suite.class)
@Suite.SuiteClasses({
// ScannerTest.class,
        abs.backend.AllBackendTests.class, abs.frontend.AllFrontendTests.class })
public class AllTests {

}
