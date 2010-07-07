package abs;

import org.junit.runner.RunWith;
import org.junit.runners.Suite;

@RunWith(Suite.class)
@Suite.SuiteClasses({
        //        ScannerTest.class, 
        abs.backend.AllTests.class
        , abs.frontend.AllTests.class
        })
public class AllTests {

}
