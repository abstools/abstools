package abs.frontend.pardef;

import org.junit.runner.RunWith;
import org.junit.runners.Suite;
import org.junit.runners.Suite.SuiteClasses;

@RunWith(Suite.class)
@SuiteClasses({
    PartialFunctionTest.class,
    ParametricPartialFunctionTest.class,
    ParFnAppTest.class
})
public class PardefTests {

}
