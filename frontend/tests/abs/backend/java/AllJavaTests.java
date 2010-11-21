package abs.backend.java;

import org.junit.runner.RunWith;
import org.junit.runners.Suite;

import abs.backend.common.AllSemanticTests;
import abs.backend.maude.MaudeTests;

@RunWith(Suite.class)
@Suite.SuiteClasses({ JavaPrimitiveTests.class, JavaStmtTests.class, JavaExprTests.class, JavaExamplesTests.class,
        JavaObservationTest.class })
public class AllJavaTests {

}
