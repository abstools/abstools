package abs.backend;

import org.junit.runner.RunWith;
import org.junit.runners.Suite;

import abs.backend.common.AllSemanticTests;
import abs.backend.common.FunctionalTests;
import abs.backend.common.ObjectTests;
import abs.backend.common.SemanticTests;
import abs.backend.java.AllJavaTests;
import abs.backend.java.JavaStmtTests;
import abs.backend.java.JavaPrimitiveTests;
import abs.backend.maude.MaudeTests;

@RunWith(Suite.class)
@Suite.SuiteClasses({ AllJavaTests.class, AllSemanticTests.class, MaudeTests.class })
public class AllTests {
}
