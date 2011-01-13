package abs.backend.common;

import java.util.Arrays;
import java.util.Collection;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;
import org.junit.runners.Parameterized.Parameters;

import abs.backend.BackendTestDriver;
import abs.backend.java.JavaTestDriver;
import abs.backend.maude.MaudeTestDriver;

@RunWith(Parameterized.class)
public class SemanticTests {
    private BackendTestDriver driver;

    public SemanticTests(BackendTestDriver d) {
        driver = d;
    }

    @Parameters
    public static Collection<?> data() {
        Object[][] data = new Object[][] { { new JavaTestDriver() }, { new JavaTestDriver(1) } , { new MaudeTestDriver() } };
        return Arrays.asList(data);
    }

    public void assertEvalTrue(String absCode) {
        driver.assertEvalTrue("module BackendTest; " + absCode);
    }

    public void assertEvalFails(String absCode) {
        driver.assertEvalFails("module BackendTest; " + absCode);
    }

}
