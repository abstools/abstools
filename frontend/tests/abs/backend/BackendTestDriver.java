package abs.backend;

public abstract class BackendTestDriver {

    public abstract void assertEvalEquals(String absCode, boolean value);

    public abstract void assertEvalFails(String absCode);

    public void assertEvalTrue(String absCode) {
        assertEvalEquals(absCode, true);
    }
}
