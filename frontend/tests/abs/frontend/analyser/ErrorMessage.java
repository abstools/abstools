package abs.frontend.analyser;

public enum ErrorMessage {
    CYCLIC_INHERITANCE("Cyclic inheritance chain for interface: %s"),
    UNKOWN_INTERFACE("Unknown interface: %s");
    
    private String pattern;

    ErrorMessage(String pattern) {
        this.pattern = pattern;
    }
    
    public String withArgs(String... args) {
        return String.format(pattern, (Object[]) args);
    }
}
