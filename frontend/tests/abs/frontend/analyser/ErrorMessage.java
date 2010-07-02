package abs.frontend.analyser;

public enum ErrorMessage {
    CYCLIC_INHERITANCE("Cyclic inheritance chain for interface: %s"),
    UNKOWN_INTERFACE("Unknown interface: %s"),
    UNKOWN_DATATYPE("Unknown datatype: %s"),
    UNKOWN_INTERFACE_OR_DATATYPE("Unknown interface or datatype: %s"),
    DUPLICATE_TYPE_DECL("Duplicate type declaration: %s");

        
    private String pattern;

    ErrorMessage(String pattern) {
        this.pattern = pattern;
    }
    
    public String withArgs(String... args) {
        return String.format(pattern, (Object[]) args);
    }
}
