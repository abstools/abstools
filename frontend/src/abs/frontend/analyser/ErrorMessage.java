package abs.frontend.analyser;

public enum ErrorMessage {
      CYCLIC_INHERITANCE("Cyclic inheritance chain for interface: %s")
    , UNKOWN_INTERFACE("Unknown interface: %s")
    , UNKOWN_DATATYPE("Unknown datatype: %s")
    , UNKOWN_DATACONSTRUCTOR("Unknown datatype constructor: %s")
    , UNKOWN_INTERFACE_OR_DATATYPE("Unknown interface or datatype: %s")
    , DUPLICATE_TYPE_DECL("Duplicate type declaration: %s")
    , EXPECTED_TYPE("Expected type %s but found type %s instead")
    , NO_SUBTYPE("Type %s must be a subtype of type %s")
    , VAR_INIT_REQUIRED("A variable must be initialized if it is not of a reference type")
    ;

        
    private String pattern;

    ErrorMessage(String pattern) {
        this.pattern = pattern;
    }
    
    public String withArgs(String... args) {
        return String.format(pattern, (Object[]) args);
    }
}
