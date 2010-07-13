package abs.frontend.analyser;

public enum ErrorMessage {
      CYCLIC_INHERITANCE("Cyclic inheritance chain for interface: %s.")
    , UNKOWN_INTERFACE("Unknown interface: %s.")
    , UNKOWN_DATATYPE("Unknown datatype: %s.")
    , UNKOWN_DATACONSTRUCTOR("Unknown datatype constructor: %s.")
    , UNKOWN_INTERFACE_OR_DATATYPE("Unknown interface or datatype: %s.")
    , DUPLICATE_TYPE_DECL("Duplicate type declaration: %s.")
    , EXPECTED_TYPE("Expected type %s, but found type %s instead.")
    , NO_SUBTYPE("Type %s must be a subtype of type %s.")
    , CANNOT_ASSIGN("Cannot assign %s to type %s.")
    , VAR_INIT_REQUIRED("A variable must be initialized if it is not of a reference type.")
    , EXPECTED_FUT_TYPE("Expected a future type, but found type %s instead.")
    , TYPE_NOT_RESOLVABLE("Type %s cannot be resolved.")
    , CONSTRUCTOR_NOT_RESOLVABLE("Constructor %s cannot be resolved.")
    , BRANCH_NO_SAME_TYPE("Case branches with different types. Expected %s, but found %s.")
    , CASE_NO_DATATYPE("Cases are only possible on data types, but found type %s.")
    , WRONG_NUMBER_OF_ARGS("Wrong number of arguments. Expected %s, but found %s.")
    , PATTERN_WRONG_TYPE("Pattern type %s does not match declared type %s.")
    , DUPLICATE_CONSTRUCTOR("Constructor %s is already defined.")
    ;

        
    private String pattern;

    ErrorMessage(String pattern) {
        this.pattern = pattern;
    }
    
    public String withArgs(String... args) {
        return String.format(pattern, (Object[]) args);
    }
}
