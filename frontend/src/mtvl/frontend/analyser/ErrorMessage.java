package mtvl.frontend.analyser;

public enum ErrorMessage {
      UNDECLARED_VARIABLE("Unknown variable: %s.")
    , EXPECTED_BOOL("Inferred type Int for %s, expected Bool.")
    , EXPECTED_INT("Inferred type Bool for %s, expected Int.")
    , VAR_INIT_REQUIRED("A variable must be initialized.")
    , NAME_NOT_RESOLVABLE("Name %s cannot be resolved.")
    , EQUALITY_INCOMPARABLE_TYPE("Equality expression with incomparable types, %s and %s.")
    , DUPLICATE_FEATURE("Feature %s is already defined.")
    , DUPLICATE_VARIABLE("Variable %s is already defined.")
    ;

        
    private String pattern;

    ErrorMessage(String pattern) {
        this.pattern = pattern;
    }
    
    public String withArgs(String... args) {
        return String.format(pattern, (Object[]) args);
    }
}
