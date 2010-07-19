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
    , FIELD_INIT_REQUIRED("A field must be initialized if it is not of a reference type.")
    , EXPECTED_FUT_TYPE("Expected a future type, but found type %s instead.")
    , EXPECTED_ADDABLE_TYPE("Expected numeric or string type for operator '+', but found type %s instead.")
    , TYPE_NOT_RESOLVABLE("Type %s cannot be resolved.")
    , CONSTRUCTOR_NOT_RESOLVABLE("Constructor %s cannot be resolved.")
    , FUNCTION_NOT_RESOLVABLE("Function %s cannot be resolved.")
    , BRANCH_INCOMPARABLE_TYPE("Case branches with incomparable types, %s and %s.")
    , CASE_NO_DATATYPE("Cases are only possible on data types, but found type %s.")
    , EQUALITY_INCOMPARABLE_TYPE("Equality expression with incomparable types, %s and %s.")
    , WRONG_NUMBER_OF_ARGS("Wrong number of arguments. Expected %s, but found %s.")
    , TYPE_MISMATCH("Type %s does not match declared type %s.")
    , DUPLICATE_CONSTRUCTOR("Constructor %s is already defined.")
    , DUPLICATE_METHOD_NAME("Method %s is already defined.")
    , DUPLICATE_PARAM_NAME("Parameter %s is already defined.")
    , DUPLICATE_FIELD_NAME("Field %s is already defined.")
    , ONLY_INTERFACE_EXTEND("Interfaces can only extend other interfaces, but %s is not an interface.")
    , NO_METHOD_OVERRIDE("Method %s overrides an existing method of interface %s.")
    , NO_METHOD_IMPL("Method %s does not exist in any implemented interface.")
    , METHOD_IMPL_WRONG_NUM_PARAMS("Method %s does not have the same number of parameters as defined in interface %s. Expected %s, but found %s.")
    , METHOD_IMPL_WRONG_PARAM_TYPE("Parameter %s of method %s has a different type as defined in interface %s. Expected %s, but found %s.")
    , TARGET_NO_INTERFACE_TYPE("Target expression is not typable to an interface.")
    , METHOD_NOT_FOUND("Method %s could not be found")
    , NO_CLASS_DECL("Class %s could not be found")
    ;

        
    private String pattern;

    ErrorMessage(String pattern) {
        this.pattern = pattern;
    }
    
    public String withArgs(String... args) {
        return String.format(pattern, (Object[]) args);
    }
}
