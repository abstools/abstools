package org.abs_models.backend.scala;

import java.util.HashMap;
import java.util.Map;
import java.util.function.Function;


/**
 * Some ABS types have an equivalent proper Java type. This is function to be
 * able to translate those.
 */
public class ScalaTypeTranslator implements Function<String, String> {

    private final Map<String, String> absTypes = new HashMap<>();
    public final Map<String, String> abstractTypes = new HashMap<>();
    private final Map<String, String> functionalTypes = new HashMap<>();
    private final Map<String, String> staticTypes = new HashMap<>();
    private final Map<String, String> remoteNames = new HashMap<>();
    private final String[] keywords = new String[] { "abstract", "case", "catch", "class", "def", "do", "else",
            "extends", "false", "final", "finally", "for", "forSome", "if", "implicit", "import", "lazy","length", "match",
            "new", "Null", "object", " override", "package", " private", "protected", "return", "sealed", "super",
            "this", "throw", "trait", "Try", "true", "type", "val", "Var", "while", "with", "yield", "wait", "Rat", "Bool", "Int", "Fut", "True", "False", "Unit", "Float",
            "String", "Exception", "numerator", "denominator", "truncate", "random", "substr", "log","exp","sqrt", "float", "rat", "floor", "ceil", "strlen", "toString",
            "print", "println", "readln", "currentms", "watch", "watchEx", "lowlevelDeadline" };
    private final Map<String, String> kwNames = new HashMap<>();
    
    public ScalaTypeTranslator() {
        fillABSTypes(absTypes);
        fillFunctionalTypes(functionalTypes);
        fillKeywords();
    }

    @Override
    public String apply(String absType) {
        String javaType = translateJava(absType);
        if (javaType != null) {
            // System.out.println(String.format("1> %s => %s", absType,
            // javaType));
            return javaType;
        }
        String type = translateAbstract(absType);
        if (type == null) {
            // System.out.println(String.format("2> %s => %s", absType, type));
            return absType;
        }
        javaType = translateJava(type);
        // System.out.println(String.format("3> %s => %s => %s", absType, type,
        // javaType));
        return javaType == null ? type : javaType;
    }

    protected String getRemoteName(String variable) {
        return remoteNames.get(variable);
    }

    protected String translateFunctionalType(String type) {
        if (this.functionalTypes.containsKey(type)) {
            return this.functionalTypes.get(type);
        }
        return type;
    }

    protected String translateStaticType(String type) {
        return staticTypes.get(type);
    }
    
    protected String translateKeyword(String type) {
        if (this.kwNames.containsKey(type)) {
            return this.kwNames.get(type);
        }
        return type;
    }

    private String translateJava(String absType) {
        
        return absTypes.get(absType);
    }

    private String translateAbstract(String absType) {
        return abstractTypes.get(absType);
    }

    protected void fillABSTypes(final Map<String, String> types) {
        types.put("Rat", "Rational");
        types.put("ABS.StdLib.Rat", "Rational");
        types.put("ABS.StdLib.String", "String");
        types.put("Bool", "Boolean");
        types.put("ABS.StdLib.Bool", "Boolean");
        types.put("ABS.StdLib.Int", "Int");
        types.put("ABS.StdLib.Fut", ScalaVisitor.ABSFUTURE_CLASS);
        types.put("Fut", ScalaVisitor.ABSFUTURE_CLASS);
        types.put("Unit", "Void");
        types.put("ABS.StdLib.Unit", "Void");
    }

    protected void fillFunctionalTypes(final Map<String, String> types) {
        functionalTypes.put("True", "true");
        functionalTypes.put("False", "false");

    }
    protected void fillKeywords() {
        for (String string : keywords) {
            kwNames.put(string, "v"+string);
        }
    }
        
    

    protected void registerAbstractType(String absType, String defType) {
        this.abstractTypes.put(absType, defType);
        // System.out.println(this.abstractTypes);
    }

    protected void deRegisterAbstractType(String absType) {
        this.abstractTypes.remove(absType);
        // System.out.println(this.abstractTypes);
    }

    protected void registerStaticType(String stType, String defType) {
        this.staticTypes.put(stType, defType);
    }

    protected boolean inStaticTypes(String stType) {
        return this.staticTypes.containsKey(stType);
    }

    protected void registerRemoteName(String stName, String remoteName) {
        this.remoteNames.put(stName, remoteName);
    }

}
