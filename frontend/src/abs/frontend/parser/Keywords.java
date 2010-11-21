package abs.frontend.parser;

/**
 * This class is currently used by the Eclipse-Plugin as we have not found yet a
 * better way to get a list of all keywords. The list must be hold consistent
 * with the list defined in the ABS.flex file manually
 * 
 */
public class Keywords {

    // This list has to be consistent with keywords in ABS.flex
    private static String[] keywords = { "module", "import", "export", "from", "class", "interface", "extends", "data",
            "def", "implements", "delta", "adds", "modifies", "removes", "productline", "features", "core", "after",
            "when", "product", "while", "return", "skip", "get", "null", "await", "if", "else", "suspend", "new",
            "this", "case", "let", "in", "cog", "type", "assert", "builtin" };

    public static String[] getKeywords() {
        return keywords;
    }
}
