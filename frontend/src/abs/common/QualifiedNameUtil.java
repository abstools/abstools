package abs.common;

import abs.frontend.ast.List;
import abs.frontend.ast.Name;
import abs.frontend.ast.QualifiedName;

public class QualifiedNameUtil {
    public static final QualifiedName create(String... args) {
        List<Name> names = new List<Name>();
        for (String s : args) {
            names = names.add(new Name(s));
        }
        return new QualifiedName(names);
    }
}
