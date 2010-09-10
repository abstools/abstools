package abs.common;

import abs.frontend.ast.List;
import abs.frontend.ast.Name;
import abs.frontend.ast.QualifiedName;
import abs.frontend.ast.SimpleName;

public class QualifiedNameUtil {
    public static final QualifiedName create(String... args) {
        List<SimpleName> names = new List<SimpleName>();
        for (String s : args) {
            names = names.add(new SimpleName(s));
        }
        return new QualifiedName(names);
    }
}
