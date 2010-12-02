package abs.frontend.typechecker;

import abs.frontend.ast.Annotation;
import abs.frontend.ast.PureExp;

public class TypeAnnotation {
    private Type type;
    private PureExp exp;
    
    public TypeAnnotation(Annotation a) {
        exp = a.getValue();
        type = a.getType();
    }

}
