package abs.frontend.typechecker;

import abs.frontend.analyser.ErrorMessage;
import abs.frontend.analyser.TypeError;
import abs.frontend.ast.ASTNode;
import abs.frontend.ast.Annotation;
import abs.frontend.ast.Call;
import abs.frontend.ast.ClassDecl;
import abs.frontend.ast.DataConstructorExp;
import abs.frontend.ast.List;
import abs.frontend.ast.Model;
import abs.frontend.ast.NewExp;
import abs.frontend.typechecker.KindedName.Kind;
import abs.frontend.typechecker.ext.DefaultTypeSystemExtension;

public class ClassKindTypeExtension extends DefaultTypeSystemExtension {
    
    public ClassKindTypeExtension(Model m) {
        super(m);
    }

    @Override
    public void checkNewExp(NewExp e) {
        ClassDecl d = (ClassDecl) e.lookup(new KindedName(Kind.CLASS,e.getClassName()));
        List<Annotation> anns = d.getAnnotations();
        String found = null;
        for (Annotation a : anns) {
            if (a.getType().getQualifiedName().equals("ABS.StdLib.ClassKindAnnotation")) {
                DataConstructorExp de = (DataConstructorExp) a.getValue();
                found = de.getDecl().getName();
            }
        }
            
        if (found != null) {
            if (e.hasCog()) {
                if (!found.equals("COG")) {
                    errors.add(new TypeError(e,ErrorMessage.CLASSKIND_PLAIN,d.getName()));
                }
            } else {
                if (found.equals("COG")) {
                    errors.add(new TypeError(e,ErrorMessage.CLASSKIND_COG,d.getName()));
                }
            }
        }
    }

}
